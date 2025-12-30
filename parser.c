/* c/owl-lang/parser.c
 *
 * Minimal recursive-descent parser for Owl that:
 * - Retains the full token stream (including trivia) from the lexer
 * - Builds an AST arena with token index coverage on each node
 * - Provides basic error recovery to continue parsing after errors
 *
 * Notes:
 * - This is an initial, pragmatic implementation. It parses core language
 *   constructs (tuples, functions, blocks, var/assign, return, if, loop,
 *   and expressions with precedence, calls/indexing/arrays/groups).
 * - It does not perform semantic checks (e.g., symbol resolution).
 * - Tuple construction vs function call:
 *     If we see named arguments `field = expr` inside parens and the callee
 *     is a bare identifier, we emit OWL_AST_EXPR_TUPLE_CTOR. Otherwise, we
 *     emit OWL_AST_EXPR_CALL.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "parser.h"
#include "lexer.h"

/* ===========================
   Small utility macros
   =========================== */

#define OWL_MAX(a,b) ((a) > (b) ? (a) : (b))
#define OWL_MIN(a,b) ((a) < (b) ? (a) : (b))

#define OWL_MALLOC(T, n)   ((T*)malloc(sizeof(T) * (size_t)(n)))
#define OWL_CALLOC(T, n)   ((T*)calloc((size_t)(n), sizeof(T)))
#define OWL_REALLOC(T, p, n) ((T*)realloc((p), sizeof(T) * (size_t)(n)))

/* ===========================
   Dynamic slice helpers
   =========================== */

static void tokidx_slice_push(OwlTokIdxSlice* s, OwlTokIdx v) {
    if (s->len + 1 > s->cap) {
        size_t new_cap = s->cap ? s->cap * 2 : 4;
        s->items = OWL_REALLOC(OwlTokIdx, s->items, new_cap);
        s->cap = new_cap;
    }
    s->items[s->len++] = v;
}

static void astid_slice_push(OwlAstIdSlice* s, OwlAstId v) {
    if (s->len + 1 > s->cap) {
        size_t new_cap = s->cap ? s->cap * 2 : 4;
        s->items = OWL_REALLOC(OwlAstId, s->items, new_cap);
        s->cap = new_cap;
    }
    s->items[s->len++] = v;
}

/* ===========================
   AST arena helpers
   =========================== */

static OwlAstId ast_push(OwlAst* ast, const OwlAstNode* node) {
    if (ast->len + 1 > ast->cap) {
        size_t new_cap = ast->cap ? ast->cap * 2 : 16;
        ast->nodes = OWL_REALLOC(OwlAstNode, ast->nodes, new_cap);
        ast->cap = new_cap;
    }
    ast->nodes[ast->len] = *node;
    ast->len += 1;
    return (OwlAstId)ast->len; /* 1-based id */
}

static void ast_init(OwlAst* ast) {
    ast->nodes = NULL;
    ast->len = 0;
    ast->cap = 0;
}

/* ===========================
   Error handling
   =========================== */

typedef struct {
    OwlParseError* items;
    size_t len;
    size_t cap;
} ErrorVec;

static void errors_push(ErrorVec* ev, OwlParseErrorCode code, OwlSpan span, OwlTokIdx at_tok) {
    if (ev->len + 1 > ev->cap) {
        size_t new_cap = ev->cap ? ev->cap * 2 : 8;
        ev->items = OWL_REALLOC(OwlParseError, ev->items, new_cap);
        ev->cap = new_cap;
    }
    OwlParseError err;
    err.code = code;
    err.span = span;
    err.at_token = at_tok;
    ev->items[ev->len++] = err;
}

/* ===========================
   Parser core
   =========================== */

typedef struct {
    /* Source kept for returning in result */
    const char* source;
    size_t source_len;

    /* Token buffer (including trivia) */
    OwlToken* tokens;
    size_t token_count;

    /* Cursor over token buffer (index into tokens). We advance across all tokens
       but typically skip trivia for syntactic decisions. */
    OwlTokIdx cur;

    /* Options & outputs */
    OwlParserOptions opts;
    OwlAst ast;
    ErrorVec errors;
} Parser;

/* Token helpers */

static inline int is_eof_tok(const OwlToken* t) { return t->kind == OWL_TOK_EOF; }

static inline int is_trivia_kind(OwlTokenKind k) {
    return owl_token_is_trivia(k);
}

static inline const OwlToken* tok(const Parser* p, OwlTokIdx idx) {
    if (idx >= p->token_count) return NULL;
    return &p->tokens[idx];
}

static inline const OwlToken* cur_tok(const Parser* p) {
    return tok(p, p->cur);
}

/* Advance one token (even if trivia); returns old index */
static inline OwlTokIdx advance_any(Parser* p) {
    OwlTokIdx prev = p->cur;
    if (p->cur < p->token_count) p->cur++;
    return prev;
}

/* Skip trivia tokens (whitespace/newline/comments) */
static void skip_trivia(Parser* p) {
    while (p->cur < p->token_count) {
        OwlTokenKind k = p->tokens[p->cur].kind;
        if (!is_trivia_kind(k)) break;
        p->cur++;
    }
}

/* Peek next significant token index (skipping trivia). Returns token_count (past end) if none. */
static OwlTokIdx peek_sig_idx(Parser* p) {
    OwlTokIdx i = p->cur;
    while (i < p->token_count && is_trivia_kind(p->tokens[i].kind)) {
        i++;
    }
    return i;
}

/* Match a specific token kind at the next significant position, consuming it if present. */
static int match_sig(Parser* p, OwlTokenKind kind, OwlTokIdx* out_idx) {
    OwlTokIdx i = peek_sig_idx(p);
    if (i < p->token_count && p->tokens[i].kind == kind) {
        p->cur = i + 1;
        if (out_idx) *out_idx = i;
        return 1;
    }
    return 0;
}

/* Expect a specific token kind at the next significant position; on failure, record error. */
static int expect_sig(Parser* p, OwlTokenKind kind, OwlTokIdx* out_idx, OwlParseErrorCode err_code) {
    OwlTokIdx i = peek_sig_idx(p);
    if (i < p->token_count && p->tokens[i].kind == kind) {
        p->cur = i + 1;
        if (out_idx) *out_idx = i;
        return 1;
    }
    /* Error */
    const OwlToken* t = tok(p, i < p->token_count ? i : (p->token_count - 1));
    OwlSpan sp = t ? t->span : (OwlSpan){0};
    errors_push(&p->errors, err_code, sp, i);
    return 0;
}

/* Look ahead at next N significant tokens; returns their indexes via array (len n).
   If out[i] == token_count, that lookahead exhausted stream. */
static void lookahead_sig(Parser* p, int n, OwlTokIdx* out) {
    OwlTokIdx i = p->cur;
    int k = 0;
    while (k < n) {
        while (i < p->token_count && is_trivia_kind(p->tokens[i].kind)) i++;
        out[k++] = i;
        if (i < p->token_count) i++;
    }
}

/* Synchronize to end of statement or block on error:
   Advance until we find ';' or '}' or EOF; consume ';' if present. */
static void sync_after_error(Parser* p) {
    for (;;) {
        OwlTokIdx i = p->cur;
        if (i >= p->token_count) break;
        OwlTokenKind k = p->tokens[i].kind;
        if (k == OWL_TOK_SEMICOLON) { p->cur = i + 1; break; }
        if (k == OWL_TOK_RBRACE || k == OWL_TOK_EOF) { /* don't consume '}' here */ break; }
        p->cur++;
    }
}

/* AST node initialization helpers */

static OwlAstNode make_node(OwlAstNodeKind kind) {
    OwlAstNode n;
    memset(&n, 0, sizeof(n));
    n.kind = kind;
    return n;
}

/* Span helpers using token indexes */
static OwlSpan span_from_tokens(const Parser* p, OwlTokIdx first, OwlTokIdx last_excl) {
    OwlSpan s;
    if (first < p->token_count) s.start = p->tokens[first].span.start;
    else memset(&s.start, 0, sizeof(s.start));
    if (last_excl > 0 && last_excl - 1 < p->token_count) s.end = p->tokens[last_excl - 1].span.end;
    else memset(&s.end, 0, sizeof(s.end));
    return s;
}

/* ===========================
   Forward decls for parsing
   =========================== */

static OwlAstId parse_program(Parser* p);
static OwlAstId parse_top_level(Parser* p, OwlTokIdx* out_first_tok, OwlTokIdx* out_last_tok_excl, OwlTopLevelKind* out_kind);

/* Decls and types */
static OwlAstId parse_tuple_decl(Parser* p, OwlTokIdx first_tok);
static OwlAstId parse_func_decl(Parser* p, OwlTokIdx name_tok);
static OwlAstId parse_type(Parser* p);
static int      peek_is_type_keyword(OwlTokenKind k);

/* Statements and expressions */
static OwlAstId parse_block(Parser* p);
static OwlAstId parse_stmt(Parser* p);
static OwlAstId parse_stmt_if(Parser* p, OwlTokIdx if_tok);
static OwlAstId parse_stmt_loop(Parser* p, OwlTokIdx loop_tok);
static OwlAstId parse_stmt_return(Parser* p, OwlTokIdx ret_tok);
static OwlAstId parse_stmt_var_or_assign(Parser* p, OwlTokIdx name_tok);

static OwlAstId parse_expr(Parser* p);
static OwlAstId parse_expr_bp(Parser* p, int min_prec);
static OwlAstId parse_unary(Parser* p);
static OwlAstId parse_postfix(Parser* p, OwlAstId primary);
static OwlAstId parse_primary(Parser* p);
static int      get_binary_prec(OwlTokenKind k);
static int      is_binary_op(OwlTokenKind k);
static int      is_unary_op(OwlTokenKind k);

/* Helpers for named tuple ctor */
static int      next_paren_has_named_args(Parser* p);

/* ===========================
   Parse implementations
   =========================== */

static OwlAstId parse_program(Parser* p) {
    OwlAstNode prog = make_node(OWL_AST_PROGRAM);
    prog.first_tok = p->cur;

    /* dynamic array of top-level items */
    OwlTopLevelItem* items = NULL;
    size_t len = 0, cap = 0;

    for (;;) {
        skip_trivia(p);
        OwlTokIdx i = p->cur;
        if (i >= p->token_count) break;
        if (p->tokens[i].kind == OWL_TOK_EOF) { p->cur = i + 1; break; }

        OwlTokIdx first, last_excl;
        OwlTopLevelKind tkind = OWL_TOP_STMT;
        OwlAstId id = parse_top_level(p, &first, &last_excl, &tkind);
        if (id == OWL_AST_ID_NONE) {
            if (!p->opts.continue_after_error) break;
            sync_after_error(p);
            continue;
        }

        if (len + 1 > cap) {
            size_t new_cap = cap ? cap * 2 : 8;
            items = OWL_REALLOC(OwlTopLevelItem, items, new_cap);
            cap = new_cap;
        }
        items[len].kind = tkind;
        items[len].node = id;
        len++;

        (void)first; (void)last_excl;
    }

    prog.last_tok_excl = p->cur;
    prog.span = span_from_tokens(p, prog.first_tok, prog.last_tok_excl);

    /* attach items */
    prog.as.program.items = items;
    prog.as.program.len = len;
    prog.as.program.cap = cap;

    return ast_push(&p->ast, &prog);
}

static OwlAstId parse_top_level(Parser* p, OwlTokIdx* out_first_tok, OwlTokIdx* out_last_tok_excl, OwlTopLevelKind* out_kind) {
    skip_trivia(p);
    OwlTokIdx first = p->cur;

    /* tuple decl? */
    OwlTokIdx i = peek_sig_idx(p);
    if (i < p->token_count && p->tokens[i].kind == OWL_TOK_KW_TUPLE) {
        p->cur = i + 1; /* consume 'tuple' */
        OwlAstId id = parse_tuple_decl(p, i);
        if (id != OWL_AST_ID_NONE) {
            if (out_kind) *out_kind = OWL_TOP_TUPLE;
            if (out_first_tok) *out_first_tok = first;
            if (out_last_tok_excl) *out_last_tok_excl = p->cur;
            return id;
        }
        return OWL_AST_ID_NONE;
    }

    /* function decl? Identifier '(' ... ')' ->? block
       Disambiguation:
       - Parse as function declaration ONLY if, after matching ')', the next significant token is '->' or '{'.
       - Otherwise, treat as a statement (e.g., an expression statement like a function call). */
    if (i < p->token_count && p->tokens[i].kind == OWL_TOK_IDENTIFIER) {
        OwlTokIdx la[2];
        lookahead_sig(p, 2, la); /* la[0] is identifier, la[1] is next significant */
        if (la[1] < p->token_count && p->tokens[la[1]].kind == OWL_TOK_LPAREN) {
            /* Scan ahead to find the matching ')' without consuming tokens */
            OwlTokIdx j = la[1] + 1;
            int depth = 1;
            while (j < p->token_count) {
                OwlTokenKind k = p->tokens[j].kind;
                if (!is_trivia_kind(k)) {
                    if (k == OWL_TOK_LPAREN) {
                        depth++;
                    } else if (k == OWL_TOK_RPAREN) {
                        depth--;
                        if (depth == 0) { j++; break; }
                    }
                }
                j++;
            }
            /* Skip trailing trivia after ')' */
            while (j < p->token_count && is_trivia_kind(p->tokens[j].kind)) j++;
            /* If next significant token is '->' or '{', this is a function declaration */
            if (j < p->token_count &&
                (p->tokens[j].kind == OWL_TOK_ARROW || p->tokens[j].kind == OWL_TOK_LBRACE)) {
                p->cur = la[0] + 1; /* consume name */
                OwlAstId fd = parse_func_decl(p, la[0]);
                if (fd != OWL_AST_ID_NONE) {
                    if (out_kind) *out_kind = OWL_TOP_FUNC;
                    if (out_first_tok) *out_first_tok = first;
                    if (out_last_tok_excl) *out_last_tok_excl = p->cur;
                    return fd;
                }
                /* fallthrough to statement if function form failed */
            }
        }
    }

    /* otherwise, parse a statement */
    OwlAstId st = parse_stmt(p);
    if (st != OWL_AST_ID_NONE) {
        if (out_kind) *out_kind = OWL_TOP_STMT;
        if (out_first_tok) *out_first_tok = first;
        if (out_last_tok_excl) *out_last_tok_excl = p->cur;
        return st;
    }

    return OWL_AST_ID_NONE;
}

/* ---------- Types ---------- */

static int peek_is_type_keyword(OwlTokenKind k) {
    return (k == OWL_TOK_KW_STRING || k == OWL_TOK_KW_INT || k == OWL_TOK_KW_FLOAT ||
            k == OWL_TOK_KW_BOOL || k == OWL_TOK_KW_BYTE);
}

static OwlAstId parse_type_name_from_tok(Parser* p, OwlTokIdx name_idx) {
    OwlAstNode n = make_node(OWL_AST_TYPE_NAME);
    n.first_tok = name_idx;
    n.last_tok_excl = name_idx + 1;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);

    if (peek_is_type_keyword(p->tokens[name_idx].kind)) {
        n.as.type_name.flavor = OWL_TYPE_NAME_PRIMITIVE;
    } else {
        n.as.type_name.flavor = OWL_TYPE_NAME_NAMED;
    }
    n.as.type_name.name_tok = name_idx;
    return ast_push(&p->ast, &n);
}

static OwlAstId parse_type(Parser* p) {
    skip_trivia(p);
    OwlTokIdx i = peek_sig_idx(p);
    if (i >= p->token_count) return OWL_AST_ID_NONE;

    /* Array type: '[' Type ']' */
    if (p->tokens[i].kind == OWL_TOK_LBRACKET) {
        OwlAstNode n = make_node(OWL_AST_TYPE_ARRAY);
        n.first_tok = i;
        p->cur = i + 1; /* consume '[' */
        n.as.type_array.lbracket_tok = i;

        OwlAstId elem = parse_type(p);
        if (elem == OWL_AST_ID_NONE) {
            /* error recovery: expect some type name */
            OwlTokIdx j = peek_sig_idx(p);
            const OwlToken* t = tok(p, j < p->token_count ? j : (p->token_count - 1));
            OwlSpan sp = t ? t->span : (OwlSpan){0};
            errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_TYPE, sp, j);
        }
        n.as.type_array.elem_type = elem;

        OwlTokIdx rb = 0;
        expect_sig(p, OWL_TOK_RBRACKET, &rb, OWL_PARSE_ERR_EXPECTED_RBRACKET);
        n.as.type_array.rbracket_tok = rb;

        n.last_tok_excl = p->cur;
        n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
        return ast_push(&p->ast, &n);
    }

    /* Primitive or named type */
    if (peek_is_type_keyword(p->tokens[i].kind) || p->tokens[i].kind == OWL_TOK_IDENTIFIER) {
        p->cur = i + 1;
        return parse_type_name_from_tok(p, i);
    }

    return OWL_AST_ID_NONE;
}

/* ---------- Declarations ---------- */

static OwlAstId parse_tuple_decl(Parser* p, OwlTokIdx tuple_kw_tok) {
    /* we have already consumed 'tuple' */
    OwlAstNode n = make_node(OWL_AST_TUPLE_DECL);
    n.first_tok = tuple_kw_tok;
    n.as.tuple_decl.tuple_kw_tok = tuple_kw_tok;

    /* name */
    OwlTokIdx name_tok = 0;
    if (!expect_sig(p, OWL_TOK_IDENTIFIER, &name_tok, OWL_PARSE_ERR_EXPECTED_IDENTIFIER)) {
        if (p->opts.continue_after_error) sync_after_error(p);
        return OWL_AST_ID_NONE;
    }
    n.as.tuple_decl.name_tok = name_tok;

    /* '(' */
    OwlTokIdx lp = 0;
    expect_sig(p, OWL_TOK_LPAREN, &lp, OWL_PARSE_ERR_UNEXPECTED_TOKEN);
    n.as.tuple_decl.lparen_tok = lp;

    /* fields: (name type) separated by commas */
    for (;;) {
        skip_trivia(p);
        OwlTokIdx i = peek_sig_idx(p);
        if (i < p->token_count && p->tokens[i].kind == OWL_TOK_RPAREN) break;
        if (i >= p->token_count) break;

        /* name */
        OwlTokIdx field_name = 0;
        if (!expect_sig(p, OWL_TOK_IDENTIFIER, &field_name, OWL_PARSE_ERR_EXPECTED_IDENTIFIER)) {
            if (p->opts.continue_after_error) { sync_after_error(p); break; }
            else return OWL_AST_ID_NONE;
        }
        tokidx_slice_push(&n.as.tuple_decl.field_name_toks, field_name);

        /* type */
        OwlAstId ty = parse_type(p);
        if (ty == OWL_AST_ID_NONE) {
            /* record error but attempt to continue */
            OwlTokIdx j = peek_sig_idx(p);
            const OwlToken* t = tok(p, j < p->token_count ? j : (p->token_count - 1));
            OwlSpan sp = t ? t->span : (OwlSpan){0};
            errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_TYPE, sp, j);
        }
        astid_slice_push(&n.as.tuple_decl.field_types, ty);

        /* comma or end */
        OwlTokIdx comma = 0;
        if (match_sig(p, OWL_TOK_COMMA, &comma)) {
            tokidx_slice_push(&n.as.tuple_decl.field_commas, comma);
            continue;
        } else {
            break;
        }
    }

    /* ')' */
    OwlTokIdx rp = 0;
    expect_sig(p, OWL_TOK_RPAREN, &rp, OWL_PARSE_ERR_EXPECTED_RPAREN);
    n.as.tuple_decl.rparen_tok = rp;

    n.last_tok_excl = p->cur;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
    return ast_push(&p->ast, &n);
}

static OwlAstId parse_func_decl(Parser* p, OwlTokIdx name_tok) {
    OwlAstNode n = make_node(OWL_AST_FUNC_DECL);
    n.first_tok = name_tok;
    n.as.func_decl.name_tok = name_tok;

    /* '(' */
    OwlTokIdx lp = 0;
    if (!expect_sig(p, OWL_TOK_LPAREN, &lp, OWL_PARSE_ERR_UNEXPECTED_TOKEN)) return OWL_AST_ID_NONE;
    n.as.func_decl.lparen_tok = lp;

    /* params (name type) list */
    skip_trivia(p);
    OwlTokIdx i = peek_sig_idx(p);
    if (i < p->token_count && p->tokens[i].kind != OWL_TOK_RPAREN) {
        for (;;) {
            OwlTokIdx pname = 0;
            if (!expect_sig(p, OWL_TOK_IDENTIFIER, &pname, OWL_PARSE_ERR_EXPECTED_IDENTIFIER)) {
                if (p->opts.continue_after_error) { sync_after_error(p); break; }
                else return OWL_AST_ID_NONE;
            }
            tokidx_slice_push(&n.as.func_decl.param_name_toks, pname);

            OwlAstId ptype = parse_type(p);
            if (ptype == OWL_AST_ID_NONE) {
                OwlTokIdx j = peek_sig_idx(p);
                const OwlToken* t = tok(p, j < p->token_count ? j : (p->token_count - 1));
                OwlSpan sp = t ? t->span : (OwlSpan){0};
                errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_TYPE, sp, j);
            }
            astid_slice_push(&n.as.func_decl.param_types, ptype);

            OwlTokIdx comma = 0;
            if (match_sig(p, OWL_TOK_COMMA, &comma)) {
                tokidx_slice_push(&n.as.func_decl.param_commas, comma);
                continue;
            } else {
                break;
            }
        }
    }

    /* ')' */
    OwlTokIdx rp = 0;
    expect_sig(p, OWL_TOK_RPAREN, &rp, OWL_PARSE_ERR_EXPECTED_RPAREN);
    n.as.func_decl.rparen_tok = rp;

    /* optional '->' return type */
    OwlTokIdx arrow = 0;
    if (match_sig(p, OWL_TOK_ARROW, &arrow)) {
        n.as.func_decl.arrow_tok = arrow;
        OwlAstId rtype = parse_type(p);
        if (rtype == OWL_AST_ID_NONE) {
            OwlTokIdx j = peek_sig_idx(p);
            const OwlToken* t = tok(p, j < p->token_count ? j : (p->token_count - 1));
            OwlSpan sp = t ? t->span : (OwlSpan){0};
            errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_TYPE, sp, j);
        }
        n.as.func_decl.return_type = rtype;
    }

    /* body block */
    OwlAstId body = parse_block(p);
    if (body == OWL_AST_ID_NONE) {
        if (!p->opts.continue_after_error) return OWL_AST_ID_NONE;
        sync_after_error(p);
    }
    n.as.func_decl.body_block = body;

    n.last_tok_excl = p->cur;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
    return ast_push(&p->ast, &n);
}

/* ---------- Statements ---------- */

static OwlAstId parse_block(Parser* p) {
    OwlTokIdx lb = 0;
    if (!expect_sig(p, OWL_TOK_LBRACE, &lb, OWL_PARSE_ERR_UNEXPECTED_TOKEN)) return OWL_AST_ID_NONE;

    OwlAstNode n = make_node(OWL_AST_STMT_BLOCK);
    n.first_tok = lb;
    n.as.stmt_block.lbrace_tok = lb;

    for (;;) {
        skip_trivia(p);
        OwlTokIdx i = peek_sig_idx(p);
        if (i >= p->token_count) break;
        if (p->tokens[i].kind == OWL_TOK_RBRACE) break;
        if (p->tokens[i].kind == OWL_TOK_EOF) break;

        OwlAstId stmt = parse_stmt(p);
        if (stmt == OWL_AST_ID_NONE) {
            if (!p->opts.continue_after_error) break;
            sync_after_error(p);
        } else {
            astid_slice_push(&n.as.stmt_block.stmts, stmt);
        }
    }

    OwlTokIdx rb = 0;
    expect_sig(p, OWL_TOK_RBRACE, &rb, OWL_PARSE_ERR_EXPECTED_RBRACE);
    n.as.stmt_block.rbrace_tok = rb;

    n.last_tok_excl = p->cur;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
    return ast_push(&p->ast, &n);
}

static OwlAstId parse_stmt_if(Parser* p, OwlTokIdx if_tok) {
    OwlAstNode n = make_node(OWL_AST_STMT_IF);
    n.first_tok = if_tok;
    n.as.stmt_if.if_kw_tok = if_tok;

    /* condition expr */
    OwlAstId cond = parse_expr(p);
    if (cond == OWL_AST_ID_NONE) {
        OwlTokIdx j = peek_sig_idx(p);
        const OwlToken* t = tok(p, j < p->token_count ? j : (p->token_count - 1));
        OwlSpan sp = t ? t->span : (OwlSpan){0};
        errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_EXPR, sp, j);
    }
    n.as.stmt_if.cond_expr = cond;

    /* then block */
    OwlAstId thenb = parse_block(p);
    n.as.stmt_if.then_block = thenb;

    /* optional else */
    OwlTokIdx else_kw = 0;
    if (match_sig(p, OWL_TOK_KW_ELSE, &else_kw)) {
        n.as.stmt_if.else_kw_tok = else_kw;
        OwlAstId elseb = parse_block(p);
        n.as.stmt_if.else_block = elseb;
    }

    n.last_tok_excl = p->cur;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
    return ast_push(&p->ast, &n);
}

static OwlAstId parse_stmt_loop(Parser* p, OwlTokIdx loop_tok) {
    OwlAstNode n = make_node(OWL_AST_STMT_LOOP);
    n.first_tok = loop_tok;
    n.as.stmt_loop.loop_kw_tok = loop_tok;

    /* condition expr */
    OwlAstId cond = parse_expr(p);
    if (cond == OWL_AST_ID_NONE) {
        OwlTokIdx j = peek_sig_idx(p);
        const OwlToken* t = tok(p, j < p->token_count ? j : (p->token_count - 1));
        OwlSpan sp = t ? t->span : (OwlSpan){0};
        errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_EXPR, sp, j);
    }
    n.as.stmt_loop.cond_expr = cond;

    /* body block */
    OwlAstId body = parse_block(p);
    n.as.stmt_loop.body_block = body;

    /* else */
    OwlTokIdx else_kw = 0;
    if (match_sig(p, OWL_TOK_KW_ELSE, &else_kw)) {
        n.as.stmt_loop.else_kw_tok = else_kw;
        OwlAstId elseb = parse_block(p);
        n.as.stmt_loop.else_block = elseb;
    }

    n.last_tok_excl = p->cur;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
    return ast_push(&p->ast, &n);
}

static OwlAstId parse_stmt_return(Parser* p, OwlTokIdx ret_tok) {
    OwlAstNode n = make_node(OWL_AST_STMT_RETURN);
    n.first_tok = ret_tok;
    n.as.stmt_return.return_kw_tok = ret_tok;

    /* optional expr */
    skip_trivia(p);
    OwlTokIdx i = peek_sig_idx(p);
    if (i < p->token_count && p->tokens[i].kind != OWL_TOK_SEMICOLON) {
        OwlAstId e = parse_expr(p);
        n.as.stmt_return.opt_expr = e;
    }

    /* Optional semicolon: do not error if missing */
    OwlTokIdx semi = 0;
    if (match_sig(p, OWL_TOK_SEMICOLON, &semi)) {
        n.as.stmt_return.semi_tok = semi;
    }

    n.last_tok_excl = p->cur;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
    return ast_push(&p->ast, &n);
}

static int next_is_assign_after_optional_type(Parser* p) {
    /* Look for: ( [ Type ] ) '=' where:
       - Type can be:
         * '[' ... ']'
         * primitive keyword
         * identifier (single token)
       We don't consume; just peeking. */
    OwlTokIdx save = p->cur;

    /* Skip trivia after the name already consumed */
    OwlTokIdx i = peek_sig_idx(p);
    if (i >= p->token_count) return 0;
    OwlTokenKind k = p->tokens[i].kind;

    if (k == OWL_TOK_LBRACKET) {
        /* find matching ']' */
        int depth = 0;
        do {
            if (i >= p->token_count) { p->cur = save; return 0; }
            k = p->tokens[i].kind;
            if (k == OWL_TOK_LBRACKET) depth++;
            else if (k == OWL_TOK_RBRACKET) depth--;
            i++;
        } while (depth > 0);
        /* now peek '=' */
        while (i < p->token_count && is_trivia_kind(p->tokens[i].kind)) i++;
        p->cur = save;
        return (i < p->token_count && p->tokens[i].kind == OWL_TOK_ASSIGN);
    } else if (peek_is_type_keyword(k)) {
        i++;
        while (i < p->token_count && is_trivia_kind(p->tokens[i].kind)) i++;
        p->cur = save;
        return (i < p->token_count && p->tokens[i].kind == OWL_TOK_ASSIGN);
    } else if (k == OWL_TOK_IDENTIFIER) {
        i++;
        while (i < p->token_count && is_trivia_kind(p->tokens[i].kind)) i++;
        p->cur = save;
        return (i < p->token_count && p->tokens[i].kind == OWL_TOK_ASSIGN);
    }

    p->cur = save;
    return (k == OWL_TOK_ASSIGN);
}

static OwlAstId parse_stmt_var_or_assign(Parser* p, OwlTokIdx name_tok) {
    OwlAstNode n = make_node(OWL_AST_STMT_VAR_OR_ASSIGN);
    n.first_tok = name_tok;
    n.as.stmt_var_or_assign.name_tok = name_tok;

    /* Optional type annotation */
    skip_trivia(p);
    OwlTokIdx i = peek_sig_idx(p);
    if (i < p->token_count) {
        OwlTokenKind k = p->tokens[i].kind;
        if (k == OWL_TOK_LBRACKET || peek_is_type_keyword(k)) {
            OwlAstId ty = parse_type(p);
            n.as.stmt_var_or_assign.opt_type = ty;
        } else if (k == OWL_TOK_IDENTIFIER) {
            /* Heuristic: treat as type name if followed by '=' */
            OwlTokIdx i2 = i + 1;
            while (i2 < p->token_count && is_trivia_kind(p->tokens[i2].kind)) i2++;
            if (i2 < p->token_count && p->tokens[i2].kind == OWL_TOK_ASSIGN) {
                /* consume identifier as type name */
                p->cur = i + 1;
                n.as.stmt_var_or_assign.opt_type = parse_type_name_from_tok(p, i);
            }
        }
    }

    /* '=' */
    OwlTokIdx assign_tok = 0;
    expect_sig(p, OWL_TOK_ASSIGN, &assign_tok, OWL_PARSE_ERR_EXPECTED_ASSIGN);
    n.as.stmt_var_or_assign.assign_tok = assign_tok;

    /* expr */
    OwlAstId rhs = parse_expr(p);
    n.as.stmt_var_or_assign.rhs_expr = rhs;

    /* Optional ';' */
    OwlTokIdx semi = 0;
    if (match_sig(p, OWL_TOK_SEMICOLON, &semi)) {
        n.as.stmt_var_or_assign.semi_tok = semi;
    }

    n.last_tok_excl = p->cur;
    n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
    return ast_push(&p->ast, &n);
}

static OwlAstId parse_stmt(Parser* p) {
    skip_trivia(p);
    OwlTokIdx i = peek_sig_idx(p);
    if (i >= p->token_count) return OWL_AST_ID_NONE;

    OwlTokenKind k = p->tokens[i].kind;
    if (k == OWL_TOK_LBRACE) {
        return parse_block(p);
    }
    if (k == OWL_TOK_KW_IF) {
        p->cur = i + 1;
        return parse_stmt_if(p, i);
    }
    if (k == OWL_TOK_KW_LOOP) {
        p->cur = i + 1;
        return parse_stmt_loop(p, i);
    }
    if (k == OWL_TOK_KW_RETURN) {
        p->cur = i + 1;
        return parse_stmt_return(p, i);
    }
    if (k == OWL_TOK_IDENTIFIER) {
        /* Could be var/assign or an expression statement (e.g., a function call). */
        OwlTokIdx name_tok = i;
        p->cur = i + 1; /* consume identifier */
        /* Decide based on next significant token: '=' => var/assign, otherwise expression stmt */
        OwlTokIdx next = peek_sig_idx(p);
        if (next < p->token_count && p->tokens[next].kind == OWL_TOK_ASSIGN) {
            return parse_stmt_var_or_assign(p, name_tok);
        } else {
            /* Build identifier expr and parse postfix (calls, indexing, etc.) */
            OwlAstNode idn = make_node(OWL_AST_EXPR_IDENTIFIER);
            idn.first_tok = name_tok;
            idn.as.expr_ident.name_tok = name_tok;
            idn.last_tok_excl = p->cur;
            idn.span = span_from_tokens(p, idn.first_tok, idn.last_tok_excl);
            OwlAstId expr = ast_push(&p->ast, &idn);

            expr = parse_postfix(p, expr);

            /* Wrap as an expression statement with optional semicolon */
            OwlAstNode st = make_node(OWL_AST_STMT_EXPR);
            st.first_tok = name_tok;
            st.as.stmt_expr.expr = expr;
            OwlTokIdx semi = 0;
            if (match_sig(p, OWL_TOK_SEMICOLON, &semi)) {
                st.as.stmt_expr.semi_tok = semi;
            }
            st.last_tok_excl = p->cur;
            st.span = span_from_tokens(p, st.first_tok, st.last_tok_excl);
            return ast_push(&p->ast, &st);
        }
    }

    /* Unexpected start of statement */
    const OwlToken* t = tok(p, i);
    OwlSpan sp = t ? t->span : (OwlSpan){0};
    errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_STMT, sp, i);
    if (p->opts.continue_after_error) sync_after_error(p);
    return OWL_AST_ID_NONE;
}

/* ---------- Expressions ---------- */

static int is_unary_op(OwlTokenKind k) {
    return (k == OWL_TOK_PLUS || k == OWL_TOK_MINUS || k == OWL_TOK_UNKNOWN /* placeholder */ || k == OWL_TOK_BANG_EQ /* not correct '!' token kind not defined */);
    /* NOTE: The lexer doesn't define a dedicated '!' token for unary not.
       For now, we treat '!' via a small workaround: we'll detect a single '!' as UNKNOWN and upgrade it.
       A better approach is to add OWL_TOK_BANG to the lexer. */
}

static int is_binary_op(OwlTokenKind k) {
    switch (k) {
        case OWL_TOK_OR_OR:
        case OWL_TOK_AND_AND:
        case OWL_TOK_EQ_EQ:
        case OWL_TOK_BANG_EQ:
        case OWL_TOK_LT:
        case OWL_TOK_LT_EQ:
        case OWL_TOK_GT:
        case OWL_TOK_GT_EQ:
        case OWL_TOK_PLUS:
        case OWL_TOK_MINUS:
        case OWL_TOK_STAR:
        case OWL_TOK_SLASH:
        case OWL_TOK_PERCENT:
            return 1;
        default:
            return 0;
    }
}

static int get_binary_prec(OwlTokenKind k) {
    switch (k) {
        case OWL_TOK_OR_OR:   return 1;
        case OWL_TOK_AND_AND: return 2;
        case OWL_TOK_EQ_EQ:
        case OWL_TOK_BANG_EQ: return 3;
        case OWL_TOK_LT:
        case OWL_TOK_LT_EQ:
        case OWL_TOK_GT:
        case OWL_TOK_GT_EQ:   return 4;
        case OWL_TOK_PLUS:
        case OWL_TOK_MINUS:   return 5;
        case OWL_TOK_STAR:
        case OWL_TOK_SLASH:
        case OWL_TOK_PERCENT: return 6;
        default: return 0;
    }
}

static OwlAstId parse_primary(Parser* p) {
    skip_trivia(p);
    OwlTokIdx i = peek_sig_idx(p);
    if (i >= p->token_count) return OWL_AST_ID_NONE;
    OwlTokenKind k = p->tokens[i].kind;

    /* Literals */
    if (k == OWL_TOK_INT_LITERAL || k == OWL_TOK_FLOAT_LITERAL || k == OWL_TOK_STRING_LITERAL ||
        k == OWL_TOK_KW_TRUE || k == OWL_TOK_KW_FALSE) {
        OwlAstNode n = make_node(OWL_AST_EXPR_LITERAL);
        n.first_tok = i;
        p->cur = i + 1;
        if (k == OWL_TOK_INT_LITERAL) n.as.expr_literal.flavor = OWL_LITERAL_INT;
        else if (k == OWL_TOK_FLOAT_LITERAL) n.as.expr_literal.flavor = OWL_LITERAL_FLOAT;
        else if (k == OWL_TOK_STRING_LITERAL) n.as.expr_literal.flavor = OWL_LITERAL_STRING;
        else n.as.expr_literal.flavor = OWL_LITERAL_BOOL;
        n.as.expr_literal.tok = i;
        n.last_tok_excl = p->cur;
        n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
        return ast_push(&p->ast, &n);
    }

    /* Grouping */
    if (k == OWL_TOK_LPAREN) {
        OwlAstNode n = make_node(OWL_AST_EXPR_GROUP);
        n.first_tok = i;
        p->cur = i + 1;
        n.as.expr_group.lparen_tok = i;
        OwlAstId inner = parse_expr(p);
        n.as.expr_group.inner = inner;
        OwlTokIdx rp = 0;
        expect_sig(p, OWL_TOK_RPAREN, &rp, OWL_PARSE_ERR_EXPECTED_RPAREN);
        n.as.expr_group.rparen_tok = rp;
        n.last_tok_excl = p->cur;
        n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
        return ast_push(&p->ast, &n);
    }

    /* Array literal */
    if (k == OWL_TOK_LBRACKET) {
        OwlAstNode n = make_node(OWL_AST_EXPR_ARRAY);
        n.first_tok = i;
        p->cur = i + 1;
        n.as.expr_array.lbracket_tok = i;

        skip_trivia(p);
        OwlTokIdx j = peek_sig_idx(p);
        if (j < p->token_count && p->tokens[j].kind != OWL_TOK_RBRACKET) {
            for (;;) {
                OwlAstId e = parse_expr(p);
                astid_slice_push(&n.as.expr_array.elements, e);
                OwlTokIdx comma = 0;
                if (match_sig(p, OWL_TOK_COMMA, &comma)) {
                    tokidx_slice_push(&n.as.expr_array.element_commas, comma);
                    continue;
                } else {
                    break;
                }
            }
        }

        OwlTokIdx rb = 0;
        expect_sig(p, OWL_TOK_RBRACKET, &rb, OWL_PARSE_ERR_EXPECTED_RBRACKET);
        n.as.expr_array.rbracket_tok = rb;
        n.last_tok_excl = p->cur;
        n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
        return ast_push(&p->ast, &n);
    }

    /* Identifier (could be bare, call, or tuple ctor with named args) */
    if (k == OWL_TOK_IDENTIFIER) {
        /* Start with bare identifier expr */
        OwlAstNode idn = make_node(OWL_AST_EXPR_IDENTIFIER);
        idn.first_tok = i;
        idn.as.expr_ident.name_tok = i;
        p->cur = i + 1;
        idn.last_tok_excl = p->cur;
        idn.span = span_from_tokens(p, idn.first_tok, idn.last_tok_excl);
        OwlAstId id_expr = ast_push(&p->ast, &idn);

        /* Postfixes: calls/index */
        return parse_postfix(p, id_expr);
    }

    /* Unknown primary */
    const OwlToken* t = tok(p, i);
    OwlSpan sp = t ? t->span : (OwlSpan){0};
    errors_push(&p->errors, OWL_PARSE_ERR_EXPECTED_EXPR, sp, i);
    /* consume to make progress */
    p->cur = i + 1;
    return OWL_AST_ID_NONE;
}

static int next_paren_has_named_args(Parser* p) {
    /* Assume current position is right before '('; look inside parens:
       if first non-trivia inside is IDENTIFIER and next is '=', return 1. */
    OwlTokIdx save = p->cur;
    OwlTokIdx lp = peek_sig_idx(p);
    if (lp >= p->token_count || p->tokens[lp].kind != OWL_TOK_LPAREN) return 0;
    OwlTokIdx i = lp + 1;
    while (i < p->token_count && is_trivia_kind(p->tokens[i].kind)) i++;
    if (i < p->token_count && p->tokens[i].kind == OWL_TOK_RPAREN) { p->cur = save; return 0; }
    if (i < p->token_count && p->tokens[i].kind == OWL_TOK_IDENTIFIER) {
        OwlTokIdx j = i + 1;
        while (j < p->token_count && is_trivia_kind(p->tokens[j].kind)) j++;
        int res = (j < p->token_count && p->tokens[j].kind == OWL_TOK_ASSIGN);
        p->cur = save;
        return res;
    }
    p->cur = save;
    return 0;
}

static OwlAstId parse_postfix(Parser* p, OwlAstId primary) {
    for (;;) {
        skip_trivia(p);
        OwlTokIdx i = peek_sig_idx(p);
        if (i >= p->token_count) break;
        OwlTokenKind k = p->tokens[i].kind;

        if (k == OWL_TOK_LPAREN) {
            /* Call or tuple ctor (named) when callee is an identifier node */
            /* Fetch the callee node */
            const OwlAstNode* callee_node = owl_ast_get_const(&p->ast, primary);
            int callee_is_identifier = callee_node && callee_node->kind == OWL_AST_EXPR_IDENTIFIER;

            if (callee_is_identifier && next_paren_has_named_args(p)) {
                /* Tuple ctor with named args */
                OwlAstNode n = make_node(OWL_AST_EXPR_TUPLE_CTOR);
                n.first_tok = callee_node->first_tok;
                n.as.expr_tuple_ctor.name_tok = callee_node->as.expr_ident.name_tok;

                /* consume '(' */
                p->cur = i + 1;
                n.as.expr_tuple_ctor.lparen_tok = i;

                /* named list */
                for (;;) {
                    skip_trivia(p);
                    OwlTokIdx j = peek_sig_idx(p);
                    if (j < p->token_count && p->tokens[j].kind == OWL_TOK_RPAREN) break;

                    OwlTokIdx fname = 0;
                    if (!expect_sig(p, OWL_TOK_IDENTIFIER, &fname, OWL_PARSE_ERR_EXPECTED_IDENTIFIER)) break;
                    tokidx_slice_push(&n.as.expr_tuple_ctor.named_field_toks, fname);

                    OwlTokIdx eq = 0;
                    expect_sig(p, OWL_TOK_ASSIGN, &eq, OWL_PARSE_ERR_EXPECTED_ASSIGN);
                    tokidx_slice_push(&n.as.expr_tuple_ctor.named_eq_toks, eq);

                    OwlAstId val = parse_expr(p);
                    astid_slice_push(&n.as.expr_tuple_ctor.named_values, val);

                    OwlTokIdx comma = 0;
                    if (match_sig(p, OWL_TOK_COMMA, &comma)) {
                        tokidx_slice_push(&n.as.expr_tuple_ctor.named_commas, comma);
                        continue;
                    } else {
                        break;
                    }
                }

                OwlTokIdx rp = 0;
                expect_sig(p, OWL_TOK_RPAREN, &rp, OWL_PARSE_ERR_EXPECTED_RPAREN);
                n.as.expr_tuple_ctor.rparen_tok = rp;

                n.last_tok_excl = p->cur;
                n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);

                primary = ast_push(&p->ast, &n);
                continue;
            } else {
                /* Call with positional args */
                OwlAstNode n = make_node(OWL_AST_EXPR_CALL);
                const OwlAstNode* base = owl_ast_get_const(&p->ast, primary);
                n.first_tok = base ? base->first_tok : i;
                n.as.expr_call.callee = primary;

                p->cur = i + 1;
                n.as.expr_call.lparen_tok = i;

                skip_trivia(p);
                OwlTokIdx j = peek_sig_idx(p);
                if (j < p->token_count && p->tokens[j].kind != OWL_TOK_RPAREN) {
                    for (;;) {
                        OwlAstId arg = parse_expr(p);
                        astid_slice_push(&n.as.expr_call.args, arg);
                        OwlTokIdx comma = 0;
                        if (match_sig(p, OWL_TOK_COMMA, &comma)) {
                            tokidx_slice_push(&n.as.expr_call.arg_commas, comma);
                            continue;
                        } else {
                            break;
                        }
                    }
                }

                OwlTokIdx rp = 0;
                expect_sig(p, OWL_TOK_RPAREN, &rp, OWL_PARSE_ERR_EXPECTED_RPAREN);
                n.as.expr_call.rparen_tok = rp;

                n.last_tok_excl = p->cur;
                n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);

                primary = ast_push(&p->ast, &n);
                continue;
            }
        }

        if (k == OWL_TOK_LBRACKET) {
            /* Indexing: target '[' expr ']' */
            OwlAstNode n = make_node(OWL_AST_EXPR_INDEX);
            const OwlAstNode* base = owl_ast_get_const(&p->ast, primary);
            n.first_tok = base ? base->first_tok : i;
            n.as.expr_index.target = primary;

            p->cur = i + 1;
            n.as.expr_index.lbracket_tok = i;

            OwlAstId idx = parse_expr(p);
            n.as.expr_index.index_expr = idx;

            OwlTokIdx rb = 0;
            expect_sig(p, OWL_TOK_RBRACKET, &rb, OWL_PARSE_ERR_EXPECTED_RBRACKET);
            n.as.expr_index.rbracket_tok = rb;

            n.last_tok_excl = p->cur;
            n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);

            primary = ast_push(&p->ast, &n);
            continue;
        }

        break;
    }

    return primary;
}

static OwlAstId parse_unary(Parser* p) {
    skip_trivia(p);
    OwlTokIdx i = peek_sig_idx(p);
    if (i >= p->token_count) return OWL_AST_ID_NONE;

    OwlTokenKind k = p->tokens[i].kind;
    /* The lexer currently doesn't provide OWL_TOK_BANG for '!' unary.
       We'll accept PLUS/MINUS as unary and ignore '!' for now. */
    if (k == OWL_TOK_PLUS || k == OWL_TOK_MINUS /* || k == OWL_TOK_BANG */) {
        OwlAstNode n = make_node(OWL_AST_EXPR_UNARY);
        n.first_tok = i;
        p->cur = i + 1;
        n.as.expr_unary.op_tok_kind = k;
        n.as.expr_unary.op_tok = i;
        OwlAstId rhs = parse_unary(p);
        n.as.expr_unary.operand = rhs;
        n.last_tok_excl = p->cur;
        n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);
        return ast_push(&p->ast, &n);
    }

    OwlAstId prim = parse_primary(p);
    return prim;
}

static OwlAstId parse_expr_bp(Parser* p, int min_prec) {
    OwlAstId left = parse_unary(p);
    for (;;) {
        skip_trivia(p);
        OwlTokIdx i = peek_sig_idx(p);
        if (i >= p->token_count) break;
        OwlTokenKind k = p->tokens[i].kind;
        if (!is_binary_op(k)) break;

        int prec = get_binary_prec(k);
        if (prec < min_prec) break;

        /* left-associative */
        OwlTokIdx op_tok = i;
        p->cur = i + 1;

        OwlAstId right = parse_expr_bp(p, prec + 1);

        OwlAstNode n = make_node(OWL_AST_EXPR_BINARY);
        /* Determine coverage from left node start */
        const OwlAstNode* leftn = owl_ast_get_const(&p->ast, left);
        n.first_tok = leftn ? leftn->first_tok : op_tok;
        n.as.expr_binary.left = left;
        n.as.expr_binary.op_tok = op_tok;
        n.as.expr_binary.op_tok_kind = k;
        n.as.expr_binary.right = right;
        n.last_tok_excl = p->cur;
        n.span = span_from_tokens(p, n.first_tok, n.last_tok_excl);

        left = ast_push(&p->ast, &n);
    }
    return left;
}

static OwlAstId parse_expr(Parser* p) {
    return parse_expr_bp(p, 1);
}

/* ===========================
   Public API
   =========================== */

static void collect_tokens(Parser* p) {
    /* Lex entire source into tokens buffer */
    OwlLexer lx;
    owl_lexer_init(&lx, p->source, p->source_len);

    size_t cap = 0, len = 0;
    OwlToken* vec = NULL;

    for (;;) {
        OwlToken t = owl_lexer_next(&lx);
        if (len + 1 > cap) {
            size_t new_cap = cap ? cap * 2 : 64;
            vec = OWL_REALLOC(OwlToken, vec, new_cap);
            cap = new_cap;
        }
        vec[len++] = t;
        if (t.kind == OWL_TOK_EOF) break;
    }

    p->tokens = vec;
    p->token_count = len;
    p->cur = 0;
}

OwlParseResult owl_parse(const char* source, size_t source_len, const OwlParserOptions* options) {
    OwlParseResult r;
    memset(&r, 0, sizeof(r));
    r.source = source;
    r.source_len = source_len;

    Parser p;
    memset(&p, 0, sizeof(p));
    p.source = source;
    p.source_len = source_len;
    p.opts.continue_after_error = 1;
    if (options) p.opts = *options;

    ast_init(&p.ast);

    /* Lex first to collect full token stream */
    collect_tokens(&p);

    /* Attach tokens to result (we will move ownership at the end) */
    r.tokens = p.tokens;
    r.token_count = p.token_count;

    /* Parse */
    OwlAstId root = parse_program(&p);
    r.ast = p.ast;
    r.root = root;

    /* Errors */
    r.errors = p.errors.items;
    r.error_count = p.errors.len;

    return r;
}

static void free_slice_tokidx(OwlTokIdxSlice* s) {
    if (s->items) free(s->items);
    s->items = NULL; s->len = s->cap = 0;
}

static void free_slice_astid(OwlAstIdSlice* s) {
    if (s->items) free(s->items);
    s->items = NULL; s->len = s->cap = 0;
}

void owl_parse_free(OwlParseResult* result) {
    if (!result) return;

    /* Free tokens */
    if (result->tokens) {
        free(result->tokens);
        result->tokens = NULL;
        result->token_count = 0;
    }

    /* Free AST arena (and any nested dynamic arrays) */
    if (result->ast.nodes) {
        for (size_t i = 0; i < result->ast.len; i++) {
            OwlAstNode* n = &result->ast.nodes[i];
            switch (n->kind) {
                case OWL_AST_TUPLE_DECL:
                    free_slice_tokidx(&n->as.tuple_decl.field_name_toks);
                    free_slice_astid(&n->as.tuple_decl.field_types);
                    free_slice_tokidx(&n->as.tuple_decl.field_commas);
                    break;
                case OWL_AST_FUNC_DECL:
                    free_slice_tokidx(&n->as.func_decl.param_name_toks);
                    free_slice_astid(&n->as.func_decl.param_types);
                    free_slice_tokidx(&n->as.func_decl.param_commas);
                    break;
                case OWL_AST_STMT_BLOCK:
                    free_slice_astid(&n->as.stmt_block.stmts);
                    break;
                case OWL_AST_STMT_VAR_OR_ASSIGN:
                    /* nothing heap-allocated */
                    break;
                case OWL_AST_STMT_RETURN:
                    /* nothing */
                    break;
                case OWL_AST_STMT_IF:
                    /* nothing */
                    break;
                case OWL_AST_STMT_LOOP:
                    /* nothing */
                    break;
                case OWL_AST_EXPR_CALL:
                    free_slice_astid(&n->as.expr_call.args);
                    free_slice_tokidx(&n->as.expr_call.arg_commas);
                    break;
                case OWL_AST_EXPR_INDEX:
                    break;
                case OWL_AST_EXPR_ARRAY:
                    free_slice_astid(&n->as.expr_array.elements);
                    free_slice_tokidx(&n->as.expr_array.element_commas);
                    break;
                case OWL_AST_EXPR_GROUP:
                    break;
                case OWL_AST_EXPR_TUPLE_CTOR:
                    free_slice_astid(&n->as.expr_tuple_ctor.positional_args);
                    free_slice_tokidx(&n->as.expr_tuple_ctor.positional_commas);
                    free_slice_tokidx(&n->as.expr_tuple_ctor.named_field_toks);
                    free_slice_tokidx(&n->as.expr_tuple_ctor.named_eq_toks);
                    free_slice_astid(&n->as.expr_tuple_ctor.named_values);
                    free_slice_tokidx(&n->as.expr_tuple_ctor.named_commas);
                    break;
                case OWL_AST_PROGRAM:
                    if (n->as.program.items) free(n->as.program.items);
                    n->as.program.items = NULL; n->as.program.len = n->as.program.cap = 0;
                    break;
                default:
                    break;
            }
        }
        free(result->ast.nodes);
        result->ast.nodes = NULL;
        result->ast.len = result->ast.cap = 0;
    }

    /* Free errors */
    if (result->errors) {
        free(result->errors);
        result->errors = NULL;
        result->error_count = 0;
    }
}
