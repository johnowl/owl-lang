/*
 * c/owl-lang/transpile.c
 *
 * Owl Transpiler — Iteration 1 Codegen Skeleton
 *
 * This file implements a minimal subset of the interfaces declared in transpile.h,
 * focusing on scaffolding for:
 *  - CodeWriter (buffered output, indentation)
 *  - Error collection
 *  - Scope stack for tracking string lifetimes
 *  - Codegen context init/free
 *  - Prologue emission (C17 includes and runtime header)
 *  - Very basic stubs for expression/statement emission and built-in print
 *
 * Notes:
 *  - Iteration 1 enforces "no type promotion". Mixed int/float expressions should
 *    be rejected at transpile-time (TODO in type checker).
 *  - String ownership: any string declared in a scope should be freed when that
 *    scope ends; reassigning a string should free the previous value first (TODO in emit).
 *  - The functions below provide a skeleton to grow from; many are stubs or partial.
 *
 * To integrate:
 *  - Include this source in your build alongside owl_rt.h, parser.h, and transpile.h.
 *  - Implement the TODO sections incrementally per the iteration plan.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "parser.h"
#include "transpile.h"

/* ============================================================
 * Internal helpers
 * ============================================================
 */

static void* xrealloc(void* p, size_t nbytes) {
    void* np = realloc(p, nbytes);
    if (!np) {
        fprintf(stderr, "[transpile] out of memory\n");
        abort();
    }
    return np;
}

static void* xmalloc(size_t nbytes) {
    void* p = malloc(nbytes);
    if (!p) {
        fprintf(stderr, "[transpile] out of memory\n");
        abort();
    }
    return p;
}

/* duplicate a non-null-terminated slice into a C string */
static char* strndup_owl(const char* s, size_t n) {
    char* p = (char*)malloc(n + 1);
    if (!p) {
        fprintf(stderr, "[transpile] out of memory (strndup_owl)\n");
        abort();
    }
    if (n && s) memcpy(p, s, n);
    p[n] = '\0';
    return p;
}

/* write a C-escaped string literal contents (without surrounding quotes) */
static void cw_write_c_escaped(OwlCodeWriter* cw, const char* s, size_t n) {
    for (size_t i = 0; i < n; i++) {
        unsigned char c = (unsigned char)s[i];
        switch (c) {
            case '\\': owl_cw_write(cw, "\\\\"); break;
            case '\"': owl_cw_write(cw, "\\\""); break;
            case '\n': owl_cw_write(cw, "\\n"); break;
            case '\r': owl_cw_write(cw, "\\r"); break;
            case '\t': owl_cw_write(cw, "\\t"); break;
            default:
                if (c < 0x20 || c == 0x7F) {
                    char buf[5];
                    snprintf(buf, sizeof(buf), "\\x%02X", c);
                    owl_cw_write(cw, buf);
                } else {
                    char ch[2] = {(char)c, 0};
                    owl_cw_write(cw, ch);
                }
        }
    }
}

/* ============================================================
 * Code Writer
 * ============================================================
 */

void owl_cw_init(OwlCodeWriter* cw) {
    cw->buf = NULL;
    cw->len = 0;
    cw->cap = 0;
    cw->indent = 0;
    cw->at_line_start = 1;
}

void owl_cw_free(OwlCodeWriter* cw) {
    if (!cw) return;
    if (cw->buf) free(cw->buf);
    cw->buf = NULL;
    cw->len = cw->cap = 0;
    cw->indent = 0;
    cw->at_line_start = 1;
}

void owl_cw_reserve(OwlCodeWriter* cw, size_t need) {
    if (cw->len + need <= cw->cap) return;
    size_t new_cap = cw->cap ? cw->cap * 2 : 1024;
    while (new_cap < cw->len + need) new_cap *= 2;
    cw->buf = (char*)xrealloc(cw->buf, new_cap);
    cw->cap = new_cap;
}

static void cw_write_indent(OwlCodeWriter* cw) {
    if (!cw->at_line_start) return;
    cw->at_line_start = 0;
    const int spaces_per_indent = 4;
    int total = cw->indent * spaces_per_indent;
    owl_cw_reserve(cw, (size_t)total);
    for (int i = 0; i < total; i++) {
        cw->buf[cw->len++] = ' ';
    }
}

void owl_cw_writen(OwlCodeWriter* cw, const char* s, size_t n) {
    if (!s || n == 0) return;
    cw_write_indent(cw);
    owl_cw_reserve(cw, n);
    memcpy(cw->buf + cw->len, s, n);
    cw->len += n;
}

void owl_cw_write(OwlCodeWriter* cw, const char* s) {
    if (!s) return;
    owl_cw_writen(cw, s, strlen(s));
}

void owl_cw_write_int(OwlCodeWriter* cw, long v) {
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "%ld", v);
    if (n < 0) n = 0;
    owl_cw_writen(cw, buf, (size_t)n);
}

void owl_cw_newline(OwlCodeWriter* cw) {
    owl_cw_reserve(cw, 1);
    cw->buf[cw->len++] = '\n';
    cw->at_line_start = 1;
}

void owl_cw_indent_inc(OwlCodeWriter* cw) { cw->indent++; }
void owl_cw_indent_dec(OwlCodeWriter* cw) { if (cw->indent > 0) cw->indent--; }

void owl_cw_open_block(OwlCodeWriter* cw) {
    owl_cw_write(cw, "{");
    owl_cw_newline(cw);
    owl_cw_indent_inc(cw);
}

void owl_cw_close_block(OwlCodeWriter* cw) {
    owl_cw_indent_dec(cw);
    owl_cw_write(cw, "}");
    owl_cw_newline(cw);
}

/* ============================================================
 * Error collection
 * ============================================================
 */

void owl_tgen_errors_init(OwlTranspileErrors* errs) {
    errs->items = NULL;
    errs->len = 0;
    errs->cap = 0;
}

void owl_tgen_errors_free(OwlTranspileErrors* errs) {
    if (!errs) return;
    if (errs->items) free(errs->items);
    errs->items = NULL;
    errs->len = errs->cap = 0;
}

void owl_tgen_errors_push(OwlTranspileErrors* errs, OwlTranspileErrorCode code, OwlTokIdx at_token, const char* message) {
    if (errs->len + 1 > errs->cap) {
        size_t nc = errs->cap ? errs->cap * 2 : 8;
        errs->items = (OwlTranspileError*)xrealloc(errs->items, nc * sizeof(OwlTranspileError));
        errs->cap = nc;
    }
    OwlTranspileError e;
    e.code = code;
    e.at_token = at_token;
    e.message = message;
    errs->items[errs->len++] = e;
}

/* ============================================================
 * Scope stack
 * ============================================================
 */

static void scope_frame_init(OwlScopeFrame* f) {
    f->symbols = NULL;
    f->len = 0;
    f->cap = 0;
}

static void scope_frame_free(OwlScopeFrame* f) {
    if (!f) return;
    if (f->symbols) free(f->symbols);
    f->symbols = NULL;
    f->len = f->cap = 0;
}

void owl_scope_init(OwlScopeStack* st) {
    st->frames = NULL;
    st->depth = 0;
    st->cap = 0;
}

void owl_scope_free(OwlScopeStack* st) {
    if (!st) return;
    for (size_t i = 0; i < st->depth; i++) {
        scope_frame_free(&st->frames[i]);
    }
    if (st->frames) free(st->frames);
    st->frames = NULL;
    st->depth = st->cap = 0;
}

void owl_scope_push(OwlScopeStack* st) {
    if (st->depth + 1 > st->cap) {
        size_t nc = st->cap ? st->cap * 2 : 4;
        st->frames = (OwlScopeFrame*)xrealloc(st->frames, nc * sizeof(OwlScopeFrame));
        st->cap = nc;
    }
    scope_frame_init(&st->frames[st->depth]);
    st->depth++;
}

void owl_emit_string_free(OwlCodegen* cg, const char* var_name); /* forward (defined later) */

void owl_scope_pop(OwlScopeStack* st, OwlCodeWriter* cw) {
    if (st->depth == 0) return;
    OwlScopeFrame* top = &st->frames[st->depth - 1];
    /* Emit frees for any string vars declared in this frame.
       We can't call owl_emit_string_free here (no cg), so inline the freeing pattern:
       caller is expected to wrap this via codegen (we provide a convenience below). */
    for (size_t i = 0; i < top->len; i++) {
        if (top->symbols[i].needs_free && top->symbols[i].name) {
            owl_cw_write(cw, "owl_str_free(&");
            owl_cw_write(cw, top->symbols[i].name);
            owl_cw_write(cw, ");");
            owl_cw_newline(cw);
        }
    }
    scope_frame_free(top);
    st->depth--;
}

int owl_scope_declare(OwlScopeStack* st, const char* name, OwlTypeInfo type, int needs_free) {
    if (st->depth == 0) return 0;
    OwlScopeFrame* top = &st->frames[st->depth - 1];
    if (top->len + 1 > top->cap) {
        size_t nc = top->cap ? top->cap * 2 : 4;
        top->symbols = (OwlSymbol*)xrealloc(top->symbols, nc * sizeof(OwlSymbol));
        top->cap = nc;
    }
    top->symbols[top->len].name = name;
    top->symbols[top->len].type = type;
    top->symbols[top->len].needs_free = needs_free ? 1 : 0;
    top->len++;
    return 1;
}

const OwlSymbol* owl_scope_lookup(const OwlScopeStack* st, const char* name) {
    if (st->depth == 0 || !name) return NULL;
    for (size_t d = st->depth; d > 0; d--) {
        const OwlScopeFrame* f = &st->frames[d - 1];
        for (size_t i = 0; i < f->len; i++) {
            if (f->symbols[i].name && strcmp(f->symbols[i].name, name) == 0) {
                return &f->symbols[i];
            }
        }
    }
    return NULL;
}

/* ============================================================
 * Codegen Context
 * ============================================================
 */

void owl_codegen_init(OwlCodegen* cg, const OwlParseResult* pr, const OwlTranspileOptions* opts) {
    memset(cg, 0, sizeof(*cg));
    cg->ast = &pr->ast;
    cg->nodes = pr->ast.nodes;
    cg->node_count = pr->ast.len;
    cg->tokens = pr->tokens;
    cg->token_count = pr->token_count;
    if (opts) cg->opts = *opts;

    owl_cw_init(&cg->writer);
    owl_scope_init(&cg->scopes);
    owl_tgen_errors_init(&cg->errors);
}

void owl_codegen_free(OwlCodegen* cg) {
    owl_scope_free(&cg->scopes);
    owl_cw_free(&cg->writer);
    owl_tgen_errors_free(&cg->errors);
    memset(cg, 0, sizeof(*cg));
}

/* ============================================================
 * Prologue emission
 * ============================================================
 */

void owl_emit_prologue(OwlCodegen* cg) {
    OwlCodeWriter* cw = &cg->writer;
    owl_cw_write(cw, "/* Generated by Owl Transpiler (Iteration 1) */");
    owl_cw_newline(cw);
    owl_cw_write(cw, "#include <stdint.h>");
    owl_cw_newline(cw);
    owl_cw_write(cw, "#include <stdio.h>");
    owl_cw_newline(cw);
    owl_cw_write(cw, "#include \"owl_rt.h\"");
    owl_cw_newline(cw);
    owl_cw_newline(cw);
}

/* ============================================================
 * Type system (Iteration 1) — stubs
 * ============================================================
 */

OwlTypeInfo owl_tgen_type_of_expr(OwlCodegen* cg, OwlAstId expr_id) {
    OwlTypeInfo ti; ti.kind = OWL_TY_UNKNOWN;
    const OwlAstNode* n = owl_ast_get_const(cg->ast, expr_id);
    if (!n) return ti;

    switch (n->kind) {
        case OWL_AST_EXPR_LITERAL:
            switch (n->as.expr_literal.flavor) {
                case OWL_LITERAL_INT:   ti.kind = OWL_TY_INT; break;
                case OWL_LITERAL_FLOAT: ti.kind = OWL_TY_FLOAT; break;
                case OWL_LITERAL_STRING:ti.kind = OWL_TY_STRING; break;
                case OWL_LITERAL_BOOL:  ti.kind = OWL_TY_BOOL; break;
                default: break;
            }
            break;
        case OWL_AST_EXPR_IDENTIFIER: {
            const OwlToken* toks = (const OwlToken*)cg->tokens;
            const OwlToken* t = &toks[n->as.expr_ident.name_tok];
            char* name = strndup_owl(t->lexeme, t->lexeme_len);
            const OwlSymbol* sym = owl_scope_lookup(&cg->scopes, name);
            free(name);
            if (sym) ti = sym->type;
            break;
        }
        default:
            break;
    }
    return ti;
}

int owl_tgen_require_same_type(OwlCodegen* cg, OwlTypeInfo lhs, OwlTypeInfo rhs, OwlTokIdx at_token) {
    (void)at_token;
    if (lhs.kind == rhs.kind && lhs.kind != OWL_TY_UNKNOWN) return 1;
    owl_tgen_errors_push(&cg->errors, OWL_TGEN_ERR_TYPE_MISMATCH, at_token, "type mismatch");
    return 0;
}

/* ============================================================
 * String helpers
 * ============================================================
 */

const char* owl_emit_string_literal_temp(OwlCodegen* cg, OwlAstId expr_id) {
    const OwlAstNode* lit = owl_ast_get_const(cg->ast, expr_id);
    const OwlToken* toks = (const OwlToken*)cg->tokens;

    static char tmp_name[32];
    static int counter = 0;
    snprintf(tmp_name, sizeof(tmp_name), "__str_tmp%d", counter++);

    /* Expect a string literal token */
    OwlTokIdx tok_idx = (lit && lit->kind == OWL_AST_EXPR_LITERAL) ? lit->as.expr_literal.tok : 0;
    const OwlToken* t = &toks[tok_idx];

    /* Write: OwlString __str_tmpN = owl_str_from_cstr("..."); */
    owl_cw_write(&cg->writer, "OwlString ");
    owl_cw_write(&cg->writer, tmp_name);
    owl_cw_write(&cg->writer, " = owl_str_from_cstr(\"");
    if (t->lexeme_len >= 2 && t->lexeme[0] == '\"') {
        const char* s = t->lexeme + 1;
        size_t n = t->lexeme_len - 2; /* strip quotes */
        cw_write_c_escaped(&cg->writer, s, n);
    }
    owl_cw_write(&cg->writer, "\");");
    owl_cw_newline(&cg->writer);
    return tmp_name;
}

void owl_emit_string_free(OwlCodegen* cg, const char* var_name) {
    if (!var_name) return;
    owl_cw_write(&cg->writer, "owl_str_free(&");
    owl_cw_write(&cg->writer, var_name);
    owl_cw_write(&cg->writer, ");");
    owl_cw_newline(&cg->writer);
}

/* ============================================================
 * Expression emission — skeleton
 * ============================================================
 */

const char* owl_emit_expr(OwlCodegen* cg, OwlAstId expr_id, OwlTypeInfo* out_type) {
    static char buf[128];
    buf[0] = '\0';
    const OwlAstNode* n = owl_ast_get_const(cg->ast, expr_id);
    const OwlToken* toks = (const OwlToken*)cg->tokens;
    if (out_type) out_type->kind = OWL_TY_UNKNOWN;
    if (!n) return "0";

    switch (n->kind) {
        case OWL_AST_EXPR_LITERAL: {
            const OwlToken* t = &toks[n->as.expr_literal.tok];
            switch (n->as.expr_literal.flavor) {
                case OWL_LITERAL_INT: {
                    if (out_type) out_type->kind = OWL_TY_INT;
                    /* copy lexeme into a null-terminated buffer */
                    size_t len = t->lexeme_len < sizeof(buf) - 1 ? t->lexeme_len : sizeof(buf) - 1;
                    memcpy(buf, t->lexeme, len);
                    buf[len] = '\0';
                    return buf;
                }
                case OWL_LITERAL_FLOAT:
                    if (out_type) out_type->kind = OWL_TY_FLOAT;
                    /* append 'f' to ensure float */
                    {
                        size_t len = t->lexeme_len < sizeof(buf) - 2 ? t->lexeme_len : sizeof(buf) - 2;
                        memcpy(buf, t->lexeme, len);
                        /* add 'f' if not present at end */
                        if (len > 0 && (buf[len-1] == 'f' || buf[len-1] == 'F')) {
                            buf[len] = '\0';
                        } else {
                            buf[len++] = 'f';
                            buf[len] = '\0';
                        }
                        return buf;
                    }
                case OWL_LITERAL_BOOL:
                    if (out_type) out_type->kind = OWL_TY_BOOL;
                    return (t->kind == OWL_TOK_KW_TRUE) ? "1" : "0";
                case OWL_LITERAL_STRING:
                    if (out_type) out_type->kind = OWL_TY_STRING;
                    /* Caller should handle string literal via temp; return placeholder */
                    return "__OWL_STRING_LITERAL__";
                default: break;
            }
            break;
        }
        case OWL_AST_EXPR_IDENTIFIER: {
            const OwlToken* t = &toks[n->as.expr_ident.name_tok];
            if (out_type) {
                char* name = strndup_owl(t->lexeme, t->lexeme_len);
                const OwlSymbol* sym = owl_scope_lookup(&cg->scopes, name);
                if (sym) *out_type = sym->type;
                free(name);
            }
            /* copy identifier name into a null-terminated buffer */
            size_t len = t->lexeme_len < sizeof(buf) - 1 ? t->lexeme_len : sizeof(buf) - 1;
            memcpy(buf, t->lexeme, len);
            buf[len] = '\0';
            return buf;
        }
        default:
            break;
    }
    return "0";
}

/* ============================================================
 * Statement emission — skeleton
 * ============================================================
 */

void owl_emit_stmt_var_or_assign(OwlCodegen* cg, OwlAstId stmt_id) {
    const OwlAstNode* st = owl_ast_get_const(cg->ast, stmt_id);
    if (!st || st->kind != OWL_AST_STMT_VAR_OR_ASSIGN) return;

    OwlCodeWriter* cw = &cg->writer;
    const OwlToken* toks = (const OwlToken*)cg->tokens;

    /* LHS name */
    const OwlToken* name_tok = &toks[st->as.stmt_var_or_assign.name_tok];
    char* name_c = strndup_owl(name_tok->lexeme, name_tok->lexeme_len);

    /* Determine RHS type */
    OwlAstId rhs = st->as.stmt_var_or_assign.rhs_expr;
    OwlTypeInfo rhs_ty = owl_tgen_type_of_expr(cg, rhs);

    /* Declaration vs assignment */
    const OwlSymbol* existing = owl_scope_lookup(&cg->scopes, name_c);
    int is_decl = (existing == NULL);

    if (is_decl) {
        /* map type to C type */
        switch (rhs_ty.kind) {
            case OWL_TY_INT:   owl_cw_write(cw, "int32_t "); break;
            case OWL_TY_FLOAT: owl_cw_write(cw, "float "); break;
            case OWL_TY_BOOL:  owl_cw_write(cw, "uint8_t "); break;
            case OWL_TY_BYTE:  owl_cw_write(cw, "uint8_t "); break;
            case OWL_TY_STRING:owl_cw_write(cw, "OwlString "); break;
            default:           owl_cw_write(cw, "/* unknown type */ int "); break;
        }
        /* name = init */
        owl_cw_writen(cw, name_tok->lexeme, name_tok->lexeme_len);
        owl_cw_write(cw, " = ");

        if (rhs_ty.kind == OWL_TY_STRING) {
            /* initialize string */
            const OwlAstNode* rn = owl_ast_get_const(cg->ast, rhs);
            if (rn && rn->kind == OWL_AST_EXPR_LITERAL && rn->as.expr_literal.flavor == OWL_LITERAL_STRING) {
                /* inline ctor: owl_str_from_cstr("...") */
                const OwlToken* lt = &toks[rn->as.expr_literal.tok];
                owl_cw_write(cw, "owl_str_from_cstr(\"");
                if (lt->lexeme_len >= 2 && lt->lexeme[0] == '\"') {
                    const char* s = lt->lexeme + 1;
                    size_t n = lt->lexeme_len - 2;
                    cw_write_c_escaped(cw, s, n);
                }
                owl_cw_write(cw, "\")");
            } else if (rn && rn->kind == OWL_AST_EXPR_IDENTIFIER) {
                const OwlToken* rt = &toks[rn->as.expr_ident.name_tok];
                owl_cw_write(cw, "owl_str_copy(");
                owl_cw_writen(cw, rt->lexeme, rt->lexeme_len);
                owl_cw_write(cw, ")");
            } else {
                owl_cw_write(cw, "/* unsupported string init */ owl_str_from_cstr(\"\")");
            }
            /* track for free on scope exit */
            OwlTypeInfo tinfo; tinfo.kind = OWL_TY_STRING;
            owl_scope_declare(&cg->scopes, name_c, tinfo, 1);
        } else {
            /* primitives: emit expression */
            OwlTypeInfo tmp;
            const char* expr_c = owl_emit_expr(cg, rhs, &tmp);
            /* if float literal, owl_emit_expr added 'f' */
            owl_cw_write(cw, expr_c);
            OwlTypeInfo tinfo; tinfo.kind = rhs_ty.kind;
            owl_scope_declare(&cg->scopes, name_c, tinfo, 0);
        }

        owl_cw_write(cw, ";");
        owl_cw_newline(cw);
    } else {
        /* assignment */
        if (existing->type.kind == OWL_TY_STRING) {
            /* free old value first */
            owl_cw_write(cw, "owl_str_free(&");
            owl_cw_writen(cw, name_tok->lexeme, name_tok->lexeme_len);
            owl_cw_write(cw, ");");
            owl_cw_newline(cw);

            owl_cw_writen(cw, name_tok->lexeme, name_tok->lexeme_len);
            owl_cw_write(cw, " = ");
            const OwlAstNode* rn = owl_ast_get_const(cg->ast, rhs);
            if (rn && rn->kind == OWL_AST_EXPR_LITERAL && rn->as.expr_literal.flavor == OWL_LITERAL_STRING) {
                const OwlToken* lt = &toks[rn->as.expr_literal.tok];
                owl_cw_write(cw, "owl_str_from_cstr(\"");
                if (lt->lexeme_len >= 2 && lt->lexeme[0] == '\"') {
                    const char* s = lt->lexeme + 1;
                    size_t n = lt->lexeme_len - 2;
                    cw_write_c_escaped(cw, s, n);
                }
                owl_cw_write(cw, "\")");
            } else if (rn && rn->kind == OWL_AST_EXPR_IDENTIFIER) {
                const OwlToken* rt = &toks[rn->as.expr_ident.name_tok];
                owl_cw_write(cw, "owl_str_copy(");
                owl_cw_writen(cw, rt->lexeme, rt->lexeme_len);
                owl_cw_write(cw, ")");
            } else {
                owl_cw_write(cw, "/* unsupported string assign */ owl_str_from_cstr(\"\")");
            }
            owl_cw_write(cw, ";");
            owl_cw_newline(cw);
        } else {
            /* primitive assignment */
            owl_cw_writen(cw, name_tok->lexeme, name_tok->lexeme_len);
            owl_cw_write(cw, " = ");
            OwlTypeInfo tmp;
            const char* expr_c = owl_emit_expr(cg, rhs, &tmp);
            /* enforce no promotion: types must match exactly */
            owl_tgen_require_same_type(cg, existing->type, tmp, 0);
            owl_cw_write(cw, expr_c);
            owl_cw_write(cw, ";");
            owl_cw_newline(cw);
        }
        free(name_c);
    }
}

void owl_emit_stmt_return(OwlCodegen* cg, OwlAstId stmt_id) {
    (void)stmt_id;
    /* TODO: emit `return <expr>;` or `return;` */
    owl_cw_write(&cg->writer, "/* TODO return */");
    owl_cw_newline(&cg->writer);
}

void owl_emit_stmt_if(OwlCodegen* cg, OwlAstId stmt_id) {
    (void)stmt_id;
    /* TODO: emit if(cond){...} else { ... } */
    owl_cw_write(&cg->writer, "/* TODO if */");
    owl_cw_newline(&cg->writer);
}

void owl_emit_stmt_loop(OwlCodegen* cg, OwlAstId stmt_id) {
    (void)stmt_id;
    /* TODO: emit loop cond body with entered flag for else */
    owl_cw_write(&cg->writer, "/* TODO loop */");
    owl_cw_newline(&cg->writer);
}

void owl_emit_stmt_block(OwlCodegen* cg, OwlAstId block_id) {
    (void)block_id;
    /* Skeleton:
       owl_cw_open_block(&cg->writer);
       owl_scope_push(&cg->scopes);
       ... emit inner statements ...
       owl_scope_pop(&cg->scopes, &cg->writer);
       owl_cw_close_block(&cg->writer);
    */
    owl_cw_open_block(&cg->writer);
    owl_scope_push(&cg->scopes);

    owl_cw_write(&cg->writer, "/* TODO block body */");
    owl_cw_newline(&cg->writer);

    owl_scope_pop(&cg->scopes, &cg->writer);
    owl_cw_close_block(&cg->writer);
}

void owl_emit_stmt_expr(OwlCodegen* cg, OwlAstId stmt_id) {
    const OwlAstNode* st = owl_ast_get_const(cg->ast, stmt_id);
    if (!st || st->kind != OWL_AST_STMT_EXPR) return;

    const OwlAstNode* expr = owl_ast_get_const(cg->ast, st->as.stmt_expr.expr);
    if (expr && expr->kind == OWL_AST_EXPR_CALL) {
        /* check callee is identifier 'print' */
        const OwlAstNode* callee = owl_ast_get_const(cg->ast, expr->as.expr_call.callee);
        if (callee && callee->kind == OWL_AST_EXPR_IDENTIFIER) {
            const OwlToken* toks = (const OwlToken*)cg->tokens;
            const OwlToken* ct = &toks[callee->as.expr_ident.name_tok];
            if (ct->lexeme_len == 5 && strncmp(ct->lexeme, "print", 5) == 0) {
                owl_emit_builtin_print(cg, st->as.stmt_expr.expr);
                return;
            }
        }
    }

    /* For other expressions, no-op in iteration 1 */
    owl_cw_write(&cg->writer, "/* expr stmt ignored */;");
    owl_cw_newline(&cg->writer);
}

/* ============================================================
 * Built-ins — print
 * ============================================================
 */

void owl_emit_builtin_print(OwlCodegen* cg, OwlAstId call_expr_id) {
    const OwlAstNode* call = owl_ast_get_const(cg->ast, call_expr_id);
    if (!call || call->kind != OWL_AST_EXPR_CALL) return;

    /* Ensure exactly one arg */
    if (call->as.expr_call.args.len != 1) {
        owl_tgen_errors_push(&cg->errors, OWL_TGEN_ERR_PRINT_ARG_COUNT, 0, "print expects exactly one argument");
        return;
    }
    OwlAstId arg = call->as.expr_call.args.items[0];
    OwlTypeInfo ty = owl_tgen_type_of_expr(cg, arg);
    const OwlToken* toks = (const OwlToken*)cg->tokens;
    OwlCodeWriter* cw = &cg->writer;

    switch (ty.kind) {
        case OWL_TY_STRING: {
            const OwlAstNode* an = owl_ast_get_const(cg->ast, arg);
            if (an && an->kind == OWL_AST_EXPR_LITERAL && an->as.expr_literal.flavor == OWL_LITERAL_STRING) {
                const char* tmp = owl_emit_string_literal_temp(cg, arg);
                owl_cw_write(cw, "owl_print_string(");
                owl_cw_write(cw, tmp);
                owl_cw_write(cw, ");");
                owl_cw_newline(cw);
                owl_emit_string_free(cg, tmp);
            } else if (an && an->kind == OWL_AST_EXPR_IDENTIFIER) {
                const OwlToken* nt = &toks[an->as.expr_ident.name_tok];
                owl_cw_write(cw, "owl_print_string(");
                owl_cw_writen(cw, nt->lexeme, nt->lexeme_len);
                owl_cw_write(cw, ");");
                owl_cw_newline(cw);
            } else {
                owl_tgen_errors_push(&cg->errors, OWL_TGEN_ERR_PRINT_ARG_TYPE, 0, "unsupported string print argument");
            }
            break;
        }
        case OWL_TY_INT: {
            OwlTypeInfo tmp;
            const char* ce = owl_emit_expr(cg, arg, &tmp);
            owl_cw_write(cw, "owl_print_int(");
            owl_cw_write(cw, ce);
            owl_cw_write(cw, ");");
            owl_cw_newline(cw);
            break;
        }
        case OWL_TY_FLOAT: {
            OwlTypeInfo tmp;
            const char* ce = owl_emit_expr(cg, arg, &tmp);
            owl_cw_write(cw, "owl_print_float(");
            owl_cw_write(cw, ce);
            owl_cw_write(cw, ");");
            owl_cw_newline(cw);
            break;
        }
        case OWL_TY_BOOL: {
            OwlTypeInfo tmp;
            const char* ce = owl_emit_expr(cg, arg, &tmp);
            owl_cw_write(cw, "owl_print_bool(");
            owl_cw_write(cw, ce);
            owl_cw_write(cw, ");");
            owl_cw_newline(cw);
            break;
        }
        case OWL_TY_BYTE: {
            OwlTypeInfo tmp;
            const char* ce = owl_emit_expr(cg, arg, &tmp);
            owl_cw_write(cw, "owl_print_byte(");
            owl_cw_write(cw, ce);
            owl_cw_write(cw, ");");
            owl_cw_newline(cw);
            break;
        }
        default:
            owl_tgen_errors_push(&cg->errors, OWL_TGEN_ERR_PRINT_ARG_TYPE, 0, "unsupported print argument type");
            break;
    }
}

/* ============================================================
 * Top-level program emission
 * ============================================================
 */

OwlTranspileResult owl_transpile_program(const OwlParseResult* pr, const OwlTranspileOptions* opts) {
    OwlTranspileResult out;
    memset(&out, 0, sizeof(out));
    owl_tgen_errors_init(&out.errors);

    OwlCodegen cg;
    owl_codegen_init(&cg, pr, opts);

    /* Prologue */
    owl_emit_prologue(&cg);

    /* Emit: int main(void) { ... } */
    OwlCodeWriter* cw = &cg.writer;
    owl_cw_write(cw, "int main(void) ");
    owl_cw_open_block(cw);
    owl_scope_push(&cg.scopes);

    /* Walk top-level program items and emit supported statements */
    const OwlAstNode* root = owl_ast_get_const(&pr->ast, pr->root);
    if (root && root->kind == OWL_AST_PROGRAM) {
        for (size_t i = 0; i < root->as.program.len; i++) {
            OwlTopLevelItem it = root->as.program.items[i];
            const OwlAstNode* n = owl_ast_get_const(&pr->ast, it.node);
            if (!n) continue;
            switch (n->kind) {
                case OWL_AST_STMT_VAR_OR_ASSIGN:
                    owl_emit_stmt_var_or_assign(&cg, it.node);
                    break;
                case OWL_AST_STMT_EXPR:
                    owl_emit_stmt_expr(&cg, it.node);
                    break;
                default:
                    /* ignore other top-level constructs in iteration 1 */
                    break;
            }
        }
    }

    /* scope cleanup before return */
    owl_scope_pop(&cg.scopes, cw);
    owl_cw_write(cw, "return 0;");
    owl_cw_newline(cw);
    owl_cw_close_block(cw);

    /* Finalize output buffer (NUL-terminate for convenience) */
    owl_cw_reserve(&cg.writer, 1);
    cg.writer.buf[cg.writer.len] = '\0';

    out.c_source_len = cg.writer.len;
    out.c_source = (char*)xmalloc(out.c_source_len + 1);
    memcpy(out.c_source, cg.writer.buf, out.c_source_len + 1);

    /* Transfer errors if any were collected during codegen (already in out.errors if pushed).
       (No additional move needed here.) */

    owl_codegen_free(&cg);
    return out;
}

void owl_transpile_result_free(OwlTranspileResult* out) {
    if (!out) return;
    if (out->c_source) {
        free(out->c_source);
        out->c_source = NULL;
        out->c_source_len = 0;
    }
    owl_tgen_errors_free(&out->errors);
}
