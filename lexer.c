/*
  Owl Lossless Lexer (C17) — Implementation
  File: c/owl/lexer.c

  This lexer produces a lossless token stream including whitespace, newlines,
  and comments. It matches the API and token kinds described in lexer.h.
*/

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"

/* --------------------------
   Internal helpers/macros
   -------------------------- */

#define OWL_MIN(a, b) ((a) < (b) ? (a) : (b))
#define OWL_MAX(a, b) ((a) > (b) ? (a) : (b))

static inline int owl_is_ident_start_impl(char c) {
    return (c >= 'A' && c <= 'Z') ||
           (c >= 'a' && c <= 'z') ||
           (c == '_'); /* allow leading '_' as identifier start for generality */
}

static inline int owl_is_ident_continue_impl(char c) {
    return owl_is_ident_start_impl(c) || (c >= '0' && c <= '9');
}

int owl_is_ident_start(char c) { return owl_is_ident_start_impl(c); }
int owl_is_ident_continue(char c) { return owl_is_ident_continue_impl(c); }

static OwlPos make_pos(size_t index, size_t line, size_t column) {
    OwlPos p;
    p.index = index;
    p.line = line;
    p.column = column;
    return p;
}

static OwlSpan make_span(OwlPos start, OwlPos end) {
    OwlSpan s;
    s.start = start;
    s.end = end;
    return s;
}

static OwlToken make_token(OwlTokenKind kind, OwlSpan span, const char* src) {
    OwlToken t;
    t.kind = kind;
    t.span = span;
    t.lexeme = src + span.start.index;
    t.lexeme_len = span.end.index - span.start.index;
    /* Zero payload union by default */
    memset(&t.as, 0, sizeof(t.as));
    return t;
}

/* --------------------------
   Public utility functions
   -------------------------- */

const char* owl_token_kind_name(OwlTokenKind kind) {
    switch (kind) {
        case OWL_TOK_EOF: return "EOF";
        case OWL_TOK_WHITESPACE: return "WHITESPACE";
        case OWL_TOK_NEWLINE: return "NEWLINE";
        case OWL_TOK_LINE_COMMENT: return "LINE_COMMENT";
        case OWL_TOK_BLOCK_COMMENT: return "BLOCK_COMMENT";
        case OWL_TOK_IDENTIFIER: return "IDENTIFIER";
        case OWL_TOK_KW_IF: return "KW_IF";
        case OWL_TOK_KW_ELSE: return "KW_ELSE";
        case OWL_TOK_KW_LOOP: return "KW_LOOP";
        case OWL_TOK_KW_RETURN: return "KW_RETURN";
        case OWL_TOK_KW_TUPLE: return "KW_TUPLE";
        case OWL_TOK_KW_TRUE: return "KW_TRUE";
        case OWL_TOK_KW_FALSE: return "KW_FALSE";
        case OWL_TOK_KW_STRING: return "KW_STRING";
        case OWL_TOK_KW_INT: return "KW_INT";
        case OWL_TOK_KW_FLOAT: return "KW_FLOAT";
        case OWL_TOK_KW_BOOL: return "KW_BOOL";
        case OWL_TOK_KW_BYTE: return "KW_BYTE";
        case OWL_TOK_STRING_LITERAL: return "STRING_LITERAL";
        case OWL_TOK_INT_LITERAL: return "INT_LITERAL";
        case OWL_TOK_FLOAT_LITERAL: return "FLOAT_LITERAL";
        case OWL_TOK_LPAREN: return "LPAREN";
        case OWL_TOK_RPAREN: return "RPAREN";
        case OWL_TOK_LBRACE: return "LBRACE";
        case OWL_TOK_RBRACE: return "RBRACE";
        case OWL_TOK_LBRACKET: return "LBRACKET";
        case OWL_TOK_RBRACKET: return "RBRACKET";
        case OWL_TOK_COMMA: return "COMMA";
        case OWL_TOK_SEMICOLON: return "SEMICOLON";
        case OWL_TOK_ASSIGN: return "ASSIGN";
        case OWL_TOK_ARROW: return "ARROW";
        case OWL_TOK_OR_OR: return "OR_OR";
        case OWL_TOK_AND_AND: return "AND_AND";
        case OWL_TOK_EQ_EQ: return "EQ_EQ";
        case OWL_TOK_BANG_EQ: return "BANG_EQ";
        case OWL_TOK_LT: return "LT";
        case OWL_TOK_LT_EQ: return "LT_EQ";
        case OWL_TOK_GT: return "GT";
        case OWL_TOK_GT_EQ: return "GT_EQ";
        case OWL_TOK_PLUS: return "PLUS";
        case OWL_TOK_MINUS: return "MINUS";
        case OWL_TOK_STAR: return "STAR";
        case OWL_TOK_SLASH: return "SLASH";
        case OWL_TOK_PERCENT: return "PERCENT";
        case OWL_TOK_DOT: return "DOT";
        case OWL_TOK_UNKNOWN: return "UNKNOWN";
    }
    return "UNKNOWN";
}

int owl_token_is_keyword(OwlTokenKind kind) {
    switch (kind) {
        case OWL_TOK_KW_IF:
        case OWL_TOK_KW_ELSE:
        case OWL_TOK_KW_LOOP:
        case OWL_TOK_KW_RETURN:
        case OWL_TOK_KW_TUPLE:
        case OWL_TOK_KW_TRUE:
        case OWL_TOK_KW_FALSE:
        case OWL_TOK_KW_STRING:
        case OWL_TOK_KW_INT:
        case OWL_TOK_KW_FLOAT:
        case OWL_TOK_KW_BOOL:
        case OWL_TOK_KW_BYTE:
            return 1;
        default:
            return 0;
    }
}

int owl_token_is_trivia(OwlTokenKind kind) {
    switch (kind) {
        case OWL_TOK_WHITESPACE:
        case OWL_TOK_NEWLINE:
        case OWL_TOK_LINE_COMMENT:
        case OWL_TOK_BLOCK_COMMENT:
            return 1;
        default:
            return 0;
    }
}

/* --------------------------
   Lexer core
   -------------------------- */

void owl_lexer_init(OwlLexer* lx, const char* src, size_t length) {
    lx->src = src;
    lx->length = length;
    lx->index = 0;
    lx->line = 1;
    lx->column = 1;
    lx->has_lookahead = 0;
    memset(&lx->lookahead, 0, sizeof(lx->lookahead));
}

static int eof(const OwlLexer* lx) {
    return lx->index >= lx->length;
}

static char peek_char(const OwlLexer* lx, size_t lookahead) {
    size_t i = lx->index + lookahead;
    if (i >= lx->length) return '\0';
    return lx->src[i];
}

static char cur_char(const OwlLexer* lx) {
    return peek_char(lx, 0);
}

static void advance(OwlLexer* lx, size_t n) {
    for (size_t i = 0; i < n && lx->index < lx->length; i++) {
        char c = lx->src[lx->index++];
        if (c == '\r') {
            /* If CRLF, treat as single newline token later.
               Here we still track position accurately: column resets, line increments when we finalize newline token. */
            lx->column += 1;
        } else if (c == '\n') {
            lx->line += 1;
            lx->column = 1;
        } else {
            lx->column += 1;
        }
    }
}

static OwlToken emit_simple(OwlLexer* lx, OwlTokenKind kind, size_t start_index, size_t start_line, size_t start_column, size_t end_index, size_t end_line, size_t end_column) {
    OwlSpan span = make_span(make_pos(start_index, start_line, start_column), make_pos(end_index, end_line, end_column));
    return make_token(kind, span, lx->src);
}

/* Newline tokenization: CRLF as one NEWLINE token, CR alone as NEWLINE, LF alone as NEWLINE */
static OwlToken lex_newline(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;

    char c0 = peek_char(lx, 0);
    char c1 = peek_char(lx, 1);

    if (c0 == '\r' && c1 == '\n') {
        advance(lx, 2);
        /* After consuming CRLF, line increments once, column set to 1 by advance's LF handling */
        size_t ei = lx->index;
        size_t el = lx->line;
        size_t ec = lx->column;
        return emit_simple(lx, OWL_TOK_NEWLINE, si, sl, sc, ei, el, ec);
    } else if (c0 == '\r' || c0 == '\n') {
        advance(lx, 1);
        size_t ei = lx->index;
        size_t el = lx->line;
        size_t ec = lx->column;
        return emit_simple(lx, OWL_TOK_NEWLINE, si, sl, sc, ei, el, ec);
    }

    /* Not a newline; caller should not have invoked this. */
    advance(lx, 1);
    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;
    return emit_simple(lx, OWL_TOK_UNKNOWN, si, sl, sc, ei, el, ec);
}

/* Lex whitespace (space/tab) as a single WHITESPACE token; does not include newlines */
static OwlToken lex_whitespace(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;
    while (!eof(lx)) {
        char c = cur_char(lx);
        if (c == ' ' || c == '\t') advance(lx, 1);
        else break;
    }
    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;
    return emit_simple(lx, OWL_TOK_WHITESPACE, si, sl, sc, ei, el, ec);
}

/* Lex line comment: //... until newline or EOF.
   The newline itself will be a separate NEWLINE token. */
static OwlToken lex_line_comment(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;
    /* consume '//' */
    advance(lx, 2);
    while (!eof(lx)) {
        char c = cur_char(lx);
        if (c == '\r' || c == '\n') break;
        advance(lx, 1);
    }
    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;
    return emit_simple(lx, OWL_TOK_LINE_COMMENT, si, sl, sc, ei, el, ec);
}

/* Lex block comment: / * ... * / (without space)
   If EOF before closing, still emit BLOCK_COMMENT (parser may flag error later). */
static OwlToken lex_block_comment(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;
    /* consume '/ *' */
    advance(lx, 2);
    while (!eof(lx)) {
        char c0 = peek_char(lx, 0);
        char c1 = peek_char(lx, 1);
        if (c0 == '*' && c1 == '/') {
            advance(lx, 2);
            break;
        }
        advance(lx, 1);
    }
    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;
    return emit_simple(lx, OWL_TOK_BLOCK_COMMENT, si, sl, sc, ei, el, ec);
}

/* String literal: " ... " with escapes \n, \t, \\, \" */
static OwlToken lex_string(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;

    int has_escapes = 0;
    advance(lx, 1); /* consume opening quote */

    while (!eof(lx)) {
        char c = cur_char(lx);
        if (c == '"') {
            advance(lx, 1);
            break;
        } else if (c == '\\') {
            has_escapes = 1;
            advance(lx, 1);
            if (!eof(lx)) {
                /* consume escaped char; we don't validate fully here */
                advance(lx, 1);
            }
        } else {
            /* Consume regular char; newline inside string is allowed but unusual.
               Keep it lossless anyway; if newline occurs before closing quote,
               the parser can decide it's an error. */
            advance(lx, 1);
        }
    }

    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;
    OwlToken t = emit_simple(lx, OWL_TOK_STRING_LITERAL, si, sl, sc, ei, el, ec);
    t.as.string_lit.has_escapes = has_escapes ? 1 : 0;
    return t;
}

/* Numeric literals: int (digits), float (digits '.' digits).
   No exponent for now; sign handled as separate PLUS/MINUS tokens. */
static OwlToken lex_number(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;

    int saw_dot = 0;
    size_t int_part_len = 0;
    size_t frac_part_len = 0;

    /* Consume leading digits */
    while (!eof(lx)) {
        char c = cur_char(lx);
        if (c >= '0' && c <= '9') {
            int_part_len++;
            advance(lx, 1);
        } else {
            break;
        }
    }

    /* Check for '.' followed by at least one digit to form a float */
    if (!eof(lx) && cur_char(lx) == '.' && (peek_char(lx, 1) >= '0' && peek_char(lx, 1) <= '9')) {
        saw_dot = 1;
        advance(lx, 1); /* consume '.' */
        while (!eof(lx)) {
            char c = cur_char(lx);
            if (c >= '0' && c <= '9') {
                frac_part_len++;
                advance(lx, 1);
            } else {
                break;
            }
        }
    }

    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;

    if (saw_dot) {
        OwlToken t = emit_simple(lx, OWL_TOK_FLOAT_LITERAL, si, sl, sc, ei, el, ec);
        /* Best-effort decode to double */
        char buf[64];
        size_t len = OWL_MIN(sizeof(buf) - 1, ei - si);
        memcpy(buf, lx->src + si, len);
        buf[len] = '\0';
        t.as.float_lit.value = strtod(buf, NULL);
        return t;
    } else {
        OwlToken t = emit_simple(lx, OWL_TOK_INT_LITERAL, si, sl, sc, ei, el, ec);
        /* Best-effort decode to int64 */
        int64_t value = 0;
        for (size_t i = si; i < ei; i++) {
            char c = lx->src[i];
            if (c >= '0' && c <= '9') {
                value = value * 10 + (int64_t)(c - '0');
            } else {
                break;
            }
        }
        t.as.int_lit.value = value;
        return t;
    }
}

/* Identifiers and keywords */
static OwlTokenKind keyword_kind(const char* s, size_t len) {
    /* Compare string literal keywords with exact match */
    #define MATCHKW(lit, kind) if (len == sizeof(lit)-1 && memcmp(s, lit, sizeof(lit)-1) == 0) return kind

    MATCHKW("if", OWL_TOK_KW_IF);
    MATCHKW("else", OWL_TOK_KW_ELSE);
    MATCHKW("loop", OWL_TOK_KW_LOOP);
    MATCHKW("return", OWL_TOK_KW_RETURN);
    MATCHKW("tuple", OWL_TOK_KW_TUPLE);

    MATCHKW("true", OWL_TOK_KW_TRUE);
    MATCHKW("false", OWL_TOK_KW_FALSE);

    MATCHKW("string", OWL_TOK_KW_STRING);
    MATCHKW("int", OWL_TOK_KW_INT);
    MATCHKW("float", OWL_TOK_KW_FLOAT);
    MATCHKW("bool", OWL_TOK_KW_BOOL);
    MATCHKW("byte", OWL_TOK_KW_BYTE);

    return OWL_TOK_IDENTIFIER;
    #undef MATCHKW
}

static OwlToken lex_identifier_or_keyword(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;
    advance(lx, 1); /* consume first char */
    while (!eof(lx) && owl_is_ident_continue_impl(cur_char(lx))) {
        advance(lx, 1);
    }
    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;

    OwlTokenKind k = keyword_kind(lx->src + si, ei - si);
    OwlToken t = emit_simple(lx, k, si, sl, sc, ei, el, ec);
    return t;
}

/* Operators and punctuators with longest-match preference */
static OwlToken lex_operator_or_punct(OwlLexer* lx) {
    size_t si = lx->index;
    size_t sl = lx->line;
    size_t sc = lx->column;

    char c0 = peek_char(lx, 0);
    char c1 = peek_char(lx, 1);

    /* Two-character operators with priority */
    if (c0 == '-' && c1 == '>') {
        advance(lx, 2);
        size_t ei = lx->index, el = lx->line, ec = lx->column;
        return emit_simple(lx, OWL_TOK_ARROW, si, sl, sc, ei, el, ec);
    }
    if (c0 == '|' && c1 == '|') {
        advance(lx, 2);
        size_t ei = lx->index, el = lx->line, ec = lx->column;
        return emit_simple(lx, OWL_TOK_OR_OR, si, sl, sc, ei, el, ec);
    }
    if (c0 == '&' && c1 == '&') {
        advance(lx, 2);
        size_t ei = lx->index, el = lx->line, ec = lx->column;
        return emit_simple(lx, OWL_TOK_AND_AND, si, sl, sc, ei, el, ec);
    }
    if (c0 == '=' && c1 == '=') {
        advance(lx, 2);
        size_t ei = lx->index, el = lx->line, ec = lx->column;
        return emit_simple(lx, OWL_TOK_EQ_EQ, si, sl, sc, ei, el, ec);
    }
    if (c0 == '!' && c1 == '=') {
        advance(lx, 2);
        size_t ei = lx->index, el = lx->line, ec = lx->column;
        return emit_simple(lx, OWL_TOK_BANG_EQ, si, sl, sc, ei, el, ec);
    }
    if (c0 == '<' && c1 == '=') {
        advance(lx, 2);
        size_t ei = lx->index, el = lx->line, ec = lx->column;
        return emit_simple(lx, OWL_TOK_LT_EQ, si, sl, sc, ei, el, ec);
    }
    if (c0 == '>' && c1 == '=') {
        advance(lx, 2);
        size_t ei = lx->index, el = lx->line, ec = lx->column;
        return emit_simple(lx, OWL_TOK_GT_EQ, si, sl, sc, ei, el, ec);
    }

    /* Single-character tokens */
    switch (c0) {
        case '(': advance(lx, 1); return emit_simple(lx, OWL_TOK_LPAREN, si, sl, sc, lx->index, lx->line, lx->column);
        case ')': advance(lx, 1); return emit_simple(lx, OWL_TOK_RPAREN, si, sl, sc, lx->index, lx->line, lx->column);
        case '{': advance(lx, 1); return emit_simple(lx, OWL_TOK_LBRACE, si, sl, sc, lx->index, lx->line, lx->column);
        case '}': advance(lx, 1); return emit_simple(lx, OWL_TOK_RBRACE, si, sl, sc, lx->index, lx->line, lx->column);
        case '[': advance(lx, 1); return emit_simple(lx, OWL_TOK_LBRACKET, si, sl, sc, lx->index, lx->line, lx->column);
        case ']': advance(lx, 1); return emit_simple(lx, OWL_TOK_RBRACKET, si, sl, sc, lx->index, lx->line, lx->column);
        case ',': advance(lx, 1); return emit_simple(lx, OWL_TOK_COMMA, si, sl, sc, lx->index, lx->line, lx->column);
        case ';': advance(lx, 1); return emit_simple(lx, OWL_TOK_SEMICOLON, si, sl, sc, lx->index, lx->line, lx->column);
        case '=': advance(lx, 1); return emit_simple(lx, OWL_TOK_ASSIGN, si, sl, sc, lx->index, lx->line, lx->column);
        case '<': advance(lx, 1); return emit_simple(lx, OWL_TOK_LT, si, sl, sc, lx->index, lx->line, lx->column);
        case '>': advance(lx, 1); return emit_simple(lx, OWL_TOK_GT, si, sl, sc, lx->index, lx->line, lx->column);
        case '+': advance(lx, 1); return emit_simple(lx, OWL_TOK_PLUS, si, sl, sc, lx->index, lx->line, lx->column);
        case '-': advance(lx, 1); return emit_simple(lx, OWL_TOK_MINUS, si, sl, sc, lx->index, lx->line, lx->column);
        case '*': advance(lx, 1); return emit_simple(lx, OWL_TOK_STAR, si, sl, sc, lx->index, lx->line, lx->column);
        case '/': advance(lx, 1); return emit_simple(lx, OWL_TOK_SLASH, si, sl, sc, lx->index, lx->line, lx->column);
        case '%': advance(lx, 1); return emit_simple(lx, OWL_TOK_PERCENT, si, sl, sc, lx->index, lx->line, lx->column);
        case '.': advance(lx, 1); return emit_simple(lx, OWL_TOK_DOT, si, sl, sc, lx->index, lx->line, lx->column);
        default: break;
    }

    /* Unknown single char */
    advance(lx, 1);
    size_t ei = lx->index;
    size_t el = lx->line;
    size_t ec = lx->column;
    return emit_simple(lx, OWL_TOK_UNKNOWN, si, sl, sc, ei, el, ec);
}

/* --------------------------
   Public lexing API
   -------------------------- */

OwlToken owl_lexer_next(OwlLexer* lx) {
    if (lx->has_lookahead) {
        lx->has_lookahead = 0;
        return lx->lookahead;
    }

    if (eof(lx)) {
        OwlSpan span = make_span(make_pos(lx->index, lx->line, lx->column),
                                 make_pos(lx->index, lx->line, lx->column));
        return make_token(OWL_TOK_EOF, span, lx->src);
    }

    /* Newline is its own token */
    char c = cur_char(lx);
    if (c == '\r' || c == '\n') {
        return lex_newline(lx);
    }

    /* Whitespace (space, tab) token */
    if (c == ' ' || c == '\t') {
        return lex_whitespace(lx);
    }

    /* Comments */
    if (c == '/' && peek_char(lx, 1) == '/') {
        return lex_line_comment(lx);
    }
    if (c == '/' && peek_char(lx, 1) == '*') {
        return lex_block_comment(lx);
    }

    /* String literal */
    if (c == '"') {
        return lex_string(lx);
    }

    /* Number literal (starting with digit) */
    if (c >= '0' && c <= '9') {
        return lex_number(lx);
    }

    /* Identifier or keyword */
    if (owl_is_ident_start_impl(c)) {
        return lex_identifier_or_keyword(lx);
    }

    /* Operators and punctuators */
    return lex_operator_or_punct(lx);
}

OwlToken owl_lexer_peek(OwlLexer* lx) {
    if (!lx->has_lookahead) {
        lx->lookahead = owl_lexer_next(lx);
        lx->has_lookahead = 1;
    }
    return lx->lookahead;
}

void owl_lexer_consume(OwlLexer* lx) {
    if (lx->has_lookahead) {
        lx->has_lookahead = 0;
    } else {
        (void)owl_lexer_next(lx);
    }
}
