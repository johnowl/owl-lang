#ifndef OWL_LEXER_H
#define OWL_LEXER_H

/*
  Owl Lossless Lexer (C17)
  - Lossless: preserves whitespace, newlines, and comments as tokens
  - Suitable for building a concrete (lossless) syntax tree for formatting and refactoring
  - Does not perform semantic analysis; strictly tokenization

  Notes:
  - The lexer treats all spaces, tabs, and newlines as distinct tokens.
  - Line comments start with // and end at a newline (the newline is a separate NEWLINE token).
  - Block comments start with slash start and end with start slash (may span multiple lines).
  - String literals support simple escapes: \n, \t, \\, \"
  - Numeric literals: int (decimal), float (digits '.' digits)
  - Identifiers: [A-Za-z] [A-Za-z0-9_]*

  Keywords:
    if, else, loop, return, tuple, true, false
  Primitive Types:
    string, int, float, bool, byte

  Punctuators and operators (core set for Owl v0.3):
    ( ) { } [ ]
    , ; =
    -> || && == != < <= > >= + - * / %
    .  (reserved for future)
*/

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Source position metadata */
typedef struct {
    size_t index;    /* absolute byte index into the source buffer */
    size_t line;     /* 1-based line number */
    size_t column;   /* 1-based column number (bytes, not code points) */
} OwlPos;

typedef struct {
    OwlPos start;
    OwlPos end;      /* end is exclusive */
} OwlSpan;

/* Token kinds (lossless, includes whitespace and comments) */
typedef enum {
    OWL_TOK_EOF = 0,

    /* Lossless trivia */
    OWL_TOK_WHITESPACE,    // spaces/tabs (no newlines)
    OWL_TOK_NEWLINE,       // \n or \r\n represented as a single NEWLINE token
    OWL_TOK_LINE_COMMENT,  // //... up to, but not including, newline
    OWL_TOK_BLOCK_COMMENT, // /*...*/ may contain newlines

    /* Identifiers and keywords */
    OWL_TOK_IDENTIFIER,
    OWL_TOK_KW_IF,
    OWL_TOK_KW_ELSE,
    OWL_TOK_KW_LOOP,
    OWL_TOK_KW_RETURN,
    OWL_TOK_KW_TUPLE,
    /* bool literals as keywords */
    OWL_TOK_KW_TRUE,
    OWL_TOK_KW_FALSE,
    /* primitive types as keywords */
    OWL_TOK_KW_STRING,
    OWL_TOK_KW_INT,
    OWL_TOK_KW_FLOAT,
    OWL_TOK_KW_BOOL,
    OWL_TOK_KW_BYTE,

    /* Literals */
    OWL_TOK_STRING_LITERAL,
    OWL_TOK_INT_LITERAL,
    OWL_TOK_FLOAT_LITERAL,

    /* Punctuators */
    OWL_TOK_LPAREN,        /* ( */
    OWL_TOK_RPAREN,        /* ) */
    OWL_TOK_LBRACE,        /* { */
    OWL_TOK_RBRACE,        /* } */
    OWL_TOK_LBRACKET,      /* [ */
    OWL_TOK_RBRACKET,      /* ] */
    OWL_TOK_COMMA,         /* , */
    OWL_TOK_SEMICOLON,     /* ; */

    /* Operators */
    OWL_TOK_ASSIGN,        /* = */
    OWL_TOK_ARROW,         /* -> */
    OWL_TOK_OR_OR,         /* || */
    OWL_TOK_AND_AND,       /* && */
    OWL_TOK_EQ_EQ,         /* == */
    OWL_TOK_BANG_EQ,       /* != */
    OWL_TOK_LT,            /* < */
    OWL_TOK_LT_EQ,         /* <= */
    OWL_TOK_GT,            /* > */
    OWL_TOK_GT_EQ,         /* >= */
    OWL_TOK_PLUS,          /* + */
    OWL_TOK_MINUS,         /* - */
    OWL_TOK_STAR,          /* * */
    OWL_TOK_SLASH,         /* / */
    OWL_TOK_PERCENT,       /* % */
    OWL_TOK_DOT,           /* . (reserved/future use) */

    /* Error/unknown */
    OWL_TOK_UNKNOWN
} OwlTokenKind;

/* Token payload for lossless lexing */
typedef struct {
    OwlTokenKind kind;
    OwlSpan      span;

    /* Raw slice into the source buffer. Not null-terminated. */
    const char*  lexeme;
    size_t       lexeme_len;

    /* Decoded values (optional). For lossless operation, consumers can ignore these. */
    union {
        struct {
            int64_t value;       /* for OWL_TOK_INT_LITERAL (decimal) */
        } int_lit;
        struct {
            double value;        /* for OWL_TOK_FLOAT_LITERAL */
        } float_lit;
        struct {
            /* For strings, we keep raw lexeme; decoding can be deferred to parser */
            /* Consider storing an arena-allocated decoded buffer in a future revision */
            uint8_t has_escapes; /* quick flag: 1 if any backslash escape was seen */
        } string_lit;
    } as;
} OwlToken;

/* Lexer state */
typedef struct {
    const char* src;        /* source buffer */
    size_t      length;     /* source length in bytes */
    size_t      index;      /* current byte index */
    size_t      line;       /* current 1-based line (for next read) */
    size_t      column;     /* current 1-based column (for next read) */

    /* Lookahead buffer for one token (optional push-back) */
    OwlToken    lookahead;
    int         has_lookahead;
} OwlLexer;

/* Public API */

/* Initialize a lexer over the given source buffer */
void owl_lexer_init(OwlLexer* lx, const char* src, size_t length);

/*
  Obtain next token.
  - Returns a token including trivia (whitespace, newline, comments) for lossless processing.
  - Returns OWL_TOK_EOF when the end of input is reached.
*/
OwlToken owl_lexer_next(OwlLexer* lx);

/*
  Peek next token without consuming.
  - If a lookahead is not present, it lexes one and stores it.
*/
OwlToken owl_lexer_peek(OwlLexer* lx);

/*
  Consume a previously peeked token (equivalent to calling next if a lookahead exists).
*/
void owl_lexer_consume(OwlLexer* lx);

/*
  Utility: Convert a token kind to a human-readable string (for diagnostics).
*/
const char* owl_token_kind_name(OwlTokenKind kind);

/* Utility: Check if a token is a keyword (including primitive type keywords and boolean literals). */
int owl_token_is_keyword(OwlTokenKind kind);

/* Utility: Check if a token is trivia (whitespace, newline, or comments). */
int owl_token_is_trivia(OwlTokenKind kind);

/* Utility: Check if a token can start an identifier or keyword. */
int owl_is_ident_start(char c);

/* Utility: Check if a token can continue an identifier. */
int owl_is_ident_continue(char c);

/* Implementation notes (for the .c file, not this header):
   - Whitespace tokenization:
     * OWL_TOK_WHITESPACE: one or more ' ' or '\t'
     * OWL_TOK_NEWLINE: '\n' or '\r\n' (single token); '\r' alone should be treated as newline too if not followed by '\n'
   - Comments:
     * Line comment: starts with //, continues until (but not including) the next newline or EOF.
     * Block comment: starts with / * and ends with * /; if EOF occurs before closing, emit OWL_TOK_BLOCK_COMMENT with full span and let parser handle error if desired.
   - Strings:
     * Start/end with double quotes.
     * Allow escapes: \n, \t, \\, \"
     * Do not unescape in the lexer by default; set as.string_lit.has_escapes when any '\' appears before closing quote.
     * Unterminated strings: emit OWL_TOK_STRING_LITERAL with span to EOF; parser can flag error.
   - Numbers:
     * Float: digits '.' digits (no exponent in v0.3; may extend later).
     * Int: digits (decimal).
     * Leading zeros permitted; no sign in literal—sign is a separate OWL_TOK_PLUS/OWL_TOK_MINUS.
   - Identifiers and keywords:
     * [A-Za-z] [A-Za-z0-9_]*
     * Map exact matches to keyword kinds (if, else, loop, return, tuple, true, false, string, int, float, bool, byte).
   - Operators and punctuators:
     * Prefer longest match (e.g., -> before -, <= before <, etc.)
   - Lossless philosophy:
     * Emit every character as part of some token; never skip.
     * Unknown/invalid sequences should produce OWL_TOK_UNKNOWN covering at least one byte to ensure forward progress.
*/

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* OWL_LEXER_H */
