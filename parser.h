#ifndef OWL_PARSER_H
#define OWL_PARSER_H

/*
  Owl Parser — Initial AST and Public API (C17)

  Goals:
  - Lossless-friendly: every AST node tracks token index range [first_tok, last_tok_excl]
    so you can reconstruct the exact source between child nodes using the token stream
    emitted by the lexer.
  - Stable references: AST nodes are referred to by integer IDs (OwlAstId) rather than
    raw pointers, which simplifies arenas, relocation, and serialization.
  - Minimal, extensible shapes for declarations, statements, expressions, and types.

  This header exposes:
  - Core AST node kinds and shapes (unions for node payloads).
  - A compact AST container (OwlAst) with node storage.
  - Parse options, errors, and a consolidated parse result struct.
  - Public parse function(s) that produce a token buffer and an AST.

  Notes:
  - The parser consumes the entire token stream (including trivia). While AST nodes
    focus on significant structure, each node maintains [first_tok, last_tok_excl]
    over the token buffer so a lossless CST can be reconstructed or attached later.
  - Identifiers and literal values are not duplicated as strings in the AST; you
    should access the original lexeme by looking up the corresponding token in the
    token buffer via recorded token indexes.
*/

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "lexer.h"

/* --------- Common IDs and slices --------- */

typedef uint32_t OwlTokIdx;  /* Index into the token buffer returned by the parser */
typedef uint32_t OwlAstId;   /* Index/handle to an AST node in the AST arena */

#define OWL_AST_ID_NONE ((OwlAstId)0u)

/* A simple dynamic slice type for AST node IDs */
typedef struct {
    OwlAstId* items;
    size_t    len;
    size_t    cap;
} OwlAstIdSlice;

/* A simple dynamic slice for token indexes (useful for separators, etc.) */
typedef struct {
    OwlTokIdx* items;
    size_t     len;
    size_t     cap;
} OwlTokIdxSlice;

/* --------- AST Kinds --------- */

typedef enum {
    /* Root */
    OWL_AST_PROGRAM = 1,

    /* Declarations */
    OWL_AST_FUNC_DECL,
    OWL_AST_TUPLE_DECL,

    /* Types */
    OWL_AST_TYPE_NAME,        /* primitive or named tuple type */
    OWL_AST_TYPE_ARRAY,       /* [Type] */

    /* Statements */
    OWL_AST_STMT_BLOCK,
    OWL_AST_STMT_VAR_OR_ASSIGN,
    OWL_AST_STMT_RETURN,
    OWL_AST_STMT_IF,
    OWL_AST_STMT_LOOP,
    OWL_AST_STMT_EXPR,        /* expression statement (e.g., function call) */

    /* Expressions */
    OWL_AST_EXPR_IDENTIFIER,
    OWL_AST_EXPR_LITERAL,     /* int, float, string, true/false as token kind */
    OWL_AST_EXPR_UNARY,
    OWL_AST_EXPR_BINARY,
    OWL_AST_EXPR_CALL,
    OWL_AST_EXPR_INDEX,       /* a[i] */
    OWL_AST_EXPR_ARRAY,       /* [a, b, c] */
    OWL_AST_EXPR_GROUP,       /* (expr) */
    OWL_AST_EXPR_TUPLE_CTOR   /* User(...), positional or named args */
} OwlAstNodeKind;

/* Unary and binary operator “kinds” mirror token kinds for simplicity */
typedef OwlTokenKind OwlUnaryOp;
typedef OwlTokenKind OwlBinaryOp;

/* --------- AST Node Payloads --------- */

/* Type nodes */

typedef enum {
    OWL_TYPE_NAME_PRIMITIVE = 1, /* string, int, float, bool, byte (by token kind) */
    OWL_TYPE_NAME_NAMED          /* user-defined tuple type name (identifier token) */
} OwlTypeNameFlavor;

typedef struct {
    OwlTypeNameFlavor flavor;
    /* For primitive: store keyword token index; for named: identifier token index. */
    OwlTokIdx name_tok; /* OWL_TOK_KW_* for primitives or OWL_TOK_IDENTIFIER for named */
} OwlAstTypeName;

typedef struct {
    /* '[' Type ']' — element type node id, with bracket tokens retained */
    OwlTokIdx   lbracket_tok;
    OwlAstId    elem_type;
    OwlTokIdx   rbracket_tok;
} OwlAstTypeArray;

/* Declarations */

typedef struct {
    /* tuple Name '(' fields ')' */
    OwlTokIdx tuple_kw_tok;    /* 'tuple' */
    OwlTokIdx name_tok;        /* identifier token of tuple name */

    OwlTokIdx lparen_tok;
    /* Parallel arrays of field names and types; commas kept in separators if needed */
    OwlTokIdxSlice field_name_toks; /* each OWL_TOK_IDENTIFIER */
    OwlAstIdSlice  field_types;     /* each is a type node */
    OwlTokIdxSlice field_commas;    /* optional, len = (field count - 1) if present */
    OwlTokIdx rparen_tok;
} OwlAstTupleDecl;

typedef struct {
    /* name '(' param list ')' '->' return_type? block */
    OwlTokIdx name_tok;      /* function name (identifier) */
    OwlTokIdx lparen_tok;

    /* Parameters: names + type nodes */
    OwlTokIdxSlice param_name_toks; /* each OWL_TOK_IDENTIFIER */
    OwlAstIdSlice  param_types;     /* each is a type node */
    OwlTokIdxSlice param_commas;    /* commas between params */
    OwlTokIdx      rparen_tok;

    /* Optional return arrow and type */
    OwlTokIdx      arrow_tok;   /* OWL_TOK_ARROW or 0 if absent */
    OwlAstId       return_type; /* 0 if absent (procedure) */

    /* Body block node */
    OwlAstId       body_block;
} OwlAstFuncDecl;

/* Statements */

typedef struct {
    /* '{' stmts '}' */
    OwlTokIdx  lbrace_tok;
    OwlAstIdSlice stmts;   /* sequence of statement nodes */
    OwlTokIdx  rbrace_tok;
} OwlAstStmtBlock;

typedef struct {
    /* Unified declaration/assignment:
       name [TypeAnnotation]? '=' Expr ';'
       LHS optional type: a type node when explicit (e.g., [int], string, User, [User]) */
    OwlTokIdx  name_tok;     /* OWL_TOK_IDENTIFIER */
    OwlAstId   opt_type;     /* 0 if absent */
    OwlTokIdx  assign_tok;   /* '=' */
    OwlAstId   rhs_expr;
    OwlTokIdx  semi_tok;     /* ';' */
} OwlAstStmtVarOrAssign;

typedef struct {
    /* 'return' Expr? ';' */
    OwlTokIdx return_kw_tok;
    OwlAstId  opt_expr;  /* 0 if absent */
    OwlTokIdx semi_tok;  /* ';' */
} OwlAstStmtReturn;

typedef struct {
    /* if Cond Block (else Block)? */
    OwlTokIdx if_kw_tok;
    OwlAstId  cond_expr;
    OwlAstId  then_block;
    OwlTokIdx else_kw_tok; /* 0 if absent */
    OwlAstId  else_block;  /* 0 if absent */
} OwlAstStmtIf;

typedef struct {
    /* loop Cond Block (else Block)? */
    OwlTokIdx loop_kw_tok;
    OwlAstId  cond_expr;
    OwlAstId  body_block;
    OwlTokIdx else_kw_tok;   /* 0 if absent */
    OwlAstId  else_block;    /* 0 if absent */
} OwlAstStmtLoop;

typedef struct {
    /* Expression statement: Expr [';'] */
    OwlAstId  expr;
    OwlTokIdx semi_tok;  /* 0 if absent (semicolon optional) */
} OwlAstStmtExpr;

/* Expressions */

typedef struct {
    /* Identifier expression: just the name token */
    OwlTokIdx name_tok; /* OWL_TOK_IDENTIFIER */
} OwlAstExprIdentifier;

typedef enum {
    OWL_LITERAL_INT,
    OWL_LITERAL_FLOAT,
    OWL_LITERAL_STRING,
    OWL_LITERAL_BOOL /* true/false keywords */
} OwlLiteralFlavor;

typedef struct {
    OwlLiteralFlavor flavor;
    OwlTokIdx        tok; /* token carrying the literal text (int/float/string/true/false) */
} OwlAstExprLiteral;

typedef struct {
    OwlUnaryOp op_tok_kind; /* one of PLUS/MINUS/!; mirrors token kind */
    OwlTokIdx  op_tok;      /* token index of the operator */
    OwlAstId   operand;
} OwlAstExprUnary;

typedef struct {
    OwlAstId   left;
    OwlBinaryOp op_tok_kind; /* mirrors token kind (e.g., PLUS, STAR, EQ_EQ, etc.) */
    OwlTokIdx   op_tok;      /* token index of the operator */
    OwlAstId    right;
} OwlAstExprBinary;

typedef struct {
    /* callee '(' args ')' */
    OwlAstId     callee;
    OwlTokIdx    lparen_tok;
    OwlAstIdSlice args;          /* positional args only (for now) */
    OwlTokIdxSlice arg_commas;   /* commas between args */
    OwlTokIdx    rparen_tok;
} OwlAstExprCall;

typedef struct {
    /* target '[' index ']' */
    OwlAstId  target;
    OwlTokIdx lbracket_tok;
    OwlAstId  index_expr;
    OwlTokIdx rbracket_tok;
} OwlAstExprIndex;

typedef struct {
    /* '[' elements ']' */
    OwlTokIdx    lbracket_tok;
    OwlAstIdSlice elements;
    OwlTokIdxSlice element_commas;
    OwlTokIdx    rbracket_tok;
} OwlAstExprArray;

typedef struct {
    /* '(' expr ')' */
    OwlTokIdx lparen_tok;
    OwlAstId  inner;
    OwlTokIdx rparen_tok;
} OwlAstExprGroup;

typedef struct {
    /* Tuple construction:
       Name '(' (positional args | named args) ')'
       Named args: field '=' expr, order independent. */
    OwlTokIdx name_tok;       /* tuple type name (identifier) */
    OwlTokIdx lparen_tok;

    /* Positional args (len > 0) OR named args (len_named > 0). Not mixed initially. */
    OwlAstIdSlice positional_args;
    OwlTokIdxSlice positional_commas;

    /* Named args */
    OwlTokIdxSlice named_field_toks; /* each is OWL_TOK_IDENTIFIER (field name) */
    OwlTokIdxSlice named_eq_toks;    /* '=' tokens corresponding to fields */
    OwlAstIdSlice  named_values;     /* expr nodes for each named field */
    OwlTokIdxSlice named_commas;

    OwlTokIdx rparen_tok;
} OwlAstExprTupleCtor;

/* --------- Program and Top-level --------- */

typedef enum {
    OWL_TOP_FUNC = 1,
    OWL_TOP_TUPLE,
    OWL_TOP_STMT
} OwlTopLevelKind;

typedef struct {
    OwlTopLevelKind kind;
    OwlAstId        node; /* node id of the corresponding AST node */
} OwlTopLevelItem;

typedef struct {
    /* Root program: sequence of top-level items */
    OwlTopLevelItem* items;
    size_t           len;
    size_t           cap;
} OwlAstProgram;

/* --------- Generic AST Node --------- */

typedef struct {
    OwlAstNodeKind kind;

    /* Lossless token coverage of this node:
       tokens[first_tok .. last_tok_excl) fully covers the node (including trivia). */
    OwlTokIdx first_tok;
    OwlTokIdx last_tok_excl;

    /* Semantic span (optional, may mirror token coverage); can be used for diagnostics */
    OwlSpan   span;

    /* Payload */
    union {
        /* Types */
        OwlAstTypeName    type_name;
        OwlAstTypeArray   type_array;

        /* Decls */
        OwlAstTupleDecl   tuple_decl;
        OwlAstFuncDecl    func_decl;

        /* Stmts */
        OwlAstStmtBlock        stmt_block;
        OwlAstStmtVarOrAssign  stmt_var_or_assign;
        OwlAstStmtReturn       stmt_return;
        OwlAstStmtIf           stmt_if;
        OwlAstStmtLoop         stmt_loop;
        OwlAstStmtExpr         stmt_expr;

        /* Exprs */
        OwlAstExprIdentifier   expr_ident;
        OwlAstExprLiteral      expr_literal;
        OwlAstExprUnary        expr_unary;
        OwlAstExprBinary       expr_binary;
        OwlAstExprCall         expr_call;
        OwlAstExprIndex        expr_index;
        OwlAstExprArray        expr_array;
        OwlAstExprGroup        expr_group;
        OwlAstExprTupleCtor    expr_tuple_ctor;

        /* Program */
        OwlAstProgram          program;
    } as;
} OwlAstNode;

/* --------- AST Arena/Container --------- */

typedef struct {
    OwlAstNode* nodes;
    size_t      len;
    size_t      cap;
} OwlAst;

/* --------- Parse diagnostics --------- */

typedef enum {
    OWL_PARSE_OK = 0,
    OWL_PARSE_ERR_UNEXPECTED_TOKEN,
    OWL_PARSE_ERR_UNTERMINATED_STRING,
    OWL_PARSE_ERR_UNTERMINATED_BLOCK_COMMENT,
    OWL_PARSE_ERR_EXPECTED_IDENTIFIER,
    OWL_PARSE_ERR_EXPECTED_TYPE,
    OWL_PARSE_ERR_EXPECTED_EXPR,
    OWL_PARSE_ERR_EXPECTED_STMT,
    OWL_PARSE_ERR_EXPECTED_RPAREN,
    OWL_PARSE_ERR_EXPECTED_RBRACE,
    OWL_PARSE_ERR_EXPECTED_RBRACKET,
    OWL_PARSE_ERR_EXPECTED_ASSIGN,
    OWL_PARSE_ERR_EXPECTED_SEMICOLON,
    OWL_PARSE_ERR_MIXED_TUPLE_ARGS,          /* mixing positional and named in tuple ctor */
    OWL_PARSE_ERR_UNKNOWN
} OwlParseErrorCode;

typedef struct {
    OwlParseErrorCode code;
    OwlSpan           span;       /* where the error occurred */
    OwlTokIdx         at_token;   /* token index near the error */
} OwlParseError;

typedef struct {
    /* Whether the parser should continue after errors (attempting error recovery). */
    int continue_after_error; /* default: 1 */
} OwlParserOptions;

/* --------- Parse result --------- */

typedef struct {
    /* Source retained for reference (not owned; pass your buffer, keep it alive while using result). */
    const char* source;
    size_t      source_len;

    /* Token stream (owned by the result; free via owl_parse_free). */
    OwlToken*   tokens;
    size_t      token_count;

    /* AST arena (owned by the result). Node 1 (index=1) is typically the Program node. */
    OwlAst      ast;
    OwlAstId    root; /* Program node id */

    /* Diagnostics */
    OwlParseError* errors;
    size_t         error_count;
} OwlParseResult;

/* --------- Public API --------- */

/*
  Parse a full Owl program (lossless-friendly).

  - The function lexes the source into a token buffer (including trivia),
    then parses into an AST with token index ranges on every node.
  - The returned OwlParseResult owns the token buffer, AST storage,
    and error array. Call owl_parse_free to release resources.

  options may be NULL to use defaults.
*/
OwlParseResult owl_parse(const char* source, size_t source_len, const OwlParserOptions* options);

/* Free all memory owned by a parse result (tokens, AST nodes, errors). */
void owl_parse_free(OwlParseResult* result);

/* Utility: get a node pointer by id (returns NULL if id is invalid). */
static inline const OwlAstNode* owl_ast_get_const(const OwlAst* ast, OwlAstId id) {
    if (!ast || id == 0 || id > ast->len) return NULL;
    return &ast->nodes[id - 1];
}
static inline OwlAstNode* owl_ast_get(OwlAst* ast, OwlAstId id) {
    if (!ast || id == 0 || id > ast->len) return NULL;
    return &ast->nodes[id - 1];
}

/* Convenience helpers: determine literal/identifier text via token buffer. */

/* Returns token pointer for a given token index, or NULL if out of range. */
static inline const OwlToken* owl_tokens_get_const(const OwlToken* tokens, size_t token_count, OwlTokIdx idx) {
    if (!tokens || idx >= token_count) return NULL;
    return &tokens[idx];
}

/* A tiny helper to check if a node covers a given token index. */
static inline int owl_node_covers_token(const OwlAstNode* node, OwlTokIdx tok_idx) {
    return node && (tok_idx >= node->first_tok) && (tok_idx < node->last_tok_excl);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* OWL_PARSER_H */
