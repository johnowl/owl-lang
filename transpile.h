#ifndef OWL_TRANSPILE_H
#define OWL_TRANSPILE_H

/*
  Owl Transpiler — Iteration 1 Codegen Stub (Header)

  Scope:
  - Variables, assignments
  - Expressions: literals, identifiers, unary (+/-), binary ops (int-only, float-only), grouping
  - Built-in print calls: print(string|int|float|bool|byte)
  - String lifetime management:
      * Strings are heap-allocated (OwlString in owl_rt.h)
      * Strings declared in a scope are freed at the end of that scope
      * String reassignment frees the old value before assigning the new value
      * Temporaries created for printing string literals are freed immediately after the call

  No type promotion:
  - int ops are only on int32_t
  - float ops are only on float
  - Mixed numeric expressions (int with float) should cause a transpile-time error

  Note:
  - This is a stub header that defines the interfaces, core data structures,
    and behaviors for iteration-1 code generation.
  - A separate implementation file (e.g. transpile.c) should implement these functions.
*/

#include <stddef.h>
#include <stdint.h>
#include "parser.h"

/* Forward declarations of parser/AST structures (kept minimal to decouple headers):
   The transpiler does not own the AST; it reads nodes and emits C17 code.
   Consumers must include the actual parser.h to use these properly. */
typedef uint32_t OwlTokIdx;
typedef uint32_t OwlAstId;

//typedef struct OwlAst OwlAst;
//typedef struct OwlAstNode OwlAstNode;
//typedef struct OwlParseResult OwlParseResult;

/* ---------- Code Writer ---------- */

typedef struct {
    char*  buf;      /* owned output buffer */
    size_t len;      /* current length */
    size_t cap;      /* capacity */
    int    indent;   /* current indentation level */
    int    at_line_start; /* whether we are at the start of a line (for indentation) */
} OwlCodeWriter;

/* Initialize/cleanup a code writer */
void owl_cw_init(OwlCodeWriter* cw);
void owl_cw_free(OwlCodeWriter* cw);

/* Ensure capacity; internal helper */
void owl_cw_reserve(OwlCodeWriter* cw, size_t need);

/* Write raw bytes */
void owl_cw_write(OwlCodeWriter* cw, const char* s);
void owl_cw_writen(OwlCodeWriter* cw, const char* s, size_t n);

/* Write formatted small integers (for convenience) */
void owl_cw_write_int(OwlCodeWriter* cw, long v);

/* Indentation control */
void owl_cw_indent_inc(OwlCodeWriter* cw);
void owl_cw_indent_dec(OwlCodeWriter* cw);

/* Write newline and apply indentation on next write */
void owl_cw_newline(OwlCodeWriter* cw);

/* Begin/end a C block with braces and indentation */
void owl_cw_open_block(OwlCodeWriter* cw);   /* writes "{", newline, indent++ */
void owl_cw_close_block(OwlCodeWriter* cw);  /* indent--, writes "}", newline */

/* ---------- Diagnostics ---------- */

typedef enum {
    OWL_TGEN_OK = 0,
    OWL_TGEN_ERR_INTERNAL,
    OWL_TGEN_ERR_UNSUPPORTED_NODE,
    OWL_TGEN_ERR_TYPE_MISMATCH,
    OWL_TGEN_ERR_MIXED_NUMERIC,       /* int + float not allowed in iteration 1 */
    OWL_TGEN_ERR_UNKNOWN_IDENTIFIER,
    OWL_TGEN_ERR_PRINT_ARG_COUNT,     /* print must have exactly 1 argument */
    OWL_TGEN_ERR_PRINT_ARG_TYPE,      /* unsupported print overload */
    OWL_TGEN_ERR_UNSUPPORTED_EXPR,
    OWL_TGEN_ERR_UNSUPPORTED_STMT
} OwlTranspileErrorCode;

typedef struct {
    OwlTranspileErrorCode code;
    /* Optional: token index and a short message; for iteration-1 keep minimal */
    OwlTokIdx at_token;    /* 0 if unknown */
    const char* message;   /* may point to a static string */
} OwlTranspileError;

/* A simple error list (owned) */
typedef struct {
    OwlTranspileError* items;
    size_t len;
    size_t cap;
} OwlTranspileErrors;

/* Errors API */
void owl_tgen_errors_init(OwlTranspileErrors* errs);
void owl_tgen_errors_free(OwlTranspileErrors* errs);
void owl_tgen_errors_push(OwlTranspileErrors* errs, OwlTranspileErrorCode code, OwlTokIdx at_token, const char* message);

/* ---------- Type System (Iteration 1) ---------- */

typedef enum {
    OWL_TY_UNKNOWN = 0,
    OWL_TY_INT,        /* int32_t */
    OWL_TY_FLOAT,      /* float */
    OWL_TY_BOOL,       /* uint8_t */
    OWL_TY_BYTE,       /* uint8_t */
    OWL_TY_STRING      /* OwlString */
} OwlTypeKind;

/* Basic type info: in iteration 1 we only need the kind */
typedef struct {
    OwlTypeKind kind;
} OwlTypeInfo;

/* ---------- Symbol Table & Scope (Iteration 1) ---------- */

typedef struct {
    /* Symbol entry */
    const char* name;     /* pointer into token lexeme or an owned copy */
    OwlTypeInfo type;
    int needs_free;       /* 1 if this is a string variable that must be freed on scope exit */
} OwlSymbol;

typedef struct {
    OwlSymbol* symbols;
    size_t len;
    size_t cap;
} OwlScopeFrame;

typedef struct {
    OwlScopeFrame* frames;
    size_t depth;   /* number of frames (top is depth-1) */
    size_t cap;
} OwlScopeStack;

/* Scope API */
void owl_scope_init(OwlScopeStack* st);
void owl_scope_free(OwlScopeStack* st);
void owl_scope_push(OwlScopeStack* st);
void owl_scope_pop(OwlScopeStack* st, OwlCodeWriter* cw); /* Emits frees for strings declared in top frame */
int  owl_scope_declare(OwlScopeStack* st, const char* name, OwlTypeInfo type, int needs_free);
const OwlSymbol* owl_scope_lookup(const OwlScopeStack* st, const char* name);

/* ---------- Transpiler Options ---------- */

typedef struct {
    /* If non-zero, include source span comments for debugging emitted C code */
    int emit_span_comments;
    /* Future: choose string literal strategy (escaped C literal vs raw array) */
} OwlTranspileOptions;

/* ---------- Transpiler Result ---------- */

typedef struct {
    /* Emitted C source code; owned */
    char* c_source;
    size_t c_source_len;

    /* Accumulated errors; owned */
    OwlTranspileErrors errors;
} OwlTranspileResult;

/* Free transpile result (source + errors) */
void owl_transpile_result_free(OwlTranspileResult* out);

/* ---------- Built-in Mapping (Iteration 1) ---------- */

/* Map Owl built-in names to C runtime functions.
   For now, only 'print' overloads are supported. */
typedef struct {
    const char* owl_name;          /* e.g., "print" */
    /* Overload function names by type */
    const char* c_print_string;    /* "owl_print_string" */
    const char* c_print_int;       /* "owl_print_int" */
    const char* c_print_float;     /* "owl_print_float" */
    const char* c_print_bool;      /* "owl_print_bool" */
    const char* c_print_byte;      /* "owl_print_byte" */
} OwlBuiltinPrintMap;

static const OwlBuiltinPrintMap OWL_BUILTIN_PRINT = {
    "print", "owl_print_string", "owl_print_int", "owl_print_float", "owl_print_bool", "owl_print_byte"
};

/* ---------- Codegen Context ---------- */

typedef struct {
    /* Input AST (not owned) */
    const OwlAst* ast;
    const OwlAstNode* nodes;  /* convenience pointer to arena */
    size_t node_count;

    /* Token buffer (not owned) */
    const void* tokens;       /* opaque to keep header decoupled */
    size_t token_count;

    /* Options */
    OwlTranspileOptions opts;

    /* Output writer and scopes */
    OwlCodeWriter writer;
    OwlScopeStack scopes;

    /* Errors */
    OwlTranspileErrors errors;
} OwlCodegen;

/* Initialize/cleanup the codegen context */
void owl_codegen_init(OwlCodegen* cg, const OwlParseResult* pr, const OwlTranspileOptions* opts);
void owl_codegen_free(OwlCodegen* cg);

/* ---------- Type Checking Helpers (Iteration 1) ---------- */

/* Get the type of an expression node; returns OWL_TY_UNKNOWN on failure and emits an error. */
OwlTypeInfo owl_tgen_type_of_expr(OwlCodegen* cg, OwlAstId expr_id);

/* Ensure LHS and RHS types match exactly; if not, emit an error. No promotion. */
int owl_tgen_require_same_type(OwlCodegen* cg, OwlTypeInfo lhs, OwlTypeInfo rhs, OwlTokIdx at_token);

/* ---------- Emission Helpers ---------- */

/* Emit includes and prologue for iteration 1 */
void owl_emit_prologue(OwlCodegen* cg);

/* Emit a string literal as a temporary OwlString, returning the unique temp name.
   The caller should emit a free after use. */
const char* owl_emit_string_literal_temp(OwlCodegen* cg, OwlAstId expr_id);

/* Emit free for a string variable in current scope: owl_str_free(&name); */
void owl_emit_string_free(OwlCodegen* cg, const char* var_name);

/* ---------- Expressions ---------- */

/* Emit C code for an expression; returns a C expression string (owned by the writer or internal pool).
   For string literals, this does not return a stable "expression"; instead use owl_emit_string_literal_temp
   and pass the temp variable to functions or assignments. */
const char* owl_emit_expr(OwlCodegen* cg, OwlAstId expr_id, OwlTypeInfo* out_type);

/* ---------- Statements ---------- */

/* Emit variable declaration/assignment:
   - If symbol not in current scope, declare variable with mapped C type
   - If string RHS, allocate via owl_str_from_literal or copy
   - If reassigning a string, free old value first
   Semicolon is optional in Owl; C emission will include semicolons as needed. */
void owl_emit_stmt_var_or_assign(OwlCodegen* cg, OwlAstId stmt_id);

/* Emit return statement (iteration 1 may ignore if functions are deferred) */
void owl_emit_stmt_return(OwlCodegen* cg, OwlAstId stmt_id);

/* Emit if/else and loop (else) blocks — stubs in iteration 1 (optional) */
void owl_emit_stmt_if(OwlCodegen* cg, OwlAstId stmt_id);
void owl_emit_stmt_loop(OwlCodegen* cg, OwlAstId stmt_id);

/* Emit block: push scope, emit inner statements, pop scope and free strings */
void owl_emit_stmt_block(OwlCodegen* cg, OwlAstId block_id);

/* Emit expression statement (e.g., function call) */
void owl_emit_stmt_expr(OwlCodegen* cg, OwlAstId stmt_id);

/* ---------- Built-ins ---------- */

/* Emit a built-in print call: resolves overload by argument type and emits a call
   to the corresponding owl_rt.h function. For string literals, creates a temp string and frees after. */
void owl_emit_builtin_print(OwlCodegen* cg, OwlAstId call_expr_id);

/* ---------- Top-level ---------- */

/* Emit the entire program into out->c_source.
   Iteration 1 can emit an int main(void) that runs top-level statements and returns 0. */
OwlTranspileResult owl_transpile_program(const OwlParseResult* pr, const OwlTranspileOptions* opts);

#endif /* OWL_TRANSPILE_H */
