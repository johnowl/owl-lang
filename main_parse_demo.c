/*
  Owl Parser Demo (C17)
  File: c/owl-lang/main_parse_demo.c

  This demo:
    - Reads Owl source from a file (argv[1]) or stdin.
    - Parses it into a full-token stream + AST (lossless-friendly).
    - Prints a brief, human-readable AST summary with key tokens and structure.
    - Prints diagnostics (if any), then frees all resources.

  Build note:
    Update your Makefile to compile and link this file with lexer.c and parser.c, e.g.:
      SRC := lexer.c parser.c main_parse_demo.c

  Note:
    This demo now recognizes expression statements (like function calls) and prints them,
    so calls won’t appear as UNKNOWN_NODE. It uses the new OWL_AST_STMT_EXPR node kind.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"

/* --------- IO helpers --------- */

static char* read_file(const char* path, size_t* out_len) {
    FILE* f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "Failed to open file: %s\n", path);
        return NULL;
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        fprintf(stderr, "Failed to seek file: %s\n", path);
        fclose(f);
        return NULL;
    }
    long sz = ftell(f);
    if (sz < 0) {
        fprintf(stderr, "Failed to tell size for file: %s\n", path);
        fclose(f);
        return NULL;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Failed to rewind file: %s\n", path);
        fclose(f);
        return NULL;
    }
    char* buf = (char*)malloc((size_t)sz);
    if (!buf) {
        fprintf(stderr, "Out of memory\n");
        fclose(f);
        return NULL;
    }
    size_t n = fread(buf, 1, (size_t)sz, f);
    fclose(f);
    if (n != (size_t)sz) {
        fprintf(stderr, "Short read (expected %ld, got %zu)\n", sz, n);
        free(buf);
        return NULL;
    }
    *out_len = (size_t)sz;
    return buf;
}

static char* read_stdin(size_t* out_len) {
    size_t cap = 16 * 1024;
    size_t len = 0;
    char* buf = (char*)malloc(cap);
    if (!buf) {
        fprintf(stderr, "Out of memory\n");
        return NULL;
    }
    for (;;) {
        if (cap - len < 4096) {
            size_t new_cap = cap * 2;
            char* nb = (char*)realloc(buf, new_cap);
            if (!nb) {
                fprintf(stderr, "Out of memory\n");
                free(buf);
                return NULL;
            }
            buf = nb;
            cap = new_cap;
        }
        size_t n = fread(buf + len, 1, cap - len, stdin);
        len += n;
        if (n == 0) {
            if (feof(stdin)) break;
            if (ferror(stdin)) {
                fprintf(stderr, "Error reading stdin\n");
                free(buf);
                return NULL;
            }
        }
    }
    *out_len = len;
    return buf;
}

/* --------- Print helpers --------- */

static void print_escaped_lexeme(const char* s, size_t len) {
    putchar('"');
    for (size_t i = 0; i < len; i++) {
        unsigned char c = (unsigned char)s[i];
        switch (c) {
            case '\n': fputs("\\n", stdout); break;
            case '\r': fputs("\\r", stdout); break;
            case '\t': fputs("\\t", stdout); break;
            case '\\': fputs("\\\\", stdout); break;
            case '"':  fputs("\\\"", stdout); break;
            default:
                if (c < 0x20 || c == 0x7F) {
                    fprintf(stdout, "\\x%02X", c);
                } else {
                    fputc(c, stdout);
                }
        }
    }
    putchar('"');
}

static void print_token_lexeme(const OwlToken* tokens, size_t token_count, OwlTokIdx idx) {
    if (idx >= token_count) { fputs("<tok-oob>", stdout); return; }
    const OwlToken* t = &tokens[idx];
    print_escaped_lexeme(t->lexeme, t->lexeme_len);
}

static const char* node_kind_name(OwlAstNodeKind k) {
    switch (k) {
        case OWL_AST_PROGRAM: return "PROGRAM";
        case OWL_AST_FUNC_DECL: return "FUNC_DECL";
        case OWL_AST_TUPLE_DECL: return "TUPLE_DECL";
        case OWL_AST_TYPE_NAME: return "TYPE_NAME";
        case OWL_AST_TYPE_ARRAY: return "TYPE_ARRAY";
        case OWL_AST_STMT_BLOCK: return "STMT_BLOCK";
        case OWL_AST_STMT_VAR_OR_ASSIGN: return "STMT_VAR_OR_ASSIGN";
        case OWL_AST_STMT_RETURN: return "STMT_RETURN";
        case OWL_AST_STMT_IF: return "STMT_IF";
        case OWL_AST_STMT_LOOP: return "STMT_LOOP";
        case OWL_AST_EXPR_IDENTIFIER: return "EXPR_IDENTIFIER";
        case OWL_AST_EXPR_LITERAL: return "EXPR_LITERAL";
        case OWL_AST_EXPR_UNARY: return "EXPR_UNARY";
        case OWL_AST_EXPR_BINARY: return "EXPR_BINARY";
        case OWL_AST_EXPR_CALL: return "EXPR_CALL";
        case OWL_AST_EXPR_INDEX: return "EXPR_INDEX";
        case OWL_AST_EXPR_ARRAY: return "EXPR_ARRAY";
        case OWL_AST_EXPR_GROUP: return "EXPR_GROUP";
        case OWL_AST_EXPR_TUPLE_CTOR: return "EXPR_TUPLE_CTOR";
        default: return "UNKNOWN_NODE";
    }
}

static const char* error_code_name(OwlParseErrorCode c) {
    switch (c) {
        case OWL_PARSE_OK: return "OK";
        case OWL_PARSE_ERR_UNEXPECTED_TOKEN: return "UNEXPECTED_TOKEN";
        case OWL_PARSE_ERR_UNTERMINATED_STRING: return "UNTERMINATED_STRING";
        case OWL_PARSE_ERR_UNTERMINATED_BLOCK_COMMENT: return "UNTERMINATED_BLOCK_COMMENT";
        case OWL_PARSE_ERR_EXPECTED_IDENTIFIER: return "EXPECTED_IDENTIFIER";
        case OWL_PARSE_ERR_EXPECTED_TYPE: return "EXPECTED_TYPE";
        case OWL_PARSE_ERR_EXPECTED_EXPR: return "EXPECTED_EXPR";
        case OWL_PARSE_ERR_EXPECTED_STMT: return "EXPECTED_STMT";
        case OWL_PARSE_ERR_EXPECTED_RPAREN: return "EXPECTED_RPAREN";
        case OWL_PARSE_ERR_EXPECTED_RBRACE: return "EXPECTED_RBRACE";
        case OWL_PARSE_ERR_EXPECTED_RBRACKET: return "EXPECTED_RBRACKET";
        case OWL_PARSE_ERR_UNDECLARED_IDENTIFIER: return "UNDECLARED_IDENTIFIER";
        case OWL_PARSE_ERR_EXPECTED_ASSIGN: return "EXPECTED_ASSIGN";
        case OWL_PARSE_ERR_EXPECTED_SEMICOLON: return "EXPECTED_SEMICOLON";
        case OWL_PARSE_ERR_MIXED_TUPLE_ARGS: return "MIXED_TUPLE_ARGS";
        case OWL_PARSE_ERR_UNKNOWN:
        default: return "UNKNOWN";
    }
}

static void indent(int n) {
    for (int i = 0; i < n; i++) putchar(' ');
}

/* Forward decls for printing */
static void print_node_brief(const OwlParseResult* r, OwlAstId id, int level);
static void print_type_inline(const OwlParseResult* r, OwlAstId type_id);

/* --------- Printing AST nodes --------- */

static void print_program(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("%s items=%zu\n", node_kind_name(n->kind), n->as.program.len);
    for (size_t i = 0; i < n->as.program.len; i++) {
        const OwlTopLevelItem* it = &n->as.program.items[i];
        indent(level + 2);
        const char* kname =
            it->kind == OWL_TOP_FUNC  ? "TopLevel:Func" :
            it->kind == OWL_TOP_TUPLE ? "TopLevel:Tuple" :
            it->kind == OWL_TOP_STMT  ? "TopLevel:Stmt" : "TopLevel:Unknown";
        printf("[%zu] %s\n", i, kname);
        print_node_brief(r, it->node, level + 4);
    }
}

static void print_type_inline(const OwlParseResult* r, OwlAstId type_id) {
    const OwlAstNode* tn = owl_ast_get_const(&r->ast, type_id);
    if (!tn) { fputs("<no-type>", stdout); return; }
    switch (tn->kind) {
        case OWL_AST_TYPE_NAME: {
            print_token_lexeme(r->tokens, r->token_count, tn->as.type_name.name_tok);
            return;
        }
        case OWL_AST_TYPE_ARRAY: {
            putchar('[');
            print_type_inline(r, tn->as.type_array.elem_type);
            putchar(']');
            return;
        }
        default:
            fputs("<type-unknown>", stdout);
            return;
    }
}

static void print_tuple_decl(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("TUPLE_DECL name=");
    print_token_lexeme(r->tokens, r->token_count, n->as.tuple_decl.name_tok);
    printf(" fields=%zu\n", n->as.tuple_decl.field_types.len);

    for (size_t i = 0; i < n->as.tuple_decl.field_types.len; i++) {
        indent(level + 2);
        printf("- ");
        print_token_lexeme(r->tokens, r->token_count, n->as.tuple_decl.field_name_toks.items[i]);
        printf(": ");
        print_type_inline(r, n->as.tuple_decl.field_types.items[i]);
        putchar('\n');
    }
}

static void print_func_decl(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("FUNC_DECL name=");
    print_token_lexeme(r->tokens, r->token_count, n->as.func_decl.name_tok);
    printf(" params=%zu", n->as.func_decl.param_types.len);
    if (n->as.func_decl.return_type) {
        printf(" -> ");
        print_type_inline(r, n->as.func_decl.return_type);
    }
    putchar('\n');

    for (size_t i = 0; i < n->as.func_decl.param_types.len; i++) {
        indent(level + 2);
        printf("- param ");
        print_token_lexeme(r->tokens, r->token_count, n->as.func_decl.param_name_toks.items[i]);
        printf(": ");
        print_type_inline(r, n->as.func_decl.param_types.items[i]);
        putchar('\n');
    }
    indent(level + 2);
    puts("{");
    print_node_brief(r, n->as.func_decl.body_block, level + 4);
    indent(level + 2);
    puts("}");
}

static void print_block(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("BLOCK stmts=%zu\n", n->as.stmt_block.stmts.len);
    for (size_t i = 0; i < n->as.stmt_block.stmts.len; i++) {
        print_node_brief(r, n->as.stmt_block.stmts.items[i], level + 2);
    }
}

static void print_var_or_assign(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("VAR_OR_ASSIGN name=");
    print_token_lexeme(r->tokens, r->token_count, n->as.stmt_var_or_assign.name_tok);
    if (n->as.stmt_var_or_assign.opt_type) {
        printf(" : ");
        print_type_inline(r, n->as.stmt_var_or_assign.opt_type);
    }
    printf(" =\n");
    print_node_brief(r, n->as.stmt_var_or_assign.rhs_expr, level + 2);
}

static void print_return(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("RETURN");
    if (n->as.stmt_return.opt_expr) {
        printf("\n");
        print_node_brief(r, n->as.stmt_return.opt_expr, level + 2);
    } else {
        printf(" (void)\n");
    }
}

static void print_if(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    puts("IF cond:");
    print_node_brief(r, n->as.stmt_if.cond_expr, level + 2);
    indent(level);
    puts("THEN:");
    print_node_brief(r, n->as.stmt_if.then_block, level + 2);
    if (n->as.stmt_if.else_block) {
        indent(level);
        puts("ELSE:");
        print_node_brief(r, n->as.stmt_if.else_block, level + 2);
    }
}

static void print_loop(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    puts("LOOP cond:");
    print_node_brief(r, n->as.stmt_loop.cond_expr, level + 2);
    indent(level);
    puts("BODY:");
    print_node_brief(r, n->as.stmt_loop.body_block, level + 2);
    if (n->as.stmt_loop.else_block) {
        indent(level);
        puts("ELSE:");
        print_node_brief(r, n->as.stmt_loop.else_block, level + 2);
    }
}

static void print_expr_ident(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("IDENT ");
    print_token_lexeme(r->tokens, r->token_count, n->as.expr_ident.name_tok);
    putchar('\n');
}

static void print_expr_literal(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    const char* flav = "LIT";
    switch (n->as.expr_literal.flavor) {
        case OWL_LITERAL_INT: flav = "INT"; break;
        case OWL_LITERAL_FLOAT: flav = "FLOAT"; break;
        case OWL_LITERAL_STRING: flav = "STRING"; break;
        case OWL_LITERAL_BOOL: flav = "BOOL"; break;
        default: break;
    }
    printf("%s ", flav);
    print_token_lexeme(r->tokens, r->token_count, n->as.expr_literal.tok);
    putchar('\n');
}

static void print_expr_unary(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("UNARY op=%s ", owl_token_kind_name(n->as.expr_unary.op_tok_kind));
    printf("lexeme=");
    print_token_lexeme(r->tokens, r->token_count, n->as.expr_unary.op_tok);
    putchar('\n');
    print_node_brief(r, n->as.expr_unary.operand, level + 2);
}

static void print_expr_binary(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("BINARY op=%s ", owl_token_kind_name(n->as.expr_binary.op_tok_kind));
    printf("lexeme=");
    print_token_lexeme(r->tokens, r->token_count, n->as.expr_binary.op_tok);
    putchar('\n');

    indent(level); puts("left:");
    print_node_brief(r, n->as.expr_binary.left, level + 2);
    indent(level); puts("right:");
    print_node_brief(r, n->as.expr_binary.right, level + 2);
}

static void print_expr_call(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("CALL args=%zu\n", n->as.expr_call.args.len);
    indent(level); puts("callee:");
    print_node_brief(r, n->as.expr_call.callee, level + 2);
    for (size_t i = 0; i < n->as.expr_call.args.len; i++) {
        indent(level);
        printf("arg[%zu]:\n", i);
        print_node_brief(r, n->as.expr_call.args.items[i], level + 2);
    }
}

static void print_expr_index(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    puts("INDEX target:");
    print_node_brief(r, n->as.expr_index.target, level + 2);
    indent(level);
    puts("INDEX expr:");
    print_node_brief(r, n->as.expr_index.index_expr, level + 2);
}

static void print_expr_array(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    printf("ARRAY elements=%zu\n", n->as.expr_array.elements.len);
    for (size_t i = 0; i < n->as.expr_array.elements.len; i++) {
        indent(level);
        printf("el[%zu]:\n", i);
        print_node_brief(r, n->as.expr_array.elements.items[i], level + 2);
    }
}

static void print_expr_group(const OwlParseResult* r, const OwlAstNode* n, int level) {
    indent(level);
    puts("GROUP");
    print_node_brief(r, n->as.expr_group.inner, level + 2);
}

static void print_expr_tuple_ctor(const OwlParseResult* r, const OwlAstNode* n, int level) {
    int has_named = (n->as.expr_tuple_ctor.named_field_toks.len > 0);
    indent(level);
    printf("TUPLE_CTOR name=");
    print_token_lexeme(r->tokens, r->token_count, n->as.expr_tuple_ctor.name_tok);
    printf(" %s\n", has_named ? "named" : "positional");

    if (has_named) {
        for (size_t i = 0; i < n->as.expr_tuple_ctor.named_field_toks.len; i++) {
            indent(level + 2);
            print_token_lexeme(r->tokens, r->token_count, n->as.expr_tuple_ctor.named_field_toks.items[i]);
            printf(" =\n");
            print_node_brief(r, n->as.expr_tuple_ctor.named_values.items[i], level + 4);
        }
    } else {
        for (size_t i = 0; i < n->as.expr_tuple_ctor.positional_args.len; i++) {
            indent(level);
            printf("arg[%zu]:\n", i);
            print_node_brief(r, n->as.expr_tuple_ctor.positional_args.items[i], level + 2);
        }
    }
}

static void print_node_brief(const OwlParseResult* r, OwlAstId id, int level) {
    const OwlAstNode* n = owl_ast_get_const(&r->ast, id);
    if (!n) { indent(level); puts("<null>"); return; }

    switch (n->kind) {
        case OWL_AST_PROGRAM:        print_program(r, n, level); break;
        case OWL_AST_TUPLE_DECL:     print_tuple_decl(r, n, level); break;
        case OWL_AST_FUNC_DECL:      print_func_decl(r, n, level); break;
        case OWL_AST_STMT_BLOCK:     print_block(r, n, level); break;
        case OWL_AST_STMT_VAR_OR_ASSIGN: print_var_or_assign(r, n, level); break;
        case OWL_AST_STMT_RETURN:    print_return(r, n, level); break;
        case OWL_AST_STMT_IF:        print_if(r, n, level); break;
        case OWL_AST_STMT_LOOP:      print_loop(r, n, level); break;
        case OWL_AST_STMT_EXPR: {
            indent(level);
            puts("EXPR_STMT");
            /* Print the contained expression (e.g., a function call) */
            print_node_brief(r, n->as.stmt_expr.expr, level + 2);
            if (n->as.stmt_expr.semi_tok) {
                indent(level + 2);
                fputs("semicolon: ", stdout);
                print_token_lexeme(r->tokens, r->token_count, n->as.stmt_expr.semi_tok);
                putchar('\n');
            }
            break;
        }

        case OWL_AST_TYPE_NAME: {
            indent(level); fputs("TYPE ", stdout);
            print_type_inline(r, id);
            putchar('\n');
            break;
        }
        case OWL_AST_TYPE_ARRAY: {
            indent(level); fputs("TYPE_ARRAY [", stdout);
            print_type_inline(r, ((const OwlAstNode*)n)->as.type_array.elem_type);
            puts("]");
            break;
        }

        case OWL_AST_EXPR_IDENTIFIER: print_expr_ident(r, n, level); break;
        case OWL_AST_EXPR_LITERAL:    print_expr_literal(r, n, level); break;
        case OWL_AST_EXPR_UNARY:      print_expr_unary(r, n, level); break;
        case OWL_AST_EXPR_BINARY:     print_expr_binary(r, n, level); break;
        case OWL_AST_EXPR_CALL:       print_expr_call(r, n, level); break;
        case OWL_AST_EXPR_INDEX:      print_expr_index(r, n, level); break;
        case OWL_AST_EXPR_ARRAY:      print_expr_array(r, n, level); break;
        case OWL_AST_EXPR_GROUP:      print_expr_group(r, n, level); break;
        case OWL_AST_EXPR_TUPLE_CTOR: print_expr_tuple_ctor(r, n, level); break;

        default:
            indent(level);
            printf("%s [tokens %u..%u)\n", node_kind_name(n->kind), (unsigned)n->first_tok, (unsigned)n->last_tok_excl);
            break;
    }
}

/* --------- Diagnostics printing --------- */

static void print_diagnostics(const OwlParseResult* r) {
    printf("\nDiagnostics: %zu error(s)\n", r->error_count);
    for (size_t i = 0; i < r->error_count; i++) {
        const OwlParseError* e = &r->errors[i];
        printf("  - %s at %zu:%zu (tok=%u)\n",
               error_code_name(e->code),
               e->span.start.line, e->span.start.column,
               (unsigned)e->at_token);
    }
}

/* --------- Main --------- */

static void usage(const char* argv0) {
    fprintf(stderr,
            "Owl Parser Demo\n"
            "Usage:\n"
            "  %s <file.owl>   Parse file and print AST summary\n"
            "  %s              Read source from stdin\n",
            argv0, argv0);
}

int main(int argc, char** argv) {
    char* src = NULL;
    size_t len = 0;

    if (argc >= 2) {
        if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
            usage(argv[0]);
            return 0;
        }
        src = read_file(argv[1], &len);
        if (!src) return 1;
    } else {
        src = read_stdin(&len);
        if (!src) return 1;
    }

    OwlParserOptions opts = {0};
    opts.continue_after_error = 1;

    OwlParseResult r = owl_parse(src, len, &opts);

    printf("Parsed tokens: %zu\n", r.token_count);
    printf("AST nodes: %zu\n", r.ast.len);
    printf("Root node id: %u\n\n", (unsigned)r.root);

    print_node_brief(&r, r.root, 0);
    print_diagnostics(&r);

    owl_parse_free(&r);
    free(src);
    return 0;
}