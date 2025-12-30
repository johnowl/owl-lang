/*
  Owl Transpile Demo (C17)
  File: c/owl-lang/main_transpile_demo.c

  This demo:
    - Reads Owl source from a file path (argv[1]) or stdin
    - Parses it into a full token stream + AST (lossless-friendly)
    - Transpiles the AST to C17 source code (iteration 1)
    - Prints the generated C code to stdout
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"
#include "transpile.h"

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

/* --------- Diagnostics printing (parser) --------- */

static const char* parse_error_code_name(OwlParseErrorCode c) {
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
        case OWL_PARSE_ERR_EXPECTED_ASSIGN: return "EXPECTED_ASSIGN";
        case OWL_PARSE_ERR_EXPECTED_SEMICOLON: return "EXPECTED_SEMICOLON";
        case OWL_PARSE_ERR_MIXED_TUPLE_ARGS: return "MIXED_TUPLE_ARGS";
        case OWL_PARSE_ERR_UNKNOWN:
        default: return "UNKNOWN";
    }
}

static void print_parse_diagnostics(const OwlParseResult* r) {
    if (!r) return;
    fprintf(stderr, "Parser diagnostics: %zu error(s)\n", r->error_count);
    for (size_t i = 0; i < r->error_count; i++) {
        const OwlParseError* e = &r->errors[i];
        fprintf(stderr, "  - %s at %zu:%zu (tok=%u)\n",
                parse_error_code_name(e->code),
                e->span.start.line, e->span.start.column,
                (unsigned)e->at_token);
    }
}

/* --------- Diagnostics printing (transpiler) --------- */

static const char* tgen_error_code_name(OwlTranspileErrorCode c) {
    switch (c) {
        case OWL_TGEN_OK: return "OK";
        case OWL_TGEN_ERR_INTERNAL: return "INTERNAL";
        case OWL_TGEN_ERR_UNSUPPORTED_NODE: return "UNSUPPORTED_NODE";
        case OWL_TGEN_ERR_TYPE_MISMATCH: return "TYPE_MISMATCH";
        case OWL_TGEN_ERR_MIXED_NUMERIC: return "MIXED_NUMERIC";
        case OWL_TGEN_ERR_UNKNOWN_IDENTIFIER: return "UNKNOWN_IDENTIFIER";
        case OWL_TGEN_ERR_PRINT_ARG_COUNT: return "PRINT_ARG_COUNT";
        case OWL_TGEN_ERR_PRINT_ARG_TYPE: return "PRINT_ARG_TYPE";
        case OWL_TGEN_ERR_UNSUPPORTED_EXPR: return "UNSUPPORTED_EXPR";
        case OWL_TGEN_ERR_UNSUPPORTED_STMT: return "UNSUPPORTED_STMT";
        default: return "UNKNOWN";
    }
}

static void print_tgen_diagnostics(const OwlTranspileResult* out) {
    if (!out) return;
    fprintf(stderr, "Transpiler diagnostics: %zu error(s)\n", out->errors.len);
    for (size_t i = 0; i < out->errors.len; i++) {
        const OwlTranspileError* e = &out->errors.items[i];
        fprintf(stderr, "  - %s (tok=%u): %s\n",
                tgen_error_code_name(e->code),
                (unsigned)e->at_token,
                e->message ? e->message : "");
    }
}

/* --------- Usage --------- */

static void usage(const char* argv0) {
    fprintf(stderr,
            "Owl Transpile Demo\n"
            "Usage:\n"
            "  %s <file.owl>   Parse and transpile file to C, print C to stdout\n"
            "  %s              Read Owl source from stdin and transpile\n",
            argv0, argv0);
}

/* --------- Main --------- */

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

    /* Parse Owl source */
    OwlParserOptions popts = {0};
    popts.continue_after_error = 1;
    OwlParseResult pr = owl_parse(src, len, &popts);

    /* Optionally, report parser diagnostics to stderr */
    print_parse_diagnostics(&pr);

    /* Transpile to C */
    OwlTranspileOptions topts = {0};
    topts.emit_span_comments = 0;
    OwlTranspileResult tres = owl_transpile_program(&pr, &topts);

    /* Print generated C to stdout */
    if (tres.c_source && tres.c_source_len > 0) {
        fwrite(tres.c_source, 1, tres.c_source_len, stdout);
        /* Ensure trailing newline for readability */
        if (tres.c_source[tres.c_source_len - 1] != '\n') {
            fputc('\n', stdout);
        }
    } else {
        fprintf(stderr, "No C source generated.\n");
    }

    /* Report transpiler diagnostics */
    print_tgen_diagnostics(&tres);

    /* Cleanup */
    owl_transpile_result_free(&tres);
    owl_parse_free(&pr);
    free(src);

    return 0;
}