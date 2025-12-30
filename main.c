/*
  Owl Lexer Demo (C17)
  File: c/owl/main.c

  Builds a small CLI to test the lossless lexer:
    - Reads a file path from argv or stdin if none provided
    - Runs the lexer and prints each token with kind, span, and raw lexeme
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"

/* Read entire file into memory. Returns malloc'd buffer and sets *out_len. */
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
        fprintf(stderr, "Failed to tell size: %s\n", path);
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
        fprintf(stderr, "Out of memory reading file\n");
        fclose(f);
        return NULL;
    }
    size_t nread = fread(buf, 1, (size_t)sz, f);
    fclose(f);
    if (nread != (size_t)sz) {
        fprintf(stderr, "Short read: expected %ld, got %zu\n", sz, nread);
        free(buf);
        return NULL;
    }
    *out_len = (size_t)sz;
    return buf;
}

/* Read all stdin into memory. Returns malloc'd buffer and sets *out_len. */
static char* read_stdin(size_t* out_len) {
    size_t cap = 16 * 1024;
    size_t len = 0;
    char* buf = (char*)malloc(cap);
    if (!buf) {
        fprintf(stderr, "Out of memory\n");
        return NULL;
    }
    for (;;) {
        size_t space = cap - len;
        if (space < 4096) {
            size_t new_cap = cap * 2;
            char* nb = (char*)realloc(buf, new_cap);
            if (!nb) {
                fprintf(stderr, "Out of memory\n");
                free(buf);
                return NULL;
            }
            buf = nb;
            cap = new_cap;
            space = cap - len;
        }
        size_t n = fread(buf + len, 1, space, stdin);
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

/* Print lexeme safely by escaping non-printables */
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
                    /* control char */
                    fprintf(stdout, "\\x%02X", c);
                } else {
                    fputc(c, stdout);
                }
        }
    }
    putchar('"');
}

/* Print a token in a readable lossless way */
static void print_token(const OwlToken* t) {
    const char* kind = owl_token_kind_name(t->kind);
    printf("%-16s  span=[%zu:%zu .. %zu:%zu]  len=%zu  lexeme=",
           kind,
           t->span.start.line, t->span.start.column,
           t->span.end.line, t->span.end.column,
           t->lexeme_len);
    print_escaped_lexeme(t->lexeme, t->lexeme_len);

    /* Optional decoded payloads for literals */
    switch (t->kind) {
        case OWL_TOK_INT_LITERAL:
            printf("  value=%lld", (long long)t->as.int_lit.value);
            break;
        case OWL_TOK_FLOAT_LITERAL:
            printf("  value=%g", t->as.float_lit.value);
            break;
        case OWL_TOK_STRING_LITERAL:
            printf("  has_escapes=%d", (int)t->as.string_lit.has_escapes);
            break;
        default: break;
    }
    putchar('\n');
}

static void usage(const char* argv0) {
    fprintf(stderr,
            "Owl Lexer Demo\n"
            "Usage:\n"
            "  %s <file.owl>   Lex file and print tokens\n"
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

    OwlLexer lx;
    owl_lexer_init(&lx, src, len);

    for (;;) {
        OwlToken tok = owl_lexer_next(&lx);
        print_token(&tok);
        if (tok.kind == OWL_TOK_EOF) break;
    }

    free(src);
    return 0;
}