#ifndef OWL_RT_H
#define OWL_RT_H

/* Owl Runtime — Iteration 1
 *
 * Features:
 * - String type with ownership semantics (heap-allocated, explicit free)
 * - Print overloads for: string, int, float, bool, byte
 *
 * Notes:
 * - Strings must be freed when they go out of scope by the transpiled code.
 * - No numeric type promotion: int operations are on int32_t, float on float.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ---------- String Type ---------- */

typedef struct {
    char*  data;
    size_t len;
} OwlString;

/* Create a string by copying raw bytes (not necessarily NUL-terminated). 
 * Appends a trailing '\0' for convenience, but len excludes it.
 */
static inline OwlString owl_str_from_literal(const char* bytes, size_t len) {
    char* d = (char*)malloc(len + 1);
    if (!d) {
        fprintf(stderr, "[owl_rt] allocation failed in owl_str_from_literal\n");
        abort();
    }
    if (len > 0 && bytes) {
        memcpy(d, bytes, len);
    }
    d[len] = '\0';
    OwlString s = { d, len };
    return s;
}

/* Convenience: create from a NUL-terminated C string (copies the data). */
static inline OwlString owl_str_from_cstr(const char* cstr) {
    size_t n = cstr ? strlen(cstr) : 0;
    return owl_str_from_literal(cstr, n);
}

/* Deep copy a string (allocates a new buffer). */
static inline OwlString owl_str_copy(OwlString src) {
    if (!src.data || src.len == 0) {
        OwlString s = { NULL, 0 };
        return s;
    }
    return owl_str_from_literal(src.data, src.len);
}

/* Free a string and reset it to empty. */
static inline void owl_str_free(OwlString* s) {
    if (!s) return;
    if (s->data) {
        free(s->data);
        s->data = NULL;
    }
    s->len = 0;
}

/* ---------- Print Overloads ---------- */

/* Print a string followed by newline. */
static inline void owl_print_string(OwlString s) {
    if (s.data && s.len > 0) {
        fwrite(s.data, 1, s.len, stdout);
    }
    fputc('\n', stdout);
}

/* Print a 32-bit integer followed by newline. */
static inline void owl_print_int(int32_t v) {
    printf("%d\n", v);
}

/* Print a 32-bit float followed by newline. */
static inline void owl_print_float(float v) {
    /* Using %g to provide a compact representation. 
       Adjust formatting in later iterations if needed. */
    printf("%g\n", v);
}

/* Print a boolean (1 byte) as "true"/"false" followed by newline. */
static inline void owl_print_bool(uint8_t v) {
    fputs(v ? "true\n" : "false\n", stdout);
}

/* Print a byte (8-bit unsigned integer) followed by newline. */
static inline void owl_print_byte(uint8_t v) {
    printf("%u\n", (unsigned)v);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* OWL_RT_H */