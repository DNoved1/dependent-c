#ifndef DEPENDENT_C_LEX
#define DEPENDENT_C_LEX

#include <stdio.h>

/* A stream of characters terminated with EOF. */
typedef struct {
    int (*next)(void *self_data);
    void (*free)(void *self_data);
    void *self_data;

    // Peeked-at characters.
    size_t peeked_cap;
    size_t peeked_len;
    char *peeked;
} CharStream;

/* Functions to create character streams from strings and files. */
CharStream strn_to_char_stream(const char *str, size_t len);
CharStream str_to_char_stream(const char *str);
CharStream file_to_char_stream(FILE *file);

/* Remove and put back characters from a stream. */
int char_stream_pop(CharStream *stream);
void char_stream_push(CharStream *stream, int c);

/* A stream of tokens terminated with TOK_EOF. Since there is only one
 * implementation the functions are not virtual. */
typedef struct {
    CharStream source;
    unsigned line;
    unsigned column;
} TokenStream;

/* Create and free token streams. */
TokenStream token_stream_new(CharStream source);
void token_stream_free(TokenStream *stream);

#endif /* DEPENDENT_C_LEX */
