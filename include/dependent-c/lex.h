#ifndef DEPENDENT_C_LEX
#define DEPENDENT_C_LEX

#include <stdio.h>

typedef enum {
      TOK_IDENT
    , TOK_RESERVED
    , TOK_SYMBOL
    , TOK_EOF
} TokenTag;

typedef enum {
      TOK_RES_TYPE
} TokenReserved;

typedef enum {
      TOK_SYM_LPAREN,   TOK_SYM_RPAREN      // ( )
    , TOK_SYM_LSQUARE,  TOK_SYM_RSQUARE     // [ ]
    , TOK_SYM_LBRACE,   TOK_SYM_RBRACE      // { }
} TokenSymbol;

typedef struct {
    unsigned line;
    unsigned column;

    TokenTag tag;
    union {
        char *ident;
        TokenReserved reserved;
        TokenSymbol symbol;
    } data;
} Token;

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

    // Peeked-at tokens.
    size_t peeked_cap;
    size_t peeked_len;
    Token *peeked;

    unsigned line;
    unsigned column;
} TokenStream;

/* Create and free token streams. */
TokenStream token_stream_new(CharStream source);
void token_stream_free(TokenStream *stream);

/* Remove and put back tokens from a stream. */
Token token_stream_pop(TokenStream *stream);
void token_stream_push(TokenStream *stream, Token token);

/* Free any allocated resources associated with a token. */
void token_free(Token token);

#endif /* DEPENDENT_C_LEX */
