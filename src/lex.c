#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dependent-c/lex.h"

/***** Strn Char Stream Implementation ***************************************/
struct strn_char_stream_data {
    char *str;
    size_t i;
};

int strn_char_stream_next(void *_self_data) {
    struct strn_char_stream_data *self_data = _self_data;

    int c = self_data->str[self_data->i];
    if (c == '\0') {
        return EOF;
    } else {
        self_data->i += 1;
        return c;
    }
}

void strn_char_stream_free(void *_self_data) {
    struct strn_char_stream_data *self_data = _self_data;

    free(self_data->str);
    free(self_data);
}

CharStream strn_to_char_stream(const char *str, size_t len) {
    CharStream result;
    struct strn_char_stream_data self_data;

    self_data.str = malloc(len + 1);
    memcpy(self_data.str, str, len);
    self_data.str[len] = '\0';
    self_data.i = 0;

    result.next = strn_char_stream_next;
    result.free = strn_char_stream_free;
    result.self_data = malloc(sizeof self_data);
    *(struct strn_char_stream_data*)result.self_data = self_data;
    result.peeked_cap = 0;
    result.peeked_len = 0;
    result.peeked = NULL;

    return result;
}

/***** Str Char Stream Implementation ****************************************/
CharStream str_to_char_stream(const char *str) {
    return strn_to_char_stream(str, strlen(str));
}

/***** File Char Stream Implementation ***************************************/
int file_char_stream_next(void *_self_data) {
    FILE *self_data = _self_data;

    return fgetc(self_data);
}

void file_char_stream_free(void *_self_data) {
    FILE *self_data = _self_data;

    fclose(self_data);
}

CharStream file_to_char_stream(FILE *file) {
    CharStream result;

    result.next = file_char_stream_next;
    result.free = file_char_stream_free;
    result.self_data = file;
    result.peeked_cap = 0;
    result.peeked_len = 0;
    result.peeked = NULL;

    return result;
}

/***** Char Stream Implementation ********************************************/
int char_stream_pop(CharStream *stream) {
    if (stream->peeked_len > 0) {
        stream->peeked_len -= 1;
        return stream->peeked[stream->peeked_len];
    } else {
        return stream->next(stream->self_data);
    }
}

void char_stream_push(CharStream *stream, int c) {
    if (c == EOF) {
        return;
    }

    if (stream->peeked_len + 1 > stream->peeked_cap) {
        stream->peeked = realloc(stream->peeked,
            (stream->peeked_len + 1) * sizeof *stream->peeked);
        stream->peeked_cap = stream->peeked_len + 1;
    }

    stream->peeked[stream->peeked_len] = c;
    stream->peeked_len += 1;
}

/***** Token Stream Implementation *******************************************/
TokenStream token_stream_new(CharStream source) {
    TokenStream result;

    result.source = source;
    result.peeked_cap = 0;
    result.peeked_len = 0;
    result.peeked = NULL;
    result.line = 1;
    result.column = 1;

    return result;
}

void token_stream_free(TokenStream *stream) {
    stream->source.free(stream->source.self_data);
    free(stream->peeked);
}

/***** Lexing ****************************************************************/
static int token_stream_pop_char(TokenStream *stream) {
    int c = char_stream_pop(&stream->source);

    if (c == '\n') {
        stream->line += 1;
        stream->column = 1;
    } else if (c != EOF ){
        stream->column += 1;
    }

    return c;
}

// Note: does not reverse the line/column update performed in pop_char.
//       As such, line/column readings should be taken before popping/pushing
//       characters.
//
//       Can be fixed by keeping a stack of column numbers which are pushed/
//       popped on a '\n' character. Reward/Effort ratio is a bit low though.
static void token_stream_push_char(TokenStream *stream, int c) {
    char_stream_push(&stream->source, c);
}

static void skip_whitespace(TokenStream *stream) {
    while (true) {
        int c = token_stream_pop_char(stream);

        if (!isspace(c)) {
            token_stream_push_char(stream, c);
            break;
        }
    }
}

static Token token_stream_next(TokenStream *stream) {
start_of_function:
    skip_whitespace(stream);

    Token token;
    token.line = stream->line;
    token.column = stream->column;

    int c = token_stream_pop_char(stream);
    if (isalpha(c) || c == '_') {
        size_t len = 0;
        token.tag = TOK_IDENT;
        token.data.ident = NULL;
        token_stream_push_char(stream, c);

        while (true) {
            c = token_stream_pop_char(stream);

            if (isalnum(c) || c == '_') {
                token.data.ident = realloc(token.data.ident, len + 1);
                token.data.ident[len] = c;
                len += 1;
            } else {
                token_stream_push_char(stream, c);
                break;
            }
        }

        token.data.ident = realloc(token.data.ident, len + 1);
        token.data.ident[len] = '\0';

        if (strcmp("type", token.data.ident) == 0) {
            free(token.data.ident);
            token.tag = TOK_RESERVED;
            token.data.reserved = TOK_RES_TYPE;
        }
    } else if (c == '(') {
        token.tag = TOK_SYMBOL;
        token.data.symbol = TOK_SYM_LPAREN;
    } else if (c == ')') {
        token.tag = TOK_SYMBOL;
        token.data.symbol = TOK_SYM_RPAREN;
    } else if (c == EOF) {
        token.tag = TOK_EOF;
    } else {
        fprintf(stderr, "Lexer encountered unexpected character '%c' at line "
            "%d, column %d. Skipping.\n", c, token.line, token.column);
        // In the presence of guarenteed tail-calls, such 'barbaric' measures
        // are not necessary. C does not have those however.
        goto start_of_function;
    }

    return token;
}

Token token_stream_pop(TokenStream *stream) {
    if (stream->peeked_len > 0) {
        stream->peeked_len -= 1;
        return stream->peeked[stream->peeked_len];
    } else {
        return token_stream_next(stream);
    }
}

void token_stream_push(TokenStream *stream, Token token) {
    if (token.tag == TOK_EOF) {
        return;
    }

    if (stream->peeked_len + 1 > stream->peeked_cap) {
        stream->peeked = realloc(stream->peeked,
            (stream->peeked_len + 1) * sizeof *stream->peeked);
        stream->peeked_cap = stream->peeked_len + 1;
    }

    stream->peeked[stream->peeked_len] = token;
    stream->peeked_len += 1;
}

/***** Token Management ******************************************************/
void token_free(Token token) {
    switch (token.tag) {
      case TOK_IDENT:
        free(token.data.ident);
        break;
      case TOK_RESERVED:
      case TOK_SYMBOL:
      case TOK_EOF:
        break;
    }
}
