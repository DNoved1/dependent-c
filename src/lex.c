#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "dependent-c/lex.h"
#include "dependent-c/memory.h"

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

    dealloc(self_data->str);
    dealloc(self_data);
}

CharStream strn_to_char_stream(const char *str, size_t len) {
    CharStream result;
    struct strn_char_stream_data self_data;

    alloc_array(self_data.str, len + 1);
    memcpy(self_data.str, str, len);
    self_data.str[len] = '\0';
    self_data.i = 0;

    result.next = strn_char_stream_next;
    result.free = strn_char_stream_free;
    result.self_data = _alloc(__FILE__, __LINE__, sizeof self_data);
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
        realloc_array(stream->peeked, stream->peeked_len + 1);
        stream->peeked_cap = stream->peeked_len + 1;
    }

    stream->peeked[stream->peeked_len] = c;
    stream->peeked_len += 1;
}

/***** Token Stream Implementation *******************************************/
TokenStream token_stream_new(CharStream source) {
    return (TokenStream){
          .source = source
        , .line = 1
        , .column = 1
    };
}

void token_stream_free(TokenStream *stream) {
    stream->source.free(stream->source.self_data);
}
