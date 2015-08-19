#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "dependent-c/lex.h"

bool token_cmp(Token x, Token y) {
    if (x.tag != y.tag)
        return false;

    switch (x.tag) {
      case TOK_IDENT:
        return strcmp(x.data.ident, y.data.ident) == 0;
      case TOK_RESERVED:
        return x.data.reserved == y.data.reserved;
      case TOK_SYMBOL:
        return x.data.symbol == y.data.symbol;
      case TOK_EOF:
        return true;
    }
}

int token_print(Token token) {
    switch (token.tag) {
      case TOK_IDENT:
        return printf("IDENT(%s)", token.data.ident);
      case TOK_RESERVED:
        switch (token.data.reserved) {
          case TOK_RES_TYPE:
            return printf("RESERVED(TYPE)");
        }
        break;
      case TOK_SYMBOL:
        switch (token.data.symbol) {
          case TOK_SYM_LPAREN:
            return printf("SYMBOL(LPAREN)");
          case TOK_SYM_RPAREN:
            return printf("SYMBOL(RPAREN)");
        }
      case TOK_EOF:
        return printf("EOF()");
   }
}

void print_whitespace(int amount) {
    for (int i = 0; i < amount; i++) {
        putchar(' ');
    }
}

bool test_lex(void) {
    const char *input =
        "  type( (\tfoobar\n) ";
    Token expected_output[] = {
          (Token){.tag = TOK_RESERVED, .data.reserved = TOK_RES_TYPE}
        , (Token){.tag = TOK_SYMBOL, .data.symbol = TOK_SYM_LPAREN}
        , (Token){.tag = TOK_SYMBOL, .data.symbol = TOK_SYM_LPAREN}
        , (Token){.tag = TOK_IDENT, .data.ident = "foobar"}
        , (Token){.tag = TOK_SYMBOL, .data.symbol = TOK_SYM_RPAREN}
        , (Token){.tag = TOK_EOF}
    };
    size_t output_len = sizeof(expected_output) / sizeof(*expected_output);
    Token output[output_len];

    TokenStream stream = token_stream_new(str_to_char_stream(input));

    for (size_t i = 0; i < output_len; i++) {
        output[i] = token_stream_pop(&stream);
    }

    token_stream_free(&stream);

    bool all_same = true;
    for (size_t i = 0; i < output_len; i++) {
        all_same &= token_cmp(output[i], expected_output[i]);
    }

    if (!all_same) {
        printf("Expected tokens do not match actual tokens.\n");
        printf("Expected vs. actual:\n");

        for (size_t i = 0; i < output_len; i++) {
            int printed = token_print(expected_output[i]);
            print_whitespace(40 - printed);
            token_print(output[i]);
            putchar('\n');
        }
    }

    for (size_t i = 0; i < output_len; i++) {
        token_free(output[i]);
    }

    return all_same;
}
