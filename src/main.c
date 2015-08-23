#include <stdlib.h>

#include "dependent-c/lex.h"
#include "dependent-c/ast.h"

int yyparse(TokenStream*, TranslationUnit*);

int main(void) {
    TokenStream stream = token_stream_new(file_to_char_stream(stdin));
    TranslationUnit unit;
    int ret_value = EXIT_SUCCESS;

    if (yyparse(&stream, &unit) == 0) {
        translation_unit_pprint(unit);
        translation_unit_free(unit);
    } else {
        ret_value = EXIT_FAILURE;
    }

    token_stream_free(&stream);

    return ret_value;
}
