#include <stdlib.h>

#include "dependent-c/lex.h"
#include "dependent-c/ast.h"
#include "dependent-c/general.h"

int yyparse(Context *);

int main(void) {
    Context context = context_new("<stdin>", file_to_char_stream(stdin));
    int ret_value = EXIT_SUCCESS;

    if (yyparse(&context) == 0) {
        translation_unit_pprint(stdout, context.ast);
    } else {
        ret_value = EXIT_FAILURE;
    }

    context_free(&context);

    return ret_value;
}
