#include <stdlib.h>

#include "dependent-c/lex.h"
#include "dependent-c/ast.h"
#include "dependent-c/general.h"
#include "dependent-c/memory.h"
#include "dependent-c/type.h"

int yyparse(Context *);

int main(void) {
    Context context = context_new("<stdin>", file_to_char_stream(stdin));
    int ret_value = EXIT_SUCCESS;

    if (yyparse(&context) == 0) {
        printf("Parsed as:\n");
        translation_unit_pprint(stdout, context.ast);
        putchar('\n');

        for (size_t i = 0; i < context.ast.num_top_levels; i++) {
            if (!type_check_top_level(&context, context.ast.top_levels[i])) {
                fprintf(stderr, "Failed to type check \"%s\".\n",
                    context.ast.top_levels[i].name);
                ret_value = EXIT_FAILURE;
            }
        }
    } else {
        ret_value = EXIT_FAILURE;
    }

    context_free(&context);

    putchar('\n');
    print_allocation_info(stdout);

    return ret_value;
}
