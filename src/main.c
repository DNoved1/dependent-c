#include <stdlib.h>

#include "dependent-c/general.h"
#include "dependent-c/memory.h"

int yyparse(Context *);

int main(void) {
    Context ctx = context_new("<stdin>", file_to_char_stream(stdin));
    ctx.color_enabled = true;
    int ret_value = EXIT_SUCCESS;

    if (yyparse(&ctx) == 0) {
        printf("Parsed as:\n");
        translation_unit_pprint(&ctx, stdout, &ctx.ast);
        putchar('\n');

        for (size_t i = 0; i < ctx.ast.num_top_levels; i++) {
            if (!type_check_top_level(&ctx, &ctx.ast.top_levels[i])) {
                fprintf(stderr, "Failed to type check \"%s\".\n",
                    ctx.ast.top_levels[i].name);
                ret_value = EXIT_FAILURE;
            }
        }
    } else {
        ret_value = EXIT_FAILURE;
    }

    context_free(&ctx);

    putchar('\n');
    print_allocation_info(stdout);

    return ret_value;
}
