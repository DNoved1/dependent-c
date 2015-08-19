#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    putchar('\n');

    bool test_lex(void);
    printf("Testing lexing.\n");
    if (!test_lex()) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
