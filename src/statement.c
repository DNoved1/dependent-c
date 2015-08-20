#include <stdlib.h>

#include "dependent-c/expr.h"
#include "dependent-c/statement.h"

void statement_free(Statement statement) {
    switch (statement.tag) {
      case STATEMENT_BLOCK:
        for (size_t i = 0; i < statement.data.block.num_statements; i++) {
            statement_free(statement.data.block.statements[i]);
        }
        free(statement.data.block.statements);
        break;

      case STATEMENT_DECL:
        expr_free(*statement.data.decl.type);
        free(statement.data.decl.type);
        free(statement.data.decl.name);
        if (statement.data.decl.is_initialized) {
            free(statement.data.decl.initial_value);
        }
        break;
    }
}
