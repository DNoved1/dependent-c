#ifndef DEPENDENT_C_STATEMENT
#define DEPENDENT_C_STATEMENT

#include "dependent-c/ast.h"

typedef enum {
      STATEMENT_BLOCK
    , STATEMENT_DECL
} StatementTag;

typedef struct Statement Statement;
struct Statement {
    StatementTag tag;
    union {
        struct {
            size_t num_statements;
            Statement *statements;
        } block;

        struct {
            Expr *type;
            char *name;
            bool is_initialized;
            Expr *initial_value;
        } decl;
    } data;
};

/* Free any resources associated with an expression. */
void statement_free(Statement statement);

#endif /* DEPENDENT_C_STATEMENT */
