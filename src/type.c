#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dependent-c/ast.h"
#include "dependent-c/type.h"

/***** Top Level Dependency Analysis *****************************************/

/* Remove a symbol at an index from a symbol set. */
static void symbol_set_remove(size_t *set_size, const char ***set,
        size_t index) {
    assert(*set_size > index);

    memmove(*set + index, *set + index + 1,
        (*set_size - index - 1) * sizeof **set);
    *set = realloc(*set, (*set_size - 1) * sizeof **set);
    *set_size -= 1;
}

/* Remove a symbol from a symbol set if present. */
static void symbol_set_delete(size_t *set_size, const char ***set,
        const char *delete) {
    for (size_t i = 0; i < *set_size; i++) {
        if (delete == (*set)[i]) {
            symbol_set_remove(set_size, set, i);
            break;
        }
    }
}

/* Add a symbol to a symbol set if not present. */
static void symbol_set_add(size_t *set_size, const char ***set,
        const char *add) {
    bool unique = true;

    for (size_t i = 0; i < *set_size; i++) {
        if (add == (*set)[i]) {
            unique = false;
            break;
        }
    }

    if (unique) {
        *set = realloc(*set, (*set_size + 1) * sizeof **set);
        (*set)[*set_size] = add;
        *set_size += 1;
    }
}

/* Union two sets. The result is placed into the first set and the second
 * set is freed. */
static void symbol_set_union(size_t *set1_size, const char ***set1,
        size_t *set2_size, const char ***set2) {
    for (size_t i = 0; i < *set2_size; i++) {
        symbol_set_add(set1_size, set1, (*set2)[i]);
    }

    free(*set2);
    *set2 = NULL;
    *set2_size = 0;
}

/* Calculate the set of free variables in an expression. */
static void expr_free_vars(Expr expr, size_t *num_free, const char ***free) {
    size_t num_free_temp[1];
    const char **free_temp[1];

    switch (expr.tag) {
      case EXPR_LITERAL:
        *num_free = 0;
        *free = NULL;
        break;

      case EXPR_IDENT:
        *num_free = 1;
        *free = malloc(sizeof **free);
        **free = expr.data.ident;
        break;

      case EXPR_FUNC_TYPE:
        expr_free_vars(*expr.data.func_type.ret_type, num_free, free);
        for (size_t i = 0; i < expr.data.func_type.num_params; i++) {
            if (expr.data.func_type.param_names[i] != NULL) {
                symbol_set_delete(num_free, free,
                    expr.data.func_type.param_names[i]);
            }
        }
        for (size_t i = 0; i < expr.data.func_type.num_params; i++) {
            expr_free_vars(expr.data.func_type.param_types[i],
                num_free_temp, free_temp);
            for (size_t j = 0; j < i; j++) {
                if (expr.data.func_type.param_names[j] != NULL) {
                    symbol_set_delete(num_free_temp, free_temp,
                        expr.data.func_type.param_names[j]);
                }
            }
            symbol_set_union(num_free, free, num_free_temp, free_temp);
        }
        break;

      case EXPR_CALL:
        expr_free_vars(*expr.data.call.func, num_free, free);
        for (size_t i = 0; i < expr.data.call.num_args; i++) {
            expr_free_vars(expr.data.call.args[i], num_free_temp, free_temp);
            symbol_set_union(num_free, free, num_free_temp, free_temp);
        }
        break;

      case EXPR_STRUCT:
        *num_free = 0;
        *free = NULL;
        for (size_t i = 0; i < expr.data.struct_.num_fields; i++) {
            expr_free_vars(expr.data.struct_.field_types[i],
                num_free_temp, free_temp);
            for (size_t j = 0; j < i; j++) {
                symbol_set_delete(num_free_temp, free_temp,
                    expr.data.struct_.field_names[j]);
            }
            symbol_set_union(num_free, free, num_free_temp, free_temp);
        }
        break;

      case EXPR_UNION:
        *num_free = 0;
        *free = NULL;
        for (size_t i = 0; i < expr.data.union_.num_fields; i++) {
            expr_free_vars(expr.data.union_.field_types[i],
                num_free_temp, free_temp);
            symbol_set_union(num_free, free, num_free_temp, free_temp);
        }
        break;

      case EXPR_PACK:
        expr_free_vars(*expr.data.pack.type, num_free, free);
        for (size_t i = 0; i < expr.data.pack.num_assigns; i++) {
            expr_free_vars(expr.data.pack.assigns[i],
                num_free_temp, free_temp);
            symbol_set_union(num_free, free, num_free_temp, free_temp);
        }
        break;

      case EXPR_MEMBER:
        expr_free_vars(*expr.data.member.record, num_free, free);
        break;

      case EXPR_POINTER:
        expr_free_vars(*expr.data.pointer, num_free, free);
        break;

      case EXPR_REFERENCE:
        expr_free_vars(*expr.data.reference, num_free, free);
        break;

      case EXPR_DEREFERENCE:
        expr_free_vars(*expr.data.dereference, num_free, free);
        break;

      case EXPR_FUNC_TYPE_OR_CALL:
        expr_free_vars(*expr.data.func_type_or_call.ret_type_or_func,
            num_free, free);
        for (size_t i = 0; i < expr.data.func_type_or_call.num_params_or_args;
                i++) {
            if (expr.data.func_type_or_call.param_names[i] != NULL) {
                symbol_set_delete(num_free, free,
                    expr.data.func_type_or_call.param_names[i]);
            }
        }
        for (size_t i = 0; i < expr.data.func_type_or_call.num_params_or_args;
                i++) {
            expr_free_vars(expr.data.func_type_or_call.param_types_or_args[i],
                num_free_temp, free_temp);
            for (size_t j = 0; j < i; j++) {
                if (expr.data.func_type_or_call.param_names[j] != NULL) {
                    symbol_set_delete(num_free_temp, free_temp,
                        expr.data.func_type_or_call.param_names[j]);
                }
            }
            symbol_set_union(num_free, free, num_free_temp, free_temp);
        }
        break;
    }
}

static void block_free_vars(size_t len, Statement[len],
    size_t*, const char***);

static void statement_free_vars(Statement statement,
        size_t *num_free, const char ***free) {
    size_t num_free_temp[1];
    const char **free_temp[1];

    switch (statement.tag) {
      case STATEMENT_EMPTY:
        *num_free = 0;
        *free = NULL;
        break;

      case STATEMENT_EXPR:
        expr_free_vars(statement.data.expr, num_free, free);
        break;

      case STATEMENT_BLOCK:
        block_free_vars(statement.data.block.num_statements,
            statement.data.block.statements, num_free, free);
        break;

      case STATEMENT_DECL:
        expr_free_vars(statement.data.decl.type, num_free, free);
        if (statement.data.decl.is_initialized) {
            expr_free_vars(statement.data.decl.initial_value,
                num_free_temp, free_temp);
            symbol_set_union(num_free, free, num_free_temp, free_temp);
        }
        break;
    }
}

static void block_free_vars(size_t len, Statement block[len],
        size_t *num_free, const char ***free) {
    size_t num_free_temp[1];
    const char **free_temp[1];
    *num_free = 0;
    *free = NULL;

    for (size_t i = 0; i < len; i++) {
        Statement statement = block[len - i - 1];

        switch (statement.tag) {
          case STATEMENT_DECL:
            symbol_set_delete(num_free, free, statement.data.decl.name);
            break;

          case STATEMENT_EMPTY:
          case STATEMENT_EXPR:
          case STATEMENT_BLOCK:
            break;
        }

        statement_free_vars(statement, num_free_temp, free_temp);
        symbol_set_union(num_free, free, num_free_temp, free_temp);
    }
}

static void top_level_free_vars(TopLevel top_level,
        size_t *num_free, const char ***free) {
    size_t num_free_temp[1];
    const char **free_temp[1];

    switch (top_level.tag) {
      case TOP_LEVEL_FUNC:;
        Expr func_type = (Expr){
              .tag = EXPR_FUNC_TYPE
            , .data.func_type.ret_type = &top_level.data.func.ret_type
            , .data.func_type.num_params = top_level.data.func.num_params
            , .data.func_type.param_types = top_level.data.func.param_types
            , .data.func_type.param_names = top_level.data.func.param_names
        };
        expr_free_vars(func_type, num_free, free);
        block_free_vars(top_level.data.func.num_statements,
            top_level.data.func.statements, num_free_temp, free_temp);
        symbol_set_union(num_free, free, num_free_temp, free_temp);
        break;
    }
}

// Originally the plan was to do actual dependency analysis and topologically
// sort functions and other declarations so that they could be declared out
// of order. However further research has indicated that this is a hard problem
// in dependently typed languages.
//
// Roughly, the reason this is a hard problem relates to the use of function
// calls in types. A function call may be allowed in a type without the code
// for the function having been supplied yet, if reductions involving that
// function are unneeded. However determining if the reductions are needed
// is difficult and involves executing those very reductions.
//
// As a result, for now at least, this function simply checks that the order
// supplied is a valid topological sort rather topologically sorting the
// declarations itself.
bool top_level_topological_sort(size_t len, const TopLevel top_levels[len],
        size_t order[len]) {
    size_t num_bound = 0;
    const char **bound = NULL;
    bool result = true;

    for (size_t i = 0; i < len; i++) {
        size_t num_free;
        const char **free;
        top_level_free_vars(top_levels[i], &num_free, &free);

        for (size_t j = 0; j < num_bound; j++) {
            symbol_set_delete(&num_free, &free, bound[j]);
        }

        if (num_free > 0) {
            result = false;

            fprintf(stderr,
                "Declaration \"%s\" depends upon undeclared values ",
                top_levels[i].name);
            for (size_t j = 0; j < num_free; j++) {
                if (j == num_free - 1) {
                    fprintf(stderr, ", and ");
                } else if (j > 0) {
                    fprintf(stderr, ", ");
                }

                fprintf(stderr, "\"%s\"", free[j]);
            }
            fprintf(stderr, ".\n");
        }
    }

    if (result) {
        for (size_t i = 0; i < len; i++) {
            order[i] = i;
        }
    }

    return result;
}

bool type_check_is_kind(Expr expr) {
    // TODO: implement

    return false;
}
