#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dependent-c/ast.h"
#include "dependent-c/general.h"
#include "dependent-c/symbol_table.h"
#include "dependent-c/type.h"

/***** Top Level Dependency Analysis *****************************************/

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
      case STATEMENT_RETURN:
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
          case STATEMENT_RETURN:
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

/***** Type Checking / Inference *********************************************/
bool type_check(Context *context, Expr expr, Expr type) {
    Expr type2;

    if (!type_infer(context, expr, &type2)) {
        return false;
    }

    bool ret_val = type_equal(context, type, type2);
    expr_free(&type2);
    return ret_val;
}

static bool type_infer_literal(Context *context, Literal literal,
        Expr *result) {
    switch (literal.tag) {
      case LIT_TYPE:
      case LIT_VOID:
      case LIT_U8:
      case LIT_S8:
      case LIT_U16:
      case LIT_S16:
      case LIT_U32:
      case LIT_S32:
      case LIT_U64:
      case LIT_S64:
        *result = literal_expr_type;
        return true;

      case LIT_INTEGRAL:
        *result = (Expr){
              .tag = EXPR_LITERAL
            , .data.literal = (Literal){.tag = LIT_U64}
        };
        return true;
    }
}

static bool type_infer_func_type(Context *context, Expr expr, Expr *result) {
    assert(expr.tag == EXPR_FUNC_TYPE);
    bool ret_val = true;

    symbol_table_enter_scope(&context->symbol_table);

    for (size_t i = 0; i < expr.data.func_type.num_params; i++) {
        if (type_check(context, expr.data.func_type.param_types[i],
                literal_expr_type)) {
            if (expr.data.func_type.param_names[i] != NULL) {
                symbol_table_register_local(&context->symbol_table,
                    expr.data.func_type.param_names[i],
                    expr.data.func_type.param_types[i]);
            }
        } else {
            ret_val = false;
            goto end_of_function;
        }
    }

    if (!type_check(context, *expr.data.func_type.ret_type, literal_expr_type)) {
        ret_val = false;
        goto end_of_function;
    }

    *result = literal_expr_type;

end_of_function:
    symbol_table_leave_scope(&context->symbol_table);
    return ret_val;
}

static bool type_infer_call(Context *context, Expr expr, Expr *result) {
    assert(expr.tag == EXPR_CALL);
    Expr func_type;

    if (!type_infer(context, *expr.data.call.func, &func_type)) {
        return false;
    }

    if (func_type.tag != EXPR_FUNC_TYPE) {
        fprintf(stderr, "Cannot call non-function type (");
        expr_pprint(stderr, 0, func_type);
        fprintf(stderr, ").\n");

        expr_free(&func_type);
        return false;
    }

    if (func_type.data.func_type.num_params != expr.data.call.num_args) {
        fprintf(stderr, "Calling function which expects %zu parameters with "
            "%zu arguments.\n", func_type.data.func_type.num_params,
            expr.data.call.num_args);

        expr_free(&func_type);
        return false;
    }

    for (size_t i = 0; i < func_type.data.func_type.num_params; i++) {
        Expr arg = expr.data.call.args[i];
        const char *param_name = func_type.data.func_type.param_names[i];

        if (type_check(context, arg, func_type.data.func_type.param_types[i])) {
            for (size_t j = i + 1; j < func_type.data.func_type.num_params; j++) {
                if (!expr_subst(context,
                        &func_type.data.func_type.param_types[j],
                        param_name, arg)) {
                    expr_free(&func_type);
                    return false;
                }
            }

            if (!expr_subst(context, func_type.data.func_type.ret_type,
                    param_name, arg)) {
                expr_free(&func_type);
                return false;
            }
        } else {
            expr_free(&func_type);
            return false;
        }
    }

    *result = expr_copy(*func_type.data.func_type.ret_type);
    expr_free(&func_type);
    return true;
}

static bool type_infer_struct(Context *context, Expr expr, Expr *result) {
    assert(expr.tag == EXPR_STRUCT);
    bool ret_val = true;

    // Check that there are no duplicated field names
    for (size_t i = 0; i < expr.data.struct_.num_fields; i++) {
        for (size_t j = i + 1; j < expr.data.struct_.num_fields; j++) {
            if (expr.data.struct_.field_names[i]
                    == expr.data.struct_.field_names[j]) {
                fprintf(stderr, "Structure declares field \"%s\" multiple "
                    "times.\n", expr.data.struct_.field_names[i]);
                return false;
            }
        }
    }

    symbol_table_enter_scope(&context->symbol_table);

    for (size_t i = 0; i < expr.data.struct_.num_fields; i++) {
        if (type_check(context, expr.data.struct_.field_types[i],
                literal_expr_type)) {
            symbol_table_register_local(&context->symbol_table,
                expr.data.struct_.field_names[i],
                expr.data.struct_.field_types[i]);
        } else {
            ret_val = false;
            goto end_of_function;
        }
    }

    *result = literal_expr_type;

end_of_function:
    symbol_table_leave_scope(&context->symbol_table);
    return ret_val;
}

static bool type_infer_pack(Context *context, Expr expr, Expr *result) {
    assert(expr.tag == EXPR_PACK);

    if (expr.data.pack.type->tag == EXPR_STRUCT) {
        // Check that there are no duplicated assignments
        for (size_t i = 0; i < expr.data.pack.num_assigns; i++) {
            for (size_t j = i + 1; j < expr.data.pack.num_assigns; j++) {
                if (expr.data.pack.field_names[i]
                        == expr.data.pack.field_names[j]) {
                    fprintf(stderr, "Assigning to field \"%s\" twice in packed "
                        "expression.\n", expr.data.pack.field_names[i]);
                    return false;
                }
            }
        }

        Expr struct_type = expr_copy(*expr.data.pack.type);

        // Determine which fields are depended upon
        bool field_depended_upon[struct_type.data.struct_.num_fields];
        for (size_t i = 0; i < struct_type.data.struct_.num_fields; i++) {
            size_t num_free[1];
            const char **free_vars[1];
            expr_free_vars(struct_type.data.struct_.field_types[i],
                num_free, free_vars);

            field_depended_upon[i] = 0;
            for (size_t j = 0; j < i; j++) {
                if (symbol_set_contains(num_free, free_vars,
                        struct_type.data.struct_.field_names[j])) {
                    field_depended_upon[j] = true;
                }
            }
        }

        // Ensure that depended upon fields are assigned
        size_t depended_upon_assign_num[struct_type.data.struct_.num_fields];
        for (size_t i = 0; i < struct_type.data.struct_.num_fields; i++) {
            if (field_depended_upon[i]) {
                bool field_assigned = false;

                for (size_t j = 0; j < expr.data.pack.num_assigns; j++) {
                    if (struct_type.data.struct_.field_names[i]
                            == expr.data.pack.field_names[j]) {
                        field_assigned = true;
                        depended_upon_assign_num[i] = j;
                        break;
                    }
                }

                if (!field_assigned) {
                    fprintf(stderr, "Depended-upon field \"%s\" not assigned "
                        "in packed expression.\n",
                        struct_type.data.struct_.field_names[i]);
                    expr_free(&struct_type);
                    return false;
                }
            }
        }

        // Substitute dependent field types for their assigned values
        for (size_t i = 0; i < struct_type.data.struct_.num_fields; i++) {
            if (field_depended_upon[i]) {
                for (size_t j = i + 1; j < struct_type.data.struct_.num_fields;
                        j++) {
                    if (!expr_subst(context,
                            &struct_type.data.struct_.field_types[j],
                            struct_type.data.struct_.field_names[i],
                            expr.data.pack.assigns[
                                depended_upon_assign_num[i]])) {
                        expr_free(&struct_type);
                        return false;
                    }
                }
            }
        }

        // Check that each assignment is of the correct type
        // (Also check that each assignment is to an existing field name)
        for (size_t i = 0; i < expr.data.pack.num_assigns; i++) {
            bool valid_field = false;

            for (size_t j = 0; j < struct_type.data.struct_.num_fields; j++) {
                if (expr.data.pack.field_names[i]
                        == struct_type.data.struct_.field_names[j]) {
                    if (type_check(context, expr.data.pack.assigns[i],
                            struct_type.data.struct_.field_types[j])) {
                        valid_field = true;
                        break;
                    } else {
                        expr_free(&struct_type);
                        return false;
                    }
                }
            }

            if (!valid_field) {
                fprintf(stderr, "Assigning to field \"%s\" which does not "
                    "exist in the struct.\n", expr.data.pack.field_names[i]);
                expr_free(&struct_type);
                return false;
            }
        }

        *result = struct_type;
        return true;

    } else if (expr.data.pack.type->tag == EXPR_UNION) {
        // Check that there is exactly one assignment
        if (expr.data.pack.num_assigns != 1) {
            fprintf(stderr, "Must assign exactly one field in a union.\n");
            return false;
        }

        // Check that that one assignment is to an existing field in the type
        size_t union_field_chosen;
        bool valid_field_chosen = false;
        for (size_t i = 0; i < expr.data.pack.type->data.union_.num_fields; i++) {
            if (expr.data.pack.type->data.union_.field_names[i]
                    == expr.data.pack.field_names[0]) {
                union_field_chosen = i;
                valid_field_chosen = true;
                break;
            }
        }
        if (!valid_field_chosen) {
            fprintf(stderr, "Assigning to field \"%s\" which does not exist "
                "in the union.\n", expr.data.pack.field_names[0]);
            return false;
        }

        // Check that assignment is of correct type
        if (type_check(context, expr.data.pack.assigns[0],
                expr.data.pack.type->data.union_.field_types[
                    union_field_chosen])) {
            *result = expr_copy(*expr.data.pack.type);
            return true;
        } else {
            return false;
        }

    } else {

        fprintf(stderr, "Cannot pack into non-struct and non-union type (");
        expr_pprint(stderr, 0, *expr.data.pack.type);
        fprintf(stderr, ").\n");

        return false;
    }
}

static bool type_infer_member(Context *context, Expr expr, Expr *result) {
    assert(expr.tag == EXPR_MEMBER);
    Expr record_type;

    if (!type_infer(context, *expr.data.member.record, &record_type)) {
        return false;
    }

    if (record_type.tag == EXPR_STRUCT) {
        // TODO: in the case that this is a dependent field being accessed we
        // need to substitute all field names X for (record).X. However,
        // evaluating record may result in side effects so we can't do that
        // directly. Instead we'll need to do something like
        //   let fresh_var = record in
        //   let X = fresh_var.X in
        //   let Y = fresh_var.Y in
        //   ... so on for all depended upon fields ...
        //   typeof( fresh_var.field_being_accessed )
        // We don't have a a let expression at the moment, so this will just
        // not work for now.

        for (size_t i = 0; i < record_type.data.struct_.num_fields; i++) {
            if (record_type.data.struct_.field_names[i]
                    == expr.data.member.field) {
                *result = expr_copy(record_type.data.struct_.field_types[i]);
                expr_free(&record_type);
                return true;
            }
        }

        fprintf(stderr, "Accessing field \"%s\" which does not exist in "
            "struct type (", expr.data.member.field);
        expr_pprint(stderr, 0, record_type);
        fprintf(stderr, ").\n");
        expr_free(&record_type);
        return false;

    } else if (record_type.tag == EXPR_UNION) {
        for (size_t i = 0; i < record_type.data.union_.num_fields; i++) {
            if (record_type.data.union_.field_names[i]
                    == expr.data.member.field) {
                *result = expr_copy(record_type.data.union_.field_types[i]);
                expr_free(&record_type);
                return true;
            }
        }

        fprintf(stderr, "Accessing field \"%s\" which does not exist in "
            "union type (", expr.data.member.field);
        expr_pprint(stderr, 0, record_type);
        fprintf(stderr, ").\n");
        expr_free(&record_type);
        return false;

    } else {
        fprintf(stderr, "Non-struct and non-union type (");
        expr_pprint(stderr, 0, record_type);
        fprintf(stderr, ") does not have field \"%s\".\n",
            expr.data.member.field);
        expr_free(&record_type);
        return false;
    }
}

bool type_infer(Context *context, Expr expr, Expr *result) {
    Expr temp;

    switch (expr.tag) {
      case EXPR_LITERAL:
        return type_infer_literal(context, expr.data.literal, result);

      case EXPR_IDENT:
        if (!symbol_table_lookup(&context->symbol_table,
                expr.data.ident, &temp)) {
            return false;
        }
        *result = expr_copy(temp);
        return true;

      case EXPR_FUNC_TYPE:
        return type_infer_func_type(context, expr, result);

      case EXPR_CALL:
        return type_infer_call(context, expr, result);

      case EXPR_STRUCT:
        return type_infer_struct(context, expr, result);

      case EXPR_UNION:
        for (size_t i = 0; i < expr.data.union_.num_fields; i++) {
            for (size_t j = i + 1; j < expr.data.union_.num_fields; j++) {
                if (expr.data.union_.field_names[i]
                        == expr.data.union_.field_names[j]) {
                    fprintf(stderr, "Union declares field \"%s\" multiple "
                        "times.\n", expr.data.union_.field_names[i]);
                    return false;
                }
            }
        }
        for (size_t i = 0; i < expr.data.union_.num_fields; i++) {
            if (!type_check(context, expr.data.union_.field_types[i],
                    literal_expr_type)) {
                return false;
            }
        }
        *result = literal_expr_type;
        return true;

      case EXPR_PACK:
        return type_infer_pack(context, expr, result);

      case EXPR_MEMBER:
        return type_infer_member(context, expr, result);

      case EXPR_POINTER:
        if (!type_check(context, *expr.data.pointer, literal_expr_type)) {
            return false;
        }
        *result = literal_expr_type;
        return true;

      case EXPR_REFERENCE:
        if (!type_infer(context, *expr.data.reference, &temp)) {
            return false;
        }
        result->tag = EXPR_POINTER;
        result->data.pointer = malloc(sizeof temp);
        *result->data.pointer = temp;
        return true;

      case EXPR_DEREFERENCE:
        if (!type_infer(context, *expr.data.dereference, &temp)) {
            return false;
        }
        if (temp.tag != EXPR_POINTER) {
            return false;
        }
        *result = expr_copy(*temp.data.pointer);
        expr_free(&temp);
        return true;
    }
}

bool type_equal(Context *context, Expr type1, Expr type2) {
    // TODO, do normalization and alpha equivalence rather than simple
    // structural equivalence.

    if (expr_equal(type1, type2)) {
        return true;
    } else {
        fprintf(stderr, "Could not determine that (");
        expr_pprint(stderr, 0, type1);
        fprintf(stderr, ") ~ (");
        expr_pprint(stderr, 0, type2);
        fprintf(stderr, ").\n");
        return false;
    }
}

static bool type_check_statement(Context *context, Statement statement,
        Expr ret_type, bool *returns) {
    Expr temp;
    *returns = false;
    bool returns_temp;

    switch (statement.tag) {
      case STATEMENT_EMPTY:
        return true;

      case STATEMENT_EXPR:
        if (!type_infer(context, statement.data.expr, &temp)) {
            return false;
        }
        expr_free(&temp);
        return true;

      case STATEMENT_RETURN:
        *returns = true;
        return type_check(context, statement.data.expr, ret_type);

      case STATEMENT_BLOCK:
        symbol_table_enter_scope(&context->symbol_table);
        for (size_t i = 0; i < statement.data.block.num_statements; i++) {
            if (!type_check_statement(context,
                    statement.data.block.statements[i],
                    ret_type, &returns_temp)) {
                symbol_table_leave_scope(&context->symbol_table);
                return false;
            }
            *returns |= returns_temp;
            if (returns_temp && i != statement.data.block.num_statements - 1) {
                fprintf(stderr, "Warning: Dead code.\n");
            }
        }
        symbol_table_leave_scope(&context->symbol_table);
        return true;

      case STATEMENT_DECL:
        if (!type_check(context, statement.data.decl.type, literal_expr_type)) {
            return false;
        }
        if (statement.data.decl.is_initialized) {
            if (!type_check(context, statement.data.decl.initial_value,
                    statement.data.decl.type)) {
                return false;
            }
        }
        symbol_table_register_local(&context->symbol_table,
            statement.data.decl.name, statement.data.decl.type);
        return true;
    }
}

bool type_check_top_level(Context *context, TopLevel top_level) {
    switch (top_level.tag) {
      case TOP_LEVEL_FUNC:;
        Expr func_type = (Expr) {
              .tag = EXPR_FUNC_TYPE
            , .data.func_type.ret_type = &top_level.data.func.ret_type
            , .data.func_type.num_params = top_level.data.func.num_params
            , .data.func_type.param_types = top_level.data.func.param_types
            , .data.func_type.param_names = top_level.data.func.param_names
        };
        if (!type_check(context, func_type, literal_expr_type)) {
            return false;
        }
        symbol_table_register_global(&context->symbol_table,
            top_level.name, func_type);
        symbol_table_enter_scope(&context->symbol_table);
        for (size_t i = 0; i < top_level.data.func.num_params; i++) {
            symbol_table_register_local(&context->symbol_table,
                top_level.data.func.param_names[i],
                top_level.data.func.param_types[i]);
        }
        bool returns = false, returns_temp;
        for (size_t i = 0; i < top_level.data.func.num_statements; i++) {
            if (!type_check_statement(context,
                    top_level.data.func.statements[i],
                    top_level.data.func.ret_type,
                    &returns_temp)) {
                symbol_table_leave_scope(&context->symbol_table);
                return false;
            }
            returns |= returns_temp;
            if (returns_temp && i != top_level.data.func.num_statements - 1) {
                fprintf(stderr, "Warning: Dead code.\n");
            }
        }
        symbol_table_leave_scope(&context->symbol_table);
        if (!returns) {
            fprintf(stderr, "Function \"%s\" does not return.\n",
                top_level.name);
            return false;
        }
        return true;
    }
}
