#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "dependent-c/ast.h"
#include "dependent-c/general.h"
#include "dependent-c/memory.h"
#include "dependent-c/symbol_table.h"
#include "dependent-c/type.h"

/***** Type Checking / Inference *********************************************/
bool type_check(Context *context, const Expr *expr, const Expr *type) {
    Expr type2[1];

    if (!type_infer(context, expr, type2)) {
        return false;
    }

    bool ret_val = type_equal(context, type, type2);
    expr_free(type2);
    return ret_val;
}

static bool type_infer_literal(Context *context, const Literal *literal,
        Expr *result) {
    switch (literal->tag) {
      case LIT_TYPE:
      case LIT_VOID:
      case LIT_U64:
      case LIT_BOOL:
        *result = literal_expr_type;
        return true;

      case LIT_INTEGRAL:
        *result = (Expr){
              .tag = EXPR_LITERAL
            , .literal.tag = LIT_U64
        };
        return true;

      case LIT_BOOLEAN:
        *result = literal_expr_bool;
        return true;
    }
}

static bool type_infer_bin_op(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_BIN_OP);
    Expr op_type;

    // Since we have heterogeneous equality, this always creates a type.
    if (expr->bin_op.op == BIN_OP_ID) {
        *result = literal_expr_type;
        return true;
    }

    if (!type_infer(context, expr->bin_op.expr1, &op_type)) {
        return false;
    }

    if (!type_check(context, expr->bin_op.expr2, &op_type)) {
        fprintf(stderr, "Binary operator expressions must have the same "
            "type.\n");
        expr_free(&op_type);
        return false;
    }

    if (!op_type.tag == EXPR_LITERAL) {
        fprintf(stderr, "Binary operator expressions only operate on "
            "integral and boolean types.\n");
        expr_free(&op_type);
        return false;
    }

    bool is_integral = false, is_boolean = false, is_type = false;
    switch (op_type.literal.tag) {
      case LIT_U64:
        is_integral = true;
        break;

      case LIT_BOOL:
        is_boolean = true;
        break;

      case LIT_TYPE:
        is_type = true;
        break;

      case LIT_VOID:
      case LIT_INTEGRAL:
      case LIT_BOOLEAN:
        break;
    }

    switch (expr->bin_op.op) {
      case BIN_OP_ID: assert(false);

      case BIN_OP_EQ:
      case BIN_OP_NE:
        if (is_integral || is_boolean) {
            *result = literal_expr_bool;
            expr_free(&op_type);
            return true;
        } else {
            fprintf(stderr, "Equality expressions only operate on integral "
                "and boolean types.\n");
            expr_free(&op_type);
            return false;
        }

      case BIN_OP_LT:
      case BIN_OP_LTE:
      case BIN_OP_GT:
      case BIN_OP_GTE:
        if (is_integral) {
            *result = literal_expr_bool;
            expr_free(&op_type);
            return true;
        } else {
            fprintf(stderr, "Relational expressions only operate on integral "
                "type.\n");
            expr_free(&op_type);
            return false;
        }

      case BIN_OP_ADD:
      case BIN_OP_SUB:
        if (is_integral) {
            *result = op_type;
            return true;
        } else {
            fprintf(stderr, "Additive expressions only operate on integral "
                "types.\n");
            expr_free(&op_type);
            return false;
        }
    }
}

static bool type_infer_func_type(Context *context, const Expr *expr,
        Expr *result) {
    assert(expr->tag == EXPR_FUNC_TYPE);
    bool ret_val = true;

    symbol_table_enter_scope(&context->symbol_table);

    for (size_t i = 0; i < expr->func_type.num_params; i++) {
        if (type_check(context, &expr->func_type.param_types[i],
                &literal_expr_type)) {
            if (expr->func_type.param_names[i] != NULL) {
                symbol_table_register_local(&context->symbol_table,
                    expr->func_type.param_names[i],
                    expr->func_type.param_types[i]);
            }
        } else {
            ret_val = false;
            goto end_of_function;
        }
    }

    if (!type_check(context, expr->func_type.ret_type, &literal_expr_type)) {
        ret_val = false;
        goto end_of_function;
    }

    *result = literal_expr_type;

end_of_function:
    symbol_table_leave_scope(&context->symbol_table);
    return ret_val;
}

static bool type_infer_lambda(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_LAMBDA);

    symbol_table_enter_scope(&context->symbol_table);

    for (size_t i = 0; i < expr->lambda.num_params; i++) {
        if (type_check(context, &expr->lambda.param_types[i],
                &literal_expr_type)) {
            symbol_table_register_local(&context->symbol_table,
                expr->lambda.param_names[i], expr->lambda.param_types[i]);
        } else {
            symbol_table_leave_scope(&context->symbol_table);
            return false;
        }
    }

    symbol_table_enter_scope(&context->symbol_table);
    bool ret_val = type_infer(context, expr->lambda.body, result);
    symbol_table_leave_scope(&context->symbol_table);
    symbol_table_leave_scope(&context->symbol_table);
    return ret_val;
}

static bool type_infer_call(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_CALL);
    Expr func_type;

    if (!type_infer(context, expr->call.func, &func_type)) {
        return false;
    }

    if (func_type.tag != EXPR_FUNC_TYPE) {
        fprintf(stderr, "Cannot call non-function type (");
        expr_pprint(stderr, &func_type);
        fprintf(stderr, ").\n");

        expr_free(&func_type);
        return false;
    }

    if (func_type.func_type.num_params != expr->call.num_args) {
        fprintf(stderr, "Calling function which expects %zu parameters with "
            "%zu arguments.\n", func_type.func_type.num_params,
            expr->call.num_args);

        expr_free(&func_type);
        return false;
    }

    for (size_t i = 0; i < func_type.func_type.num_params; i++) {
        const Expr arg = expr->call.args[i];
        const char *param_name = func_type.func_type.param_names[i];

        if (type_check(context, &arg, &func_type.func_type.param_types[i])) {
            for (size_t j = i + 1; j < func_type.func_type.num_params; j++) {
                if (!expr_subst(context,
                        &func_type.func_type.param_types[j],
                        param_name, &arg)) {
                    expr_free(&func_type);
                    return false;
                }
            }

            if (!expr_subst(context, func_type.func_type.ret_type,
                    param_name, &arg)) {
                expr_free(&func_type);
                return false;
            }
        } else {
            expr_free(&func_type);
            return false;
        }
    }

    *result = expr_copy(func_type.func_type.ret_type);
    expr_free(&func_type);
    return true;
}

static bool type_infer_struct(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_STRUCT);
    bool ret_val = true;

    // Check that there are no duplicated field names
    for (size_t i = 0; i < expr->struct_.num_fields; i++) {
        for (size_t j = i + 1; j < expr->struct_.num_fields; j++) {
            if (expr->struct_.field_names[i] == expr->struct_.field_names[j]) {
                fprintf(stderr, "Structure declares field \"%s\" multiple "
                    "times.\n", expr->struct_.field_names[i]);
                return false;
            }
        }
    }

    symbol_table_enter_scope(&context->symbol_table);

    for (size_t i = 0; i < expr->struct_.num_fields; i++) {
        if (type_check(context, &expr->struct_.field_types[i],
                &literal_expr_type)) {
            symbol_table_register_local(&context->symbol_table,
                expr->struct_.field_names[i],
                expr->struct_.field_types[i]);
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

static bool type_infer_pack(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_PACK);

    if (expr->pack.type->tag == EXPR_STRUCT) {
        // Check that there are no duplicated assignments
        for (size_t i = 0; i < expr->pack.num_assigns; i++) {
            for (size_t j = i + 1; j < expr->pack.num_assigns; j++) {
                if (expr->pack.field_names[i] == expr->pack.field_names[j]) {
                    fprintf(stderr, "Assigning to field \"%s\" twice in packed "
                        "expression.\n", expr->pack.field_names[i]);
                    return false;
                }
            }
        }

        Expr struct_type = expr_copy(expr->pack.type);

        // Determine which fields are depended upon
        bool field_depended_upon[struct_type.struct_.num_fields];
        for (size_t i = 0; i < struct_type.struct_.num_fields; i++) {
            SymbolSet free_vars[1];
            expr_free_vars(&struct_type.struct_.field_types[i], free_vars);

            field_depended_upon[i] = 0;
            for (size_t j = 0; j < i; j++) {
                if (symbol_set_contains(free_vars,
                        struct_type.struct_.field_names[j])) {
                    field_depended_upon[j] = true;
                }
            }

            symbol_set_free(free_vars);
        }

        // Ensure that depended upon fields are assigned
        size_t depended_upon_assign_num[struct_type.struct_.num_fields];
        for (size_t i = 0; i < struct_type.struct_.num_fields; i++) {
            if (field_depended_upon[i]) {
                bool field_assigned = false;

                for (size_t j = 0; j < expr->pack.num_assigns; j++) {
                    if (struct_type.struct_.field_names[i]
                            == expr->pack.field_names[j]) {
                        field_assigned = true;
                        depended_upon_assign_num[i] = j;
                        break;
                    }
                }

                if (!field_assigned) {
                    fprintf(stderr, "Depended-upon field \"%s\" not assigned "
                        "in packed expression.\n",
                        struct_type.struct_.field_names[i]);
                    expr_free(&struct_type);
                    return false;
                }
            }
        }

        // Substitute dependent field types for their assigned values
        for (size_t i = 0; i < struct_type.struct_.num_fields; i++) {
            if (field_depended_upon[i]) {
                for (size_t j = i + 1; j < struct_type.struct_.num_fields;
                        j++) {
                    if (!expr_subst(context,
                            &struct_type.struct_.field_types[j],
                            struct_type.struct_.field_names[i],
                            &expr->pack.assigns[
                                depended_upon_assign_num[i]])) {
                        expr_free(&struct_type);
                        return false;
                    }
                }
            }
        }

        // Check that each assignment is of the correct type
        // (Also check that each assignment is to an existing field name)
        for (size_t i = 0; i < expr->pack.num_assigns; i++) {
            bool valid_field = false;

            for (size_t j = 0; j < struct_type.struct_.num_fields; j++) {
                if (expr->pack.field_names[i]
                        == struct_type.struct_.field_names[j]) {
                    if (type_check(context, &expr->pack.assigns[i],
                            &struct_type.struct_.field_types[j])) {
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
                    "exist in the struct.\n", expr->pack.field_names[i]);
                expr_free(&struct_type);
                return false;
            }
        }

        *result = struct_type;
        return true;

    } else if (expr->pack.type->tag == EXPR_UNION) {
        // Check that there is exactly one assignment
        if (expr->pack.num_assigns != 1) {
            fprintf(stderr, "Must assign exactly one field in a union.\n");
            return false;
        }

        // Check that that one assignment is to an existing field in the type
        size_t union_field_chosen;
        bool valid_field_chosen = false;
        for (size_t i = 0; i < expr->pack.type->union_.num_fields; i++) {
            if (expr->pack.type->union_.field_names[i]
                    == expr->pack.field_names[0]) {
                union_field_chosen = i;
                valid_field_chosen = true;
                break;
            }
        }
        if (!valid_field_chosen) {
            fprintf(stderr, "Assigning to field \"%s\" which does not exist "
                "in the union.\n", expr->pack.field_names[0]);
            return false;
        }

        // Check that assignment is of correct type
        if (type_check(context, &expr->pack.assigns[0],
                &expr->pack.type->union_.field_types[union_field_chosen])) {
            *result = expr_copy(expr->pack.type);
            return true;
        } else {
            return false;
        }

    } else {

        fprintf(stderr, "Cannot pack into non-struct and non-union type (");
        expr_pprint(stderr, expr->pack.type);
        fprintf(stderr, ").\n");

        return false;
    }
}

static bool type_infer_member(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_MEMBER);
    Expr record_type;

    if (!type_infer(context, expr->member.record, result)) {
        return false;
    }
    if (!type_eval(context, result, &record_type)) {
        record_type = expr_copy(result);
    }
    expr_free(result);

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

        for (size_t i = 0; i < record_type.struct_.num_fields; i++) {
            if (record_type.struct_.field_names[i] == expr->member.field) {
                *result = expr_copy(&record_type.struct_.field_types[i]);
                expr_free(&record_type);
                return true;
            }
        }

        fprintf(stderr, "Accessing field \"%s\" which does not exist in "
            "struct type (", expr->member.field);
        expr_pprint(stderr, &record_type);
        fprintf(stderr, ").\n");
        expr_free(&record_type);
        return false;

    } else if (record_type.tag == EXPR_UNION) {
        for (size_t i = 0; i < record_type.union_.num_fields; i++) {
            if (record_type.union_.field_names[i] == expr->member.field) {
                *result = expr_copy(&record_type.union_.field_types[i]);
                expr_free(&record_type);
                return true;
            }
        }

        fprintf(stderr, "Accessing field \"%s\" which does not exist in "
            "union type (", expr->member.field);
        expr_pprint(stderr, &record_type);
        fprintf(stderr, ").\n");
        expr_free(&record_type);
        return false;

    } else {
        fprintf(stderr, "Non-struct and non-union type (");
        expr_pprint(stderr, &record_type);
        fprintf(stderr, ") does not have field \"%s\".\n", expr->member.field);
        expr_free(&record_type);
        return false;
    }
}

bool type_infer(Context *context, const Expr *expr, Expr *result) {
    Expr temp[1];
    Expr temp2[1];

    switch (expr->tag) {
      case EXPR_LITERAL:
        return type_infer_literal(context, &expr->literal, result);

      case EXPR_IDENT:
        if (!symbol_table_lookup(&context->symbol_table, expr->ident, temp)) {
            fprintf(stderr, "Unbound symbol \"%s\".\n", expr->ident);
            location_pprint(context->source_name, &expr->location);
            return false;
        }
        *result = expr_copy(temp);
        return true;

      case EXPR_BIN_OP:
        return type_infer_bin_op(context, expr, result);

      case EXPR_IFTHENELSE:
        if (!type_check(context, expr->ifthenelse.predicate,
                &literal_expr_bool)) {
            return false;
        }
        if (!type_infer(context, expr->ifthenelse.then_, temp)) {
            return false;
        }
        if (!type_infer(context, expr->ifthenelse.else_, temp2)) {
            expr_free(temp);
            return false;
        }
        result->tag = EXPR_IFTHENELSE;
        alloc(result->ifthenelse.predicate);
        *result->ifthenelse.predicate = expr_copy(expr->ifthenelse.predicate);
        alloc(result->ifthenelse.then_);
        *result->ifthenelse.then_ = *temp;
        alloc(result->ifthenelse.else_);
        *result->ifthenelse.else_ = *temp2;
        return true;

      case EXPR_REFLEXIVE:
        if (!type_infer(context, expr->pointer, temp)) {
            return false;
        }
        result->tag = EXPR_BIN_OP;
        result->bin_op.op = BIN_OP_ID;
        alloc(result->bin_op.expr1);
        alloc(result->bin_op.expr2);
        *result->bin_op.expr1 = expr_copy(temp);
        *result->bin_op.expr2 = *temp;
        return true;

      case EXPR_FUNC_TYPE:
        return type_infer_func_type(context, expr, result);

      case EXPR_LAMBDA:
        return type_infer_lambda(context, expr, result);

      case EXPR_CALL:
        return type_infer_call(context, expr, result);

      case EXPR_STRUCT:
        return type_infer_struct(context, expr, result);

      case EXPR_UNION:
        for (size_t i = 0; i < expr->union_.num_fields; i++) {
            for (size_t j = i + 1; j < expr->union_.num_fields; j++) {
                if (expr->union_.field_names[i] == expr->union_.field_names[j]) {
                    fprintf(stderr, "Union declares field \"%s\" multiple "
                        "times.\n", expr->union_.field_names[i]);
                    return false;
                }
            }
        }
        for (size_t i = 0; i < expr->union_.num_fields; i++) {
            if (!type_check(context, &expr->union_.field_types[i],
                    &literal_expr_type)) {
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
        if (!type_check(context, expr->pointer, &literal_expr_type)) {
            return false;
        }
        *result = literal_expr_type;
        return true;

      case EXPR_REFERENCE:
        if (!type_infer(context, expr->pointer, temp)) {
            return false;
        }
        result->tag = EXPR_POINTER;
        alloc(result->pointer);
        *result->pointer = *temp;
        return true;

      case EXPR_DEREFERENCE:
        if (!type_infer(context, expr->pointer, temp)) {
            return false;
        }
        if (temp->tag != EXPR_POINTER) {
            return false;
        }
        *result = expr_copy(temp->pointer);
        expr_free(temp);
        return true;
    }
}

bool type_equal(Context *context, const Expr *type1, const Expr *type2) {
    // TODO, do alpha equivalence rather than simple structural equivalence.

    Expr type1_whnf[1], type2_whnf[1];
    if (!type_eval(context, type1, type1_whnf)) {
        *type1_whnf = expr_copy(type1);
    }
    if (!type_eval(context, type2, type2_whnf)) {
        *type2_whnf = expr_copy(type2);
    }

    bool ret_val = expr_equal(type1_whnf, type2_whnf);
    expr_free(type1_whnf);
    expr_free(type2_whnf);

    if (!ret_val) {
        fprintf(stderr, "Could not determine that (");
        expr_pprint(stderr, type1);
        fprintf(stderr, ") ~ (");
        expr_pprint(stderr, type2);
        fprintf(stderr, ").\n");
    }

    return ret_val;
}

static bool type_eval_bin_op(Context *context, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_BIN_OP);

    Expr reduced_expr1[1], reduced_expr2[1];

    if (!type_eval(context, type->bin_op.expr1, reduced_expr1)) {
        return false;
    }
    if (!type_eval(context, type->bin_op.expr2, reduced_expr2)) {
        expr_free(reduced_expr1);
        return false;
    }

    switch (type->bin_op.op) {
      case BIN_OP_ID:
        expr_free(reduced_expr1);
        expr_free(reduced_expr2);
        *result = expr_copy(type);
        return true;

      case BIN_OP_EQ:
        if (reduced_expr1->tag == EXPR_LITERAL
                && reduced_expr1->literal.tag == LIT_INTEGRAL
                && reduced_expr2->tag == EXPR_LITERAL
                && reduced_expr2->literal.tag == LIT_INTEGRAL) {
            *result = (Expr){
                  .tag = EXPR_LITERAL
                , .literal.tag = LIT_BOOLEAN
                , .literal.boolean = reduced_expr1->literal.integral
                        == reduced_expr2->literal.integral
            };
            expr_free(reduced_expr1);
            expr_free(reduced_expr2);
            return true;
        } else {
            expr_free(reduced_expr1);
            expr_free(reduced_expr2);
            *result = expr_copy(type);
            return true;
        }

      case BIN_OP_NE:
      case BIN_OP_LT:
      case BIN_OP_LTE:
      case BIN_OP_GT:
      case BIN_OP_GTE:
      case BIN_OP_ADD:
        expr_free(reduced_expr1);
        expr_free(reduced_expr2);
        *result = expr_copy(type);
        return true;

      case BIN_OP_SUB:
        if (reduced_expr1->tag == EXPR_LITERAL
                && reduced_expr1->literal.tag == LIT_INTEGRAL
                && reduced_expr2->tag == EXPR_LITERAL
                && reduced_expr2->literal.tag == LIT_INTEGRAL) {
            *result = (Expr){
                  .tag = EXPR_LITERAL
                , .literal.tag = LIT_INTEGRAL
                , .literal.integral = reduced_expr1->literal.integral
                        - reduced_expr2->literal.integral
            };
            expr_free(reduced_expr1);
            expr_free(reduced_expr2);
            return true;
        } else {
            expr_free(reduced_expr1);
            expr_free(reduced_expr2);
            *result = expr_copy(type);
            return true;
        }
    }
}

static bool type_eval_ifthenelse(Context *context, const Expr *type,
        Expr *result) {
    assert(type->tag == EXPR_IFTHENELSE);

    // If both sides of the if branch are equivalent we can reduce to that
    if (type_equal(context, type->ifthenelse.then_,
            type->ifthenelse.else_)) {
        return type_eval(context, type->ifthenelse.then_, result);
    } else {
        fprintf(stderr, "    While checking if both if-then-else branches "
            "have the same type.\n");
    }

    Expr reduced_cond[1];
    if (!type_eval(context, type->ifthenelse.predicate, reduced_cond)) {
        return false;
    }

    if (reduced_cond->tag == EXPR_LITERAL
            && reduced_cond->literal.tag == LIT_BOOLEAN) {
        if (reduced_cond->literal.boolean) {
            return type_eval(context, type->ifthenelse.then_, result);
        } else {
            return type_eval(context, type->ifthenelse.else_, result);
        }
    } else {
        expr_free(reduced_cond);
        return false;
    }
}

static bool type_eval_call(Context *context, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_CALL);

    Expr reduced_func[1];
    if (!type_eval(context, type->call.func, reduced_func)) {
        return false;
    }

    if (reduced_func->tag != EXPR_LAMBDA) {
        fprintf(stderr, "Cannot evaluate type further because it attempts "
            "to call non-function (");
        expr_pprint(stderr, reduced_func);
        fprintf(stderr, ").\n    Started with type (");
        expr_pprint(stderr, type);
        fprintf(stderr, ").\n");
        expr_free(reduced_func);
        return false;
    }

    // Just to make sure the arguments are of the correct type
    if (!type_infer_call(context, type, result)) {
        expr_free(reduced_func);
        return false;
    }
    expr_free(result);

    for (size_t i = 0; i < type->call.num_args; i++) {
        if (!expr_subst(context, reduced_func->lambda.body,
                reduced_func->lambda.param_names[i], &type->call.args[i])) {
            expr_free(reduced_func);
            return false;
        }
    }

    bool ret_val = type_eval(context, reduced_func->lambda.body, result);
    expr_free(reduced_func);
    return ret_val;
}

bool type_eval(Context *context, const Expr *type, Expr *result) {
    Expr temp[1];
    // TODO, better normalization, up to whnf.

    switch (type->tag) {
      case EXPR_LITERAL:
        *result = expr_copy(type);
        return true;

      case EXPR_IDENT:
        if (symbol_table_lookup_define(&context->symbol_table,
                type->ident, temp)) {
            return type_eval(context, temp, result);
        } else {
            *result = expr_copy(type);
            return true;
        }

      case EXPR_BIN_OP:
        return type_eval_bin_op(context, type, result);

      case EXPR_IFTHENELSE:
        return type_eval_ifthenelse(context, type, result);

      case EXPR_REFLEXIVE:
        *result = expr_copy(type);
        return true;

      case EXPR_FUNC_TYPE:
      case EXPR_LAMBDA:
        *result = expr_copy(type);
        return true;

      case EXPR_CALL:
        return type_eval_call(context, type, result);

      case EXPR_STRUCT:
      case EXPR_UNION:
      case EXPR_PACK:
      case EXPR_MEMBER:
      case EXPR_POINTER:
      case EXPR_REFERENCE:
      case EXPR_DEREFERENCE:
        *result = expr_copy(type);
        return true;
    }
}

bool type_check_top_level(Context *context, const TopLevel *top_level) {
    Expr temp[1];

    switch (top_level->tag) {
      case TOP_LEVEL_FUNC:;
        const Expr func_type = {
              .tag = EXPR_FUNC_TYPE
            , .func_type.ret_type = (Expr*)&top_level->func.ret_type
            , .func_type.num_params = top_level->func.num_params
            , .func_type.param_types = top_level->func.param_types
            , .func_type.param_names = top_level->func.param_names
        };
        const Expr lambda = {
              .tag = EXPR_LAMBDA
            , .lambda.num_params = top_level->func.num_params
            , .lambda.param_types = top_level->func.param_types
            , .lambda.param_names = top_level->func.param_names
            , .lambda.body = (Expr*)&top_level->func.body
        };
        if (!type_check(context, &func_type, &literal_expr_type)) {
            return false;
        }
        symbol_table_register_global(&context->symbol_table,
            top_level->name, func_type);
        symbol_table_define_global(&context->symbol_table,
            top_level->name, lambda);
        symbol_table_enter_scope(&context->symbol_table);
        for (size_t i = 0; i < top_level->func.num_params; i++) {
            symbol_table_register_local(&context->symbol_table,
                top_level->func.param_names[i],
                top_level->func.param_types[i]);
        }
        if (!type_infer(context, lambda.lambda.body, temp)) {
            symbol_table_leave_scope(&context->symbol_table);
            return false;
        }
        if (!type_equal(context, &top_level->func.ret_type, temp)) {
            expr_free(temp);
            symbol_table_leave_scope(&context->symbol_table);
            return false;
        }
        expr_free(temp);
        symbol_table_leave_scope(&context->symbol_table);
        return true;
    }
}
