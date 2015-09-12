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

static bool type_infer_forall(Context *context, const Expr *expr,
        Expr *result) {
    assert(expr->tag == EXPR_FORALL);
    bool ret_val = false;

    symbol_table_enter_scope(&context->symbol_table);

    for (size_t i = 0; i < expr->forall.num_params; i++) {
        if (type_check(context, &expr->forall.param_types[i],
                &literal_expr_type)) {
            if (expr->forall.param_names[i] != NULL) {
                symbol_table_register_local(&context->symbol_table,
                    expr->forall.param_names[i], expr->forall.param_types[i]);
            }
        } else {
            goto end_of_function;
        }
    }

    if (!type_check(context, expr->forall.ret_type, &literal_expr_type)) {
        goto end_of_function;
    }

    *result = literal_expr_type;
    ret_val = true;

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

    Expr ret_type;
    symbol_table_enter_scope(&context->symbol_table);
    bool ret_val = type_infer(context, expr->lambda.body, &ret_type);
    symbol_table_leave_scope(&context->symbol_table);
    symbol_table_leave_scope(&context->symbol_table);

    if (!ret_val) {
        return false;
    }

    result->tag = EXPR_FORALL;
    result->forall.num_params = expr->lambda.num_params;
    alloc_array(result->forall.param_types, result->forall.num_params);
    alloc_array(result->forall.param_names, result->forall.num_params);
    for (size_t i = 0; i < result->forall.num_params; i++) {
        result->forall.param_types[i] = expr_copy(&expr->lambda.param_types[i]);
        result->forall.param_names[i] = expr->lambda.param_names[i];
    }
    alloc_assign(result->forall.ret_type, ret_type);
    return true;
}

static bool type_infer_call(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_CALL);
    Expr forall;

    if (!type_infer(context, expr->call.func, &forall)) {
        return false;
    }

    if (forall.tag != EXPR_FORALL) {
        fprintf(stderr, "Cannot call non-function type (");
        expr_pprint(stderr, &forall);
        fprintf(stderr, ").\n");

        expr_free(&forall);
        return false;
    }

    if (forall.forall.num_params != expr->call.num_args) {
        fprintf(stderr, "Calling function which expects %zu parameters with "
            "%zu arguments.\n", forall.forall.num_params,
            expr->call.num_args);

        expr_free(&forall);
        return false;
    }

    for (size_t i = 0; i < forall.forall.num_params; i++) {
        const Expr arg = expr->call.args[i];
        const char *param_name = forall.forall.param_names[i];

        if (type_check(context, &arg, &forall.forall.param_types[i])) {
            for (size_t j = i + 1; j < forall.forall.num_params; j++) {
                expr_subst(context, &forall.forall.param_types[j],
                    param_name, &arg);
            }

            expr_subst(context, forall.forall.ret_type, param_name, &arg);
        } else {
            expr_free(&forall);
            return false;
        }
    }

    *result = expr_copy(forall.forall.ret_type);
    expr_free(&forall);
    return true;
}

static bool type_infer_substitute(Context *context, const Expr *expr,
        Expr *result) {
    assert(expr->tag == EXPR_SUBSTITUTE);
    bool ret_val = false;

    // Initialize to trivially freeable values for easy cleanup
    Expr proof_type = literal_expr_type;
    Expr family_type = literal_expr_type;
    Expr instance_type = literal_expr_type;
    Expr value_type = literal_expr_type;

    if (!type_infer(context, expr->substitute.proof, &proof_type)
            ||!type_infer(context, expr->substitute.family, &family_type)
            || !type_infer(context, expr->substitute.instance, &instance_type)) {
        goto end_of_function;
    }

    if (proof_type.tag != EXPR_ID) {
        efprintf(stderr, "Cannot substitute with non-identity type ($e).\n",
            ewrap(&proof_type));
        goto end_of_function;
    }

    if (!type_infer(context, proof_type.id.expr1, &value_type)) {
        goto end_of_function;
    }

    // Assume that the proof type given to us is valid, in the sense that both
    // sides have the same type.

    const Expr expected_family = {
          .tag = EXPR_FORALL
        , .forall.num_params = 1
        , .forall.param_types = (Expr[]){value_type}
        , .forall.param_names = (const char*[]){NULL}
        , .forall.ret_type = &literal_expr_type
    };

    if (!type_equal(context, &expected_family, &family_type)) {
        goto end_of_function;
    }

    const Expr expected_instance = {
          .tag = EXPR_CALL
        , .call.func = expr->substitute.family
        , .call.num_args = 1
        , .call.args = (Expr[]){*proof_type.id.expr1}
    };

    if (!type_equal(context, &expected_instance, &instance_type)) {
        goto end_of_function;
    }

    result->tag = EXPR_CALL;
    *result->call.func = expr_copy(expr->substitute.family);
    result->call.num_args = 1;
    alloc_array(result->call.args, 1);
    result->call.args[0] = expr_copy(proof_type.id.expr2);
    ret_val = true;

end_of_function:
    expr_free(&proof_type);
    expr_free(&family_type);
    expr_free(&instance_type);
    expr_free(&value_type);
    return ret_val;
}

static bool type_infer_nat_ind(Context *context, const Expr *expr,
        Expr *result) {
    assert(expr->tag == EXPR_NAT_IND);

    if (!type_check(context, expr->nat_ind.natural, &literal_expr_nat)) {
        fprintf(stderr, "Cannot perform natural induction on non-natural "
            "type.\n");
        return false;
    }

    if (!type_infer(context, expr->nat_ind.base_val, result)) {
        return false;
    }

    symbol_table_enter_scope(&context->symbol_table);
    symbol_table_register_local(&context->symbol_table,
        expr->nat_ind.ind_name, literal_expr_nat);

    if (!type_check(context, expr->nat_ind.ind_val, result)) {
        expr_free(result);
        symbol_table_leave_scope(&context->symbol_table);
        return false;
    }

    symbol_table_leave_scope(&context->symbol_table);
    return true;
}

static bool type_infer_sigma(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_SIGMA);
    bool ret_val = false;

    symbol_table_enter_scope(&context->symbol_table);

    for (size_t i = 0; i < expr->sigma.num_fields; i++) {
        if (type_check(context, &expr->sigma.field_types[i],
                &literal_expr_type)) {
            if (expr->sigma.field_names[i] != NULL) {
                symbol_table_register_local(&context->symbol_table,
                    expr->sigma.field_names[i], expr->sigma.field_types[i]);
            }
        } else {
            goto end_of_function;
        }
    }

    *result = literal_expr_type;
    ret_val = true;

end_of_function:
    symbol_table_leave_scope(&context->symbol_table);
    return ret_val;
}

static bool type_infer_pack(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_PACK);

    if (expr->pack.as_type == NULL) {
        Expr sigma;
        sigma.tag = EXPR_SIGMA;
        sigma.sigma.num_fields = expr->pack.num_fields;
        alloc_array(sigma.sigma.field_names, sigma.sigma.num_fields);
        alloc_array(sigma.sigma.field_types, sigma.sigma.num_fields);

        // Initialize all field types to a trivially freeable value
        for (size_t i = 0; i < sigma.sigma.num_fields; i++) {
            sigma.sigma.field_names[i] = NULL;
            sigma.sigma.field_types[i] = literal_expr_type;
        }

        for (size_t i = 0; i < expr->pack.num_fields; i++) {
            if (!type_infer(context, &expr->pack.field_values[i],
                    &sigma.sigma.field_types[i])) {
                expr_free(&sigma);
                return false;
            }
        }

        *result = sigma;
        return true;
    } else {
        if (expr->pack.as_type->tag != EXPR_SIGMA) {
            efprintf(stderr, "Cannot cast tuple to non-sigma type ($e).\n",
                ewrap(expr->pack.as_type));
            return false;
        }

        if (expr->pack.as_type->sigma.num_fields != expr->pack.num_fields) {
            fprintf(stderr, "Cannot case tuple with %zu fields to sigma with "
                "%zu fields.\n", expr->pack.num_fields,
                expr->pack.as_type->sigma.num_fields);
        }

        Expr sigma = expr_copy(expr->pack.as_type);

        for (size_t i = 0; i < expr->pack.num_fields; i++) {
            if (!type_check(context, &expr->pack.field_values[i],
                    &sigma.sigma.field_types[i])) {
                expr_free(&sigma);
                return false;
            }

            for (size_t j = i + 1; j < sigma.sigma.num_fields; j++) {
                expr_subst(context, &sigma.sigma.field_types[j],
                    sigma.sigma.field_names[i], &expr->pack.field_values[i]);
            }
        }

        *result = sigma;
        return true;
    }
}

static bool type_infer_access(Context *context, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_ACCESS);
    bool ret_val;

    Expr sigma_expr, sigma;
    if (!type_infer(context, expr->access.record, &sigma_expr)) {
        return false;
    }
    ret_val = type_eval(context, &sigma_expr, &sigma);
    expr_free(&sigma_expr);
    if (!ret_val) {
        return false;
    }

    if (sigma.tag != EXPR_SIGMA) {
        efprintf(stderr, "Cannot access field of non-sigma type ($e).\n",
            ewrap(&sigma));
        expr_free(&sigma);
        return false;
    }

    if (expr->access.field_num >= sigma.sigma.num_fields) {
        fprintf(stderr, "Cannot access field #%zu of sigma with only "
            "%zu fields.\n", expr->access.field_num, sigma.sigma.num_fields);
        expr_free(&sigma);
        return false;
    }

    *result = expr_copy(&sigma.sigma.field_types[expr->access.field_num]);

    for (size_t i = 0; i < expr->access.field_num; i++) {
        const char *field_name = sigma.sigma.field_names[i];

        if (field_name != NULL) {
            const Expr replacement = {
                  .tag = EXPR_ACCESS
                , .access.record = expr->access.record
                , .access.field_num = i
            };

            expr_subst(context, result, field_name, &replacement);
        }
    }

    expr_free(&sigma);
    return true;
}

bool type_infer(Context *context, const Expr *expr, Expr *result) {
    Expr temp[1];
    Expr temp2[1];

    // TODO: dependent elimination of booleans and naturals
    switch (expr->tag) {
      case EXPR_TYPE:
      case EXPR_VOID:
      case EXPR_BOOL:
      case EXPR_NAT:
        *result = literal_expr_type;
        return true;

      case EXPR_BOOLEAN:
        *result = literal_expr_bool;
        return true;

      case EXPR_NATURAL:
        *result = literal_expr_nat;
        return true;

      case EXPR_IDENT:
        if (!symbol_table_lookup(&context->symbol_table, expr->ident, temp)) {
            fprintf(stderr, "Unbound symbol \"%s\".\n", expr->ident);
            location_pprint(context->source_name, &expr->location);
            return false;
        }
        *result = expr_copy(temp);
        return true;

      case EXPR_FORALL:
        return type_infer_forall(context, expr, result);

      case EXPR_LAMBDA:
        return type_infer_lambda(context, expr, result);

      case EXPR_CALL:
        return type_infer_call(context, expr, result);

      case EXPR_ID:
        if (!type_infer(context, expr->id.expr1, temp)) {
            return false;
        }
        if (!type_infer(context, expr->id.expr2, temp2)) {
            expr_free(temp);
            return false;
        }
        if (!type_equal(context, temp, temp2)) {
            efprintf(stderr, "Cannot create an identity type for unequal "
                "types ($e) and ($e).\n", ewrap(temp, temp2));
            expr_free(temp);
            expr_free(temp2);
            return false;
        }
        expr_free(temp);
        expr_free(temp2);
        *result = literal_expr_type;
        return true;

      case EXPR_REFLEXIVE:
        if (!type_infer(context, expr->reflexive, temp)) {
            return false;
        }
        result->tag = EXPR_ID;
        alloc_assign(result->id.expr1, expr_copy(temp));
        alloc_assign(result->id.expr2, *temp);
        return true;

      case EXPR_SUBSTITUTE:
        return type_infer_substitute(context, expr, result);

      case EXPR_EXPLODE:
        if (!type_check(context, expr->explode.void_instance,
                    &literal_expr_void)
                || !type_check(context, expr->explode.into_type,
                    &literal_expr_type)) {
            return false;
        }
        *result = expr_copy(expr->explode.into_type);
        return true;

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
        alloc_assign(result->ifthenelse.predicate,
            expr_copy(expr->ifthenelse.predicate));
        alloc_assign(result->ifthenelse.then_, *temp);
        alloc_assign(result->ifthenelse.else_, *temp2);
        return true;

      case EXPR_NAT_IND:
        return type_infer_nat_ind(context, expr, result);

      case EXPR_SIGMA:
        return type_infer_sigma(context, expr, result);

      case EXPR_PACK:
        return type_infer_pack(context, expr, result);

      case EXPR_ACCESS:
        return type_infer_access(context, expr, result);
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

static bool type_eval_call(Context *context, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_CALL);

    Expr reduced_func[1];
    if (!type_eval(context, type->call.func, reduced_func)) {
        return false;
    }

    if (reduced_func->tag != EXPR_LAMBDA) {
        efprintf(stderr, "Cannot evaluate type further because it attempts "
            "to call non-function ($e).\n"
            "    Started with type ($e).\n",
            ewrap(reduced_func, type));
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
        expr_subst(context, reduced_func->lambda.body,
            reduced_func->lambda.param_names[i], &type->call.args[i]);
    }

    bool ret_val = type_eval(context, reduced_func->lambda.body, result);
    expr_free(reduced_func);
    return ret_val;
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

    if (reduced_cond->tag == EXPR_BOOLEAN) {
        if (reduced_cond->boolean) {
            return type_eval(context, type->ifthenelse.then_, result);
        } else {
            return type_eval(context, type->ifthenelse.else_, result);
        }
    } else {
        expr_free(reduced_cond);
        return false;
    }
}

static bool type_eval_substitute(Context *context, const Expr *type,
        Expr *result) {
    assert(type->tag == EXPR_SUBSTITUTE);

    Expr reduced_refl;
    if (!type_eval(context, type->substitute.proof, &reduced_refl)) {
        return false;
    }

    if (reduced_refl.tag != EXPR_REFLEXIVE) {
        efprintf(stderr, "Cannot substitute with non-literal reflexive proof "
            "($e).\n", ewrap(&reduced_refl));
        expr_free(&reduced_refl);
        return false;
    }

    *result = expr_copy(type->substitute.instance);
    expr_free(&reduced_refl);
    return true;
}

static bool type_eval_explode(Context *context, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_EXPLODE);

    efprintf(stderr, "Cannot reduce explosion ($e).\n", ewrap(type));
    return false;
}

static bool type_eval_nat_ind(Context *context, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_NAT_IND);

    Expr reduced_nat;
    if (!type_eval(context, type->nat_ind.natural, &reduced_nat)) {
        return false;
    }

    if (reduced_nat.tag == EXPR_NATURAL) {
        if (reduced_nat.natural == (type->nat_ind.goes_down ? 0 : UINT64_MAX)) {
            bool ret_val = type_eval(context, type->nat_ind.base_val, result);
            expr_free(&reduced_nat);
            return ret_val;
        } else {
            Expr ind_val = expr_copy(type->nat_ind.ind_val);
            const Expr replacement = {
                  .tag = EXPR_NATURAL
                , .natural = reduced_nat.natural
                    + (type->nat_ind.goes_down ? -1 : +1)
            };
            expr_subst(context, &ind_val, type->nat_ind.ind_name,
                &replacement);
            bool ret_val = type_eval(context, &ind_val, result);
            expr_free(&reduced_nat);
            expr_free(&ind_val);
            return ret_val;
        }
    } else {
        efprintf(stderr, "Cannot evaluate natural induction with non-literal "
            "natural ($e).\n", ewrap(&reduced_nat));
        expr_free(&reduced_nat);
        return false;
    }
}

static bool type_eval_access(Context *context, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_ACCESS);

    Expr reduced_pack;
    if (!type_eval(context, type->access.record, &reduced_pack)) {
        return false;
    }

    if (reduced_pack.tag == EXPR_PACK) {
        size_t num_fields = reduced_pack.pack.num_fields;
        size_t field_num = type->access.field_num;

        if (field_num >= num_fields) {
            efprintf(stderr, "Cannot access field #%zu of literal record ($e) "
                "with %zu fields.\n", ewrap(&reduced_pack),
                field_num, num_fields);
            expr_free(&reduced_pack);
            return false;
        }

        bool ret_val = type_eval(context,
            &reduced_pack.pack.field_values[field_num], result);
        expr_free(&reduced_pack);
        return ret_val;
    } else {
        efprintf(stderr, "Cannot evaluate access of non-literal record ($e).\n",
            ewrap(&reduced_pack));
        expr_free(&reduced_pack);
        return false;
    }
}

bool type_eval(Context *context, const Expr *type, Expr *result) {
    Expr temp[1];

    switch (type->tag) {
      // These are all already in weak head normal form.
      case EXPR_TYPE:
      case EXPR_FORALL:     case EXPR_LAMBDA:
      case EXPR_ID:         case EXPR_REFLEXIVE:
      case EXPR_VOID:
      case EXPR_BOOL:       case EXPR_BOOLEAN:
      case EXPR_NAT:        case EXPR_NATURAL:
      case EXPR_SIGMA:      case EXPR_PACK:
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

      case EXPR_CALL:
        return type_eval_call(context, type, result);

      case EXPR_SUBSTITUTE:
        return type_eval_substitute(context, type, result);

      case EXPR_EXPLODE:
        return type_eval_explode(context, type, result);

      case EXPR_IFTHENELSE:
        return type_eval_ifthenelse(context, type, result);

      case EXPR_NAT_IND:
        return type_eval_nat_ind(context, type, result);

      case EXPR_ACCESS:
        return type_eval_access(context, type, result);
    }
}

bool type_check_top_level(Context *context, const TopLevel *top_level) {
    switch (top_level->tag) {
      case TOP_LEVEL_EXPR_DECL:
        symbol_table_register_global(&context->symbol_table,
            top_level->name, top_level->expr_decl.type);
        symbol_table_define_global(&context->symbol_table,
            top_level->name, top_level->expr_decl.expr);
        if (!type_check(context, &top_level->expr_decl.expr,
                &top_level->expr_decl.type)) {
            return false;
        }
        return true;
    }
}
