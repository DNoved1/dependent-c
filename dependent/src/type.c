#include <assert.h>
#include <inttypes.h>
#include <string.h>

#include "dependent-c/general.h"
#include "dependent-c/memory.h"

/***** Type Checking / Inference *********************************************/
bool type_check(Context *ctx, const Expr *expr, const Expr *type) {
    Expr type2[1];

    if (!type_infer(ctx, expr, type2)) {
        return false;
    }

    bool ret_val = type_equal(ctx, type, type2);
    expr_free(ctx, type2);
    return ret_val;
}

static bool type_infer_forall(Context *ctx, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_FORALL);
    bool ret_val = false;

    symbol_table_enter_scope(&ctx->symbol_table);

    for (size_t i = 0; i < expr->forall.num_params; i++) {
        if (type_check(ctx, &expr->forall.param_types[i], &literal_expr_type)) {
            if (expr->forall.param_names[i] != NULL) {
                symbol_table_register_local(&ctx->symbol_table,
                    expr->forall.param_names[i], expr->forall.param_types[i]);
            }
        } else {
            goto end_of_function;
        }
    }

    if (!type_check(ctx, expr->forall.ret_type, &literal_expr_type)) {
        goto end_of_function;
    }

    *result = literal_expr_type;
    ret_val = true;

end_of_function:
    symbol_table_leave_scope(&ctx->symbol_table);
    return ret_val;
}

static bool type_infer_lambda(Context *ctx, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_LAMBDA);

    symbol_table_enter_scope(&ctx->symbol_table);

    for (size_t i = 0; i < expr->lambda.num_params; i++) {
        if (type_check(ctx, &expr->lambda.param_types[i], &literal_expr_type)) {
            symbol_table_register_local(&ctx->symbol_table,
                expr->lambda.param_names[i], expr->lambda.param_types[i]);
        } else {
            symbol_table_leave_scope(&ctx->symbol_table);
            return false;
        }
    }

    Expr ret_type;
    symbol_table_enter_scope(&ctx->symbol_table);
    bool ret_val = type_infer(ctx, expr->lambda.body, &ret_type);
    symbol_table_leave_scope(&ctx->symbol_table);
    symbol_table_leave_scope(&ctx->symbol_table);

    if (!ret_val) {
        return false;
    }

    result->tag = EXPR_FORALL;
    result->forall.num_params = expr->lambda.num_params;
    alloc_array(result->forall.param_types, result->forall.num_params);
    alloc_array(result->forall.param_names, result->forall.num_params);
    for (size_t i = 0; i < result->forall.num_params; i++) {
        result->forall.param_types[i] = expr_copy(ctx, &expr->lambda.param_types[i]);
        result->forall.param_names[i] = expr->lambda.param_names[i];
    }
    alloc_assign(result->forall.ret_type, ret_type);
    return true;
}

static bool type_infer_call(Context *ctx, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_CALL);
    Expr forall;

    if (!type_infer(ctx, expr->call.func, &forall)) {
        return false;
    }

    if (forall.tag != EXPR_FORALL) {
        efprintf(ctx, stderr, "Cannot call non-function type ($e).\n",
            ewrap(&forall));
        expr_free(ctx, &forall);
        return false;
    }

    if (forall.forall.num_params != expr->call.num_args) {
        fprintf(stderr, "Calling function which expects %zu parameters with "
            "%zu arguments.\n", forall.forall.num_params,
            expr->call.num_args);

        expr_free(ctx, &forall);
        return false;
    }

    for (size_t i = 0; i < forall.forall.num_params; i++) {
        const Expr arg = expr->call.args[i];
        const char *param_name = forall.forall.param_names[i];

        if (type_check(ctx, &arg, &forall.forall.param_types[i])) {
            for (size_t j = i + 1; j < forall.forall.num_params; j++) {
                expr_subst(ctx, &forall.forall.param_types[j],
                    param_name, &arg);
            }

            expr_subst(ctx, forall.forall.ret_type, param_name, &arg);
        } else {
            expr_free(ctx, &forall);
            return false;
        }
    }

    *result = expr_copy(ctx, forall.forall.ret_type);
    expr_free(ctx, &forall);
    return true;
}

static bool type_infer_substitute(Context *ctx, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_SUBSTITUTE);
    bool ret_val = false;

    // Initialize to trivially freeable values for easy cleanup
    Expr proof_type = literal_expr_type;
    Expr family_type = literal_expr_type;
    Expr instance_type = literal_expr_type;
    Expr value_type = literal_expr_type;

    if (!type_infer(ctx, expr->substitute.proof, &proof_type)
            ||!type_infer(ctx, expr->substitute.family, &family_type)
            || !type_infer(ctx, expr->substitute.instance, &instance_type)) {
        goto end_of_function;
    }

    if (proof_type.tag != EXPR_ID) {
        efprintf(ctx, stderr, "Cannot substitute with non-identity type ($e).\n",
            ewrap(&proof_type));
        goto end_of_function;
    }

    if (!type_infer(ctx, proof_type.id.expr1, &value_type)) {
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

    if (!type_equal(ctx, &expected_family, &family_type)) {
        goto end_of_function;
    }

    const Expr expected_instance = {
          .tag = EXPR_CALL
        , .call.func = expr->substitute.family
        , .call.num_args = 1
        , .call.args = (Expr[]){*proof_type.id.expr1}
    };

    if (!type_equal(ctx, &expected_instance, &instance_type)) {
        goto end_of_function;
    }

    result->tag = EXPR_CALL;
    *result->call.func = expr_copy(ctx, expr->substitute.family);
    result->call.num_args = 1;
    alloc_array(result->call.args, 1);
    result->call.args[0] = expr_copy(ctx, proof_type.id.expr2);
    ret_val = true;

end_of_function:
    expr_free(ctx, &proof_type);
    expr_free(ctx, &family_type);
    expr_free(ctx, &instance_type);
    expr_free(ctx, &value_type);
    return ret_val;
}

static bool type_infer_nat_ind(Context *ctx, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_NAT_IND);

    if (!type_check(ctx, expr->nat_ind.natural, &literal_expr_nat)) {
        fprintf(stderr, "Cannot perform natural induction on non-natural "
            "type.\n");
        return false;
    }

    if (!type_infer(ctx, expr->nat_ind.base_val, result)) {
        return false;
    }

    symbol_table_enter_scope(&ctx->symbol_table);
    symbol_table_register_local(&ctx->symbol_table,
        expr->nat_ind.ind_name, literal_expr_nat);

    if (!type_check(ctx, expr->nat_ind.ind_val, result)) {
        expr_free(ctx, result);
        symbol_table_leave_scope(&ctx->symbol_table);
        return false;
    }

    symbol_table_leave_scope(&ctx->symbol_table);
    return true;
}

static bool type_infer_sigma(Context *ctx, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_SIGMA);
    bool ret_val = false;

    symbol_table_enter_scope(&ctx->symbol_table);

    for (size_t i = 0; i < expr->sigma.num_fields; i++) {
        if (type_check(ctx, &expr->sigma.field_types[i], &literal_expr_type)) {
            if (expr->sigma.field_names[i] != NULL) {
                symbol_table_register_local(&ctx->symbol_table,
                    expr->sigma.field_names[i], expr->sigma.field_types[i]);
            }
        } else {
            goto end_of_function;
        }
    }

    *result = literal_expr_type;
    ret_val = true;

end_of_function:
    symbol_table_leave_scope(&ctx->symbol_table);
    return ret_val;
}

static bool type_infer_pack(Context *ctx, const Expr *expr, Expr *result) {
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
            if (!type_infer(ctx, &expr->pack.field_values[i],
                    &sigma.sigma.field_types[i])) {
                expr_free(ctx, &sigma);
                return false;
            }
        }

        *result = sigma;
        return true;
    } else {
        if (expr->pack.as_type->tag != EXPR_SIGMA) {
            efprintf(ctx, stderr, "Cannot cast tuple to non-sigma type ($e).\n",
                ewrap(expr->pack.as_type));
            return false;
        }

        if (expr->pack.as_type->sigma.num_fields != expr->pack.num_fields) {
            fprintf(stderr, "Cannot case tuple with %zu fields to sigma with "
                "%zu fields.\n", expr->pack.num_fields,
                expr->pack.as_type->sigma.num_fields);
        }

        Expr sigma = expr_copy(ctx, expr->pack.as_type);

        for (size_t i = 0; i < expr->pack.num_fields; i++) {
            if (!type_check(ctx, &expr->pack.field_values[i],
                    &sigma.sigma.field_types[i])) {
                expr_free(ctx, &sigma);
                return false;
            }

            for (size_t j = i + 1; j < sigma.sigma.num_fields; j++) {
                expr_subst(ctx, &sigma.sigma.field_types[j],
                    sigma.sigma.field_names[i], &expr->pack.field_values[i]);
            }
        }

        *result = sigma;
        return true;
    }
}

static bool type_infer_access(Context *ctx, const Expr *expr, Expr *result) {
    assert(expr->tag == EXPR_ACCESS);
    bool ret_val;

    Expr sigma_expr, sigma;
    if (!type_infer(ctx, expr->access.record, &sigma_expr)) {
        return false;
    }
    ret_val = type_eval(ctx, &sigma_expr, &sigma);
    expr_free(ctx, &sigma_expr);
    if (!ret_val) {
        return false;
    }

    if (sigma.tag != EXPR_SIGMA) {
        efprintf(ctx, stderr, "Cannot access field of non-sigma type ($e).\n",
            ewrap(&sigma));
        expr_free(ctx, &sigma);
        return false;
    }

    if (expr->access.field_num >= sigma.sigma.num_fields) {
        fprintf(stderr, "Cannot access field #%zu of sigma with only "
            "%zu fields.\n", expr->access.field_num, sigma.sigma.num_fields);
        expr_free(ctx, &sigma);
        return false;
    }

    *result = expr_copy(ctx, &sigma.sigma.field_types[expr->access.field_num]);

    for (size_t i = 0; i < expr->access.field_num; i++) {
        const char *field_name = sigma.sigma.field_names[i];

        if (field_name != NULL) {
            const Expr replacement = {
                  .tag = EXPR_ACCESS
                , .access.record = expr->access.record
                , .access.field_num = i
            };

            expr_subst(ctx, result, field_name, &replacement);
        }
    }

    expr_free(ctx, &sigma);
    return true;
}

bool type_infer(Context *ctx, const Expr *expr, Expr *result) {
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
        if (!symbol_table_lookup(&ctx->symbol_table, expr->ident, temp)) {
            fprintf(stderr, "Unbound symbol \"%s\".\n", expr->ident);
            location_pprint(ctx, ctx->source_name, &expr->location);
            return false;
        }
        *result = expr_copy(ctx, temp);
        return true;

      case EXPR_FORALL:
        return type_infer_forall(ctx, expr, result);

      case EXPR_LAMBDA:
        return type_infer_lambda(ctx, expr, result);

      case EXPR_CALL:
        return type_infer_call(ctx, expr, result);

      case EXPR_ID:
        if (!type_infer(ctx, expr->id.expr1, temp)) {
            return false;
        }
        if (!type_infer(ctx, expr->id.expr2, temp2)) {
            expr_free(ctx, temp);
            return false;
        }
        if (!type_equal(ctx, temp, temp2)) {
            efprintf(ctx, stderr, "Cannot create an identity type for unequal "
                "types ($e) and ($e).\n", ewrap(temp, temp2));
            expr_free(ctx, temp);
            expr_free(ctx, temp2);
            return false;
        }
        expr_free(ctx, temp);
        expr_free(ctx, temp2);
        *result = literal_expr_type;
        return true;

      case EXPR_REFLEXIVE:
        if (!type_infer(ctx, expr->reflexive, temp)) {
            return false;
        }
        result->tag = EXPR_ID;
        alloc_assign(result->id.expr1, expr_copy(ctx, temp));
        alloc_assign(result->id.expr2, *temp);
        return true;

      case EXPR_SUBSTITUTE:
        return type_infer_substitute(ctx, expr, result);

      case EXPR_EXPLODE:
        if (!type_check(ctx, expr->explode.void_instance, &literal_expr_void)
                || !type_check(ctx, expr->explode.into_type,
                    &literal_expr_type)) {
            return false;
        }
        *result = expr_copy(ctx, expr->explode.into_type);
        return true;

      case EXPR_IFTHENELSE:
        if (!type_check(ctx, expr->ifthenelse.predicate,
                &literal_expr_bool)) {
            return false;
        }
        if (!type_infer(ctx, expr->ifthenelse.then_, temp)) {
            return false;
        }
        if (!type_infer(ctx, expr->ifthenelse.else_, temp2)) {
            expr_free(ctx, temp);
            return false;
        }
        result->tag = EXPR_IFTHENELSE;
        alloc_assign(result->ifthenelse.predicate,
            expr_copy(ctx, expr->ifthenelse.predicate));
        alloc_assign(result->ifthenelse.then_, *temp);
        alloc_assign(result->ifthenelse.else_, *temp2);
        return true;

      case EXPR_NAT_IND:
        return type_infer_nat_ind(ctx, expr, result);

      case EXPR_SIGMA:
        return type_infer_sigma(ctx, expr, result);

      case EXPR_PACK:
        return type_infer_pack(ctx, expr, result);

      case EXPR_ACCESS:
        return type_infer_access(ctx, expr, result);
    }
}

bool type_equal(Context *ctx, const Expr *type1, const Expr *type2) {
    // TODO, do alpha equivalence rather than simple structural equivalence.

    Expr type1_whnf[1], type2_whnf[1];
    if (!type_eval(ctx, type1, type1_whnf)) {
        *type1_whnf = expr_copy(ctx, type1);
    }
    if (!type_eval(ctx, type2, type2_whnf)) {
        *type2_whnf = expr_copy(ctx, type2);
    }

    bool ret_val = expr_equal(ctx, type1_whnf, type2_whnf);
    expr_free(ctx, type1_whnf);
    expr_free(ctx, type2_whnf);

    if (!ret_val) {
        efprintf(ctx, stderr, "Could not determine that ($e) ~ ($e).\n",
            ewrap(type1, type2));
    }

    return ret_val;
}

static bool type_eval_call(Context *ctx, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_CALL);

    Expr reduced_func[1];
    if (!type_eval(ctx, type->call.func, reduced_func)) {
        return false;
    }

    if (reduced_func->tag != EXPR_LAMBDA) {
        efprintf(ctx, stderr, "Cannot evaluate type further because it attempts "
            "to call non-function ($e).\n"
            "    Started with type ($e).\n",
            ewrap(reduced_func, type));
        expr_free(ctx, reduced_func);
        return false;
    }

    // Just to make sure the arguments are of the correct type
    if (!type_infer_call(ctx, type, result)) {
        expr_free(ctx, reduced_func);
        return false;
    }
    expr_free(ctx, result);

    for (size_t i = 0; i < type->call.num_args; i++) {
        expr_subst(ctx, reduced_func->lambda.body,
            reduced_func->lambda.param_names[i], &type->call.args[i]);
    }

    bool ret_val = type_eval(ctx, reduced_func->lambda.body, result);
    expr_free(ctx, reduced_func);
    return ret_val;
}

static bool type_eval_ifthenelse(Context *ctx, const Expr *type,
        Expr *result) {
    assert(type->tag == EXPR_IFTHENELSE);

    // If both sides of the if branch are equivalent we can reduce to that
    if (type_equal(ctx, type->ifthenelse.then_, type->ifthenelse.else_)) {
        return type_eval(ctx, type->ifthenelse.then_, result);
    } else {
        fprintf(stderr, "    While checking if both if-then-else branches "
            "have the same type.\n");
    }

    Expr reduced_cond[1];
    if (!type_eval(ctx, type->ifthenelse.predicate, reduced_cond)) {
        return false;
    }

    if (reduced_cond->tag == EXPR_BOOLEAN) {
        if (reduced_cond->boolean) {
            return type_eval(ctx, type->ifthenelse.then_, result);
        } else {
            return type_eval(ctx, type->ifthenelse.else_, result);
        }
    } else {
        expr_free(ctx, reduced_cond);
        return false;
    }
}

static bool type_eval_substitute(Context *ctx, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_SUBSTITUTE);

    Expr reduced_refl;
    if (!type_eval(ctx, type->substitute.proof, &reduced_refl)) {
        return false;
    }

    if (reduced_refl.tag != EXPR_REFLEXIVE) {
        efprintf(ctx, stderr, "Cannot substitute with non-literal reflexive "
            "proof ($e).\n", ewrap(&reduced_refl));
        expr_free(ctx, &reduced_refl);
        return false;
    }

    *result = expr_copy(ctx, type->substitute.instance);
    expr_free(ctx, &reduced_refl);
    return true;
}

static bool type_eval_explode(Context *ctx, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_EXPLODE);

    efprintf(ctx, stderr, "Cannot reduce explosion ($e).\n", ewrap(type));
    return false;
}

static bool type_eval_nat_ind(Context *ctx, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_NAT_IND);

    Expr reduced_nat;
    if (!type_eval(ctx, type->nat_ind.natural, &reduced_nat)) {
        return false;
    }

    if (reduced_nat.tag == EXPR_NATURAL) {
        if (reduced_nat.natural == (type->nat_ind.goes_down ? 0 : UINT64_MAX)) {
            bool ret_val = type_eval(ctx, type->nat_ind.base_val, result);
            expr_free(ctx, &reduced_nat);
            return ret_val;
        } else {
            Expr ind_val = expr_copy(ctx, type->nat_ind.ind_val);
            const Expr replacement = {
                  .tag = EXPR_NATURAL
                , .natural = reduced_nat.natural
                    + (type->nat_ind.goes_down ? -1 : +1)
            };
            expr_subst(ctx, &ind_val, type->nat_ind.ind_name, &replacement);
            bool ret_val = type_eval(ctx, &ind_val, result);
            expr_free(ctx, &reduced_nat);
            expr_free(ctx, &ind_val);
            return ret_val;
        }
    } else {
        efprintf(ctx, stderr, "Cannot evaluate natural induction with "
            "non-literal natural ($e).\n", ewrap(&reduced_nat));
        expr_free(ctx, &reduced_nat);
        return false;
    }
}

static bool type_eval_access(Context *ctx, const Expr *type, Expr *result) {
    assert(type->tag == EXPR_ACCESS);

    Expr reduced_pack;
    if (!type_eval(ctx, type->access.record, &reduced_pack)) {
        return false;
    }

    if (reduced_pack.tag == EXPR_PACK) {
        size_t num_fields = reduced_pack.pack.num_fields;
        size_t field_num = type->access.field_num;

        if (field_num >= num_fields) {
            efprintf(ctx, stderr, "Cannot access field #%zu of literal record "
                "($e) with %zu fields.\n", ewrap(&reduced_pack),
                field_num, num_fields);
            expr_free(ctx, &reduced_pack);
            return false;
        }

        bool ret_val = type_eval(ctx,
            &reduced_pack.pack.field_values[field_num], result);
        expr_free(ctx, &reduced_pack);
        return ret_val;
    } else {
        efprintf(ctx, stderr, "Cannot evaluate access of non-literal record "
            "($e).\n", ewrap(&reduced_pack));
        expr_free(ctx, &reduced_pack);
        return false;
    }
}

bool type_eval(Context *ctx, const Expr *type, Expr *result) {
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
        *result = expr_copy(ctx, type);
        return true;

      case EXPR_IDENT:
        if (symbol_table_lookup_define(&ctx->symbol_table,
                type->ident, temp)) {
            return type_eval(ctx, temp, result);
        } else {
            *result = expr_copy(ctx, type);
            return true;
        }

      case EXPR_CALL:
        return type_eval_call(ctx, type, result);

      case EXPR_SUBSTITUTE:
        return type_eval_substitute(ctx, type, result);

      case EXPR_EXPLODE:
        return type_eval_explode(ctx, type, result);

      case EXPR_IFTHENELSE:
        return type_eval_ifthenelse(ctx, type, result);

      case EXPR_NAT_IND:
        return type_eval_nat_ind(ctx, type, result);

      case EXPR_ACCESS:
        return type_eval_access(ctx, type, result);
    }
}

bool type_check_top_level(Context *ctx, const TopLevel *top_level) {
    switch (top_level->tag) {
      case TOP_LEVEL_EXPR_DECL:
        symbol_table_register_global(&ctx->symbol_table,
            top_level->name, top_level->expr_decl.type);
        symbol_table_define_global(&ctx->symbol_table,
            top_level->name, top_level->expr_decl.expr);
        if (!type_check(ctx, &top_level->expr_decl.expr,
                &top_level->expr_decl.type)) {
            return false;
        }
        return true;
    }
}
