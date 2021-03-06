#include <assert.h>
#include <inttypes.h>
#include <string.h>

#include "dependent-c/color.h"
#include "dependent-c/general.h"
#include "dependent-c/memory.h"

/***** Expression Management *************************************************/
bool expr_equal(Context *ctx, const Expr *x, const Expr *y) {
    if (x->tag != y->tag) {
        return false;
    }

    switch (x->tag) {
      case EXPR_TYPE:
      case EXPR_VOID:
      case EXPR_BOOL:
      case EXPR_NAT:
        return true;

      case EXPR_IDENT:
        return x->ident == y->ident;

      case EXPR_FORALL:
        if (x->forall.num_params != y->forall.num_params) {
            return false;
        }
        for (size_t i = 0; i < x->forall.num_params; i++) {
            if (!expr_equal(ctx,
                        &x->forall.param_types[i], &y->forall.param_types[i])
                    || x->forall.param_names[i] != y->forall.param_names[i]) {
                return false;
            }
        }
        return expr_equal(ctx, x->forall.ret_type, y->forall.ret_type);

      case EXPR_LAMBDA:
        if (x->lambda.num_params != y->lambda.num_params) {
            return false;
        }
        for (size_t i = 0; i < x->lambda.num_params; i++) {
            if (!expr_equal(ctx, x->lambda.param_types, y->lambda.param_types)
                    || x->lambda.param_names[i] != y->lambda.param_names[i]) {
                return false;
            }
        }
        return expr_equal(ctx, x->lambda.body, y->lambda.body);

      case EXPR_CALL:
        if (!expr_equal(ctx, x->call.func, y->call.func)
                || x->call.num_args != y->call.num_args) {
            return false;
        }
        for (size_t i = 0; i < x->call.num_args; i++) {
            if (!expr_equal(ctx, &x->call.args[i], &y->call.args[i])) {
                return false;
            }
        }
        return true;

      case EXPR_ID:
        return expr_equal(ctx, x->id.expr1, y->id.expr1)
            && expr_equal(ctx, x->id.expr2, y->id.expr2);

      case EXPR_REFLEXIVE:
        return expr_equal(ctx, x->reflexive, y->reflexive);

      case EXPR_SUBSTITUTE:
        return expr_equal(ctx, x->substitute.proof, y->substitute.proof)
            && expr_equal(ctx, x->substitute.family, y->substitute.family)
            && expr_equal(ctx, x->substitute.instance, y->substitute.instance);

      case EXPR_EXPLODE:
        return expr_equal(ctx, x->explode.void_instance, y->explode.void_instance)
            && expr_equal(ctx, x->explode.into_type, y->explode.into_type);

      case EXPR_BOOLEAN:
        return x->boolean == y->boolean;

      case EXPR_IFTHENELSE:
        return expr_equal(ctx, x->ifthenelse.predicate, y->ifthenelse.predicate)
            && expr_equal(ctx, x->ifthenelse.then_, y->ifthenelse.then_)
            && expr_equal(ctx, x->ifthenelse.else_, y->ifthenelse.else_);

      case EXPR_NATURAL:
        return x->natural == y->natural;

      case EXPR_NAT_IND:
        return expr_equal(ctx, x->nat_ind.natural, y->nat_ind.natural)
            && x->nat_ind.goes_down == y->nat_ind.goes_down
            && expr_equal(ctx, x->nat_ind.base_val, y->nat_ind.base_val)
            && x->nat_ind.ind_name == y->nat_ind.ind_name
            && expr_equal(ctx, x->nat_ind.ind_val, y->nat_ind.ind_val);

      case EXPR_SIGMA:
        if (x->sigma.num_fields != y->sigma.num_fields) {
            return false;
        }
        for (size_t i = 0; i < x->sigma.num_fields; i++) {
            if (x->sigma.field_names[i] != y->sigma.field_names[i]
                    || !expr_equal(ctx,
                        &x->sigma.field_types[i], &y->sigma.field_types[i])) {
                return false;
            }
        }
        return true;

      case EXPR_PACK:
        if ((x->pack.as_type == NULL && y->pack.as_type != NULL)
                || y->pack.as_type == NULL) {
            return false;
        }
        if ((x->pack.as_type != NULL
                && !expr_equal(ctx, x->pack.as_type, y->pack.as_type))
                || x->pack.num_fields != y->pack.num_fields) {
            return false;
        }
        for (size_t i = 0; i < x->pack.num_fields; i++) {
            if (!expr_equal(ctx,
                    &x->pack.field_values[i], &y->pack.field_values[i])) {
                return false;
            }
        }
        return true;

      case EXPR_ACCESS:
        return expr_equal(ctx, x->access.record, y->access.record)
            && x->access.field_num == y->access.field_num;
    }
}

Expr expr_copy(Context *ctx, const Expr *x) {
    Expr y = {.location = x->location, .tag = x->tag};

    switch (x->tag) {
      case EXPR_TYPE:
      case EXPR_VOID:
      case EXPR_BOOL:
      case EXPR_NAT:
        break;

      case EXPR_IDENT:
        y.ident = x->ident;
        break;

      case EXPR_FORALL:
        y.forall.num_params = x->forall.num_params;
        alloc_array(y.forall.param_types, y.forall.num_params);
        alloc_array(y.forall.param_names, y.forall.num_params);
        for (size_t i = 0; i < y.forall.num_params; i++) {
            y.forall.param_types[i] = expr_copy(ctx, &x->forall.param_types[i]);
            y.forall.param_names[i] = x->forall.param_names[i];
        }
        alloc_assign(y.forall.ret_type, expr_copy(ctx, x->forall.ret_type));
        break;

      case EXPR_LAMBDA:
        y.lambda.num_params = x->lambda.num_params;
        alloc_array(y.lambda.param_types, y.lambda.num_params);
        alloc_array(y.lambda.param_names, y.lambda.num_params);
        for (size_t i = 0; i < y.lambda.num_params; i++) {
            y.lambda.param_types[i] = expr_copy(ctx, &x->lambda.param_types[i]);
            y.lambda.param_names[i] = x->lambda.param_names[i];
        }
        alloc_assign(y.lambda.body, expr_copy(ctx, x->lambda.body));
        break;

      case EXPR_CALL:
        alloc_assign(y.call.func, expr_copy(ctx, x->call.func));
        y.call.num_args = x->call.num_args;
        alloc_array(y.call.args, y.call.num_args);
        for (size_t i = 0; i < y.call.num_args; i++) {
            y.call.args[i] = expr_copy(ctx, &x->call.args[i]);
        }
        break;

      case EXPR_ID:
        alloc_assign(y.id.expr1, expr_copy(ctx, x->id.expr1));
        alloc_assign(y.id.expr2, expr_copy(ctx, x->id.expr2));
        break;

      case EXPR_REFLEXIVE:
        alloc_assign(y.reflexive, expr_copy(ctx, x->reflexive));
        break;

      case EXPR_SUBSTITUTE:
        alloc_assign(y.substitute.proof, expr_copy(ctx, x->substitute.proof));
        alloc_assign(y.substitute.family, expr_copy(ctx, x->substitute.family));
        alloc_assign(y.substitute.instance, expr_copy(ctx, x->substitute.instance));
        break;

      case EXPR_EXPLODE:
        alloc_assign(y.explode.void_instance,
            expr_copy(ctx, x->explode.void_instance));
        alloc_assign(y.explode.into_type, expr_copy(ctx, x->explode.into_type));
        break;

      case EXPR_BOOLEAN:
        y.boolean = x->boolean;
        break;

      case EXPR_IFTHENELSE:
        alloc_assign(y.ifthenelse.predicate, expr_copy(ctx, x->ifthenelse.predicate));
        alloc_assign(y.ifthenelse.then_, expr_copy(ctx, x->ifthenelse.then_));
        alloc_assign(y.ifthenelse.else_, expr_copy(ctx, x->ifthenelse.else_));
        break;

      case EXPR_NATURAL:
        y.natural = x->natural;
        break;

      case EXPR_NAT_IND:
        alloc_assign(y.nat_ind.natural, expr_copy(ctx, x->nat_ind.natural));
        y.nat_ind.goes_down = x->nat_ind.goes_down;
        alloc_assign(y.nat_ind.base_val, expr_copy(ctx, x->nat_ind.base_val));
        y.nat_ind.ind_name = x->nat_ind.ind_name;
        alloc_assign(y.nat_ind.ind_val, expr_copy(ctx, x->nat_ind.ind_val));
        break;

      case EXPR_SIGMA:
        y.sigma.num_fields = x->sigma.num_fields;
        alloc_array(y.sigma.field_names, y.sigma.num_fields);
        alloc_array(y.sigma.field_types, y.sigma.num_fields);
        for (size_t i = 0; i < y.sigma.num_fields; i++) {
            y.sigma.field_names[i] = x->sigma.field_names[i];
            y.sigma.field_types[i] = expr_copy(ctx, &x->sigma.field_types[i]);
        }
        break;

      case EXPR_PACK:
        if (x->pack.as_type == NULL) {
            y.pack.as_type = NULL;
        } else {
            alloc_assign(y.pack.as_type, expr_copy(ctx, x->pack.as_type));
        }
        y.pack.num_fields = x->pack.num_fields;
        alloc_array(y.pack.field_values, y.pack.num_fields);
        for (size_t i = 0; i < y.pack.num_fields; i++) {
            y.pack.field_values[i] = expr_copy(ctx, &x->pack.field_values[i]);
        }
        break;

      case EXPR_ACCESS:
        alloc_assign(y.access.record, expr_copy(ctx, x->access.record));
        y.access.field_num = x->access.field_num;
        break;
    }

    return y;
}

void expr_free_vars(Context *ctx, const Expr *expr, SymbolSet *free_vars) {
    SymbolSet free_vars_temp[1];

    switch (expr->tag) {
      case EXPR_TYPE:
      case EXPR_VOID:
      case EXPR_BOOL:
      case EXPR_BOOLEAN:
      case EXPR_NAT:
      case EXPR_NATURAL:
        *free_vars = symbol_set_empty();
        break;

      case EXPR_IDENT:
        *free_vars = symbol_set_empty();
        symbol_set_add(free_vars, expr->ident);
        break;

      case EXPR_FORALL:
        expr_free_vars(ctx, expr->forall.ret_type, free_vars);
        for (size_t i = 0; i < expr->forall.num_params; i++) {
            if (expr->forall.param_names[i] != NULL) {
                symbol_set_delete(free_vars, expr->forall.param_names[i]);
            }
        }
        for (size_t i = 0; i < expr->forall.num_params; i++) {
            expr_free_vars(ctx, &expr->forall.param_types[i], free_vars_temp);
            for (size_t j = 0; j < i; j++) {
                if (expr->forall.param_names[j] != NULL) {
                    symbol_set_delete(free_vars_temp,
                        expr->forall.param_names[j]);
                }
            }
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_LAMBDA:
        expr_free_vars(ctx, expr->lambda.body, free_vars);
        for (size_t i = 0; i < expr->lambda.num_params; i++) {
            symbol_set_delete(free_vars, expr->lambda.param_names[i]);
        }
        for (size_t i = 0; i < expr->lambda.num_params; i++) {
            expr_free_vars(ctx, &expr->lambda.param_types[i], free_vars_temp);
            for (size_t j = 0; j < i; j++) {
                symbol_set_delete(free_vars_temp, expr->lambda.param_names[j]);
            }
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_CALL:
        expr_free_vars(ctx, expr->call.func, free_vars);
        for (size_t i = 0; i < expr->call.num_args; i++) {
            expr_free_vars(ctx, &expr->call.args[i], free_vars_temp);
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_ID:
        expr_free_vars(ctx, expr->id.expr1, free_vars);
        expr_free_vars(ctx, expr->id.expr2, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        break;

      case EXPR_REFLEXIVE:
        expr_free_vars(ctx, expr->reflexive, free_vars);
        break;

      case EXPR_SUBSTITUTE:
        expr_free_vars(ctx, expr->substitute.proof, free_vars);
        expr_free_vars(ctx, expr->substitute.family, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        expr_free_vars(ctx, expr->substitute.instance, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        break;

      case EXPR_EXPLODE:
        expr_free_vars(ctx, expr->explode.void_instance, free_vars);
        expr_free_vars(ctx, expr->explode.into_type, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        break;

      case EXPR_IFTHENELSE:
        expr_free_vars(ctx, expr->ifthenelse.predicate, free_vars);
        expr_free_vars(ctx, expr->ifthenelse.then_, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        expr_free_vars(ctx, expr->ifthenelse.else_, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        break;

      case EXPR_NAT_IND:
        expr_free_vars(ctx, expr->nat_ind.natural, free_vars);
        expr_free_vars(ctx, expr->nat_ind.base_val, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        expr_free_vars(ctx, expr->nat_ind.ind_val, free_vars_temp);
        symbol_set_delete(free_vars_temp, expr->nat_ind.ind_name);
        symbol_set_union(free_vars, free_vars_temp);
        break;

      case EXPR_SIGMA:
        *free_vars = symbol_set_empty();
        for (size_t i = 0; i < expr->sigma.num_fields; i++) {
            expr_free_vars(ctx, &expr->sigma.field_types[i], free_vars_temp);
            for (size_t j = 0; j < i; j++) {
                if (expr->sigma.field_names[j] != NULL) {
                    symbol_set_delete(free_vars_temp,
                        expr->sigma.field_names[j]);
                }
            }
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_PACK:
        if (expr->pack.as_type == NULL) {
            *free_vars = symbol_set_empty();
        } else {
            expr_free_vars(ctx, expr->pack.as_type, free_vars);
        }
        for (size_t i = 0; i < expr->pack.num_fields; i++) {
            expr_free_vars(ctx, &expr->pack.field_values[i], free_vars_temp);
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_ACCESS:
        expr_free_vars(ctx, expr->access.record, free_vars);
        break;
    }
}

static void expr_forall_subst(Context *ctx, Expr *expr,
        const char *name, const Expr *replacement) {
    assert(expr->tag == EXPR_FORALL);

    SymbolSet free_vars;
    expr_free_vars(ctx, replacement, &free_vars);

    for (size_t i = 0; i < expr->forall.num_params; i++) {
        expr_subst(ctx, &expr->forall.param_types[i], name, replacement);
        const char *old_param_name = expr->forall.param_names[i];

        if (old_param_name != NULL) {
            if (old_param_name == name) {
                symbol_set_free(&free_vars);
                return;
            }

            if (symbol_set_contains(&free_vars, old_param_name)) {
                const char *new_param_name = symbol_gensym(&ctx->interns,
                    old_param_name);
                const Expr new_replacement = {
                      .tag = EXPR_IDENT
                    , .ident = new_param_name
                };
                expr->forall.param_names[i] = new_param_name;

                for (size_t j = i + 1; j < expr->forall.num_params; j++) {
                    expr_subst(ctx, &expr->forall.param_types[i],
                        old_param_name, &new_replacement);
                }
                expr_subst(ctx, expr->forall.ret_type,
                    old_param_name, &new_replacement);
            }
        }
    }

    expr_subst(ctx, expr->forall.ret_type, name, replacement);

    symbol_set_free(&free_vars);
}

static void expr_lambda_subst(Context *ctx, Expr *expr,
        const char *name, const Expr *replacement) {
    assert(expr->tag == EXPR_LAMBDA);

    SymbolSet free_vars[1];
    expr_free_vars(ctx, replacement, free_vars);

    for (size_t i = 0; i < expr->lambda.num_params; i++) {
        expr_subst(ctx, &expr->lambda.param_types[i], name, replacement);
        const char *old_param_name = expr->lambda.param_names[i];

        if (old_param_name == name) {
            symbol_set_free(free_vars);
            return;
        }

        if (symbol_set_contains(free_vars, old_param_name)) {
            const char *new_param_name = symbol_gensym(&ctx->interns,
                old_param_name);
            const Expr new_replacement = {
                  .tag = EXPR_IDENT
                , .ident = new_param_name
            };
            expr->lambda.param_names[i] = new_param_name;

            for (size_t j = i + 1; j < expr->forall.num_params; j++) {
                expr_subst(ctx, &expr->lambda.param_types[i],
                    old_param_name, &new_replacement);
            }
            expr_subst(ctx, expr->lambda.body,
                old_param_name, &new_replacement);
        }
    }

    expr_subst(ctx, expr->lambda.body, name, replacement);

    symbol_set_free(free_vars);
}

static void expr_sigma_subst(Context *ctx, Expr *expr,
        const char *name, const Expr *replacement) {
    assert(expr->tag == EXPR_SIGMA);

    SymbolSet free_vars;
    expr_free_vars(ctx, replacement, &free_vars);

    for (size_t i = 0; i < expr->sigma.num_fields; i++) {
        expr_subst(ctx, &expr->sigma.field_types[i], name, replacement);
        const char *old_field_name = expr->sigma.field_names[i];

        if (old_field_name == name) {
            symbol_set_free(&free_vars);
            return;
        }

        if (symbol_set_contains(&free_vars, old_field_name)) {
            const char *new_field_name = symbol_gensym(&ctx->interns,
                old_field_name);
            const Expr new_replacement = {
                  .tag = EXPR_IDENT
                , .ident = new_field_name
            };
            expr->sigma.field_names[i] = new_field_name;

            for (size_t j = i + 1; j < expr->sigma.num_fields; j++) {
                expr_subst(ctx, &expr->sigma.field_types[i],
                    old_field_name, &new_replacement);
            }
        }
    }

    symbol_set_free(&free_vars);
}

void expr_subst(Context *ctx, Expr *expr,
        const char *name, const Expr *replacement) {
    SymbolSet free_vars;

    switch (expr->tag) {
      case EXPR_TYPE:
      case EXPR_VOID:
      case EXPR_BOOL:
      case EXPR_BOOLEAN:
      case EXPR_NAT:
      case EXPR_NATURAL:
        break;

      case EXPR_IDENT:
        if (name == expr->ident) {
            expr_free(ctx, expr);
            *expr = expr_copy(ctx, replacement);
        }
        break;

      case EXPR_FORALL:
        expr_forall_subst(ctx, expr, name, replacement);
        break;

      case EXPR_LAMBDA:
        expr_lambda_subst(ctx, expr, name, replacement);
        break;

      case EXPR_CALL:
        expr_subst(ctx, expr->call.func, name, replacement);
        for (size_t i = 0; i < expr->call.num_args; i++) {
            expr_subst(ctx, &expr->call.args[i], name, replacement);
        }
        break;

      case EXPR_ID:
        expr_subst(ctx, expr->id.expr1, name, replacement);
        expr_subst(ctx, expr->id.expr2, name, replacement);
        break;

      case EXPR_REFLEXIVE:
        expr_subst(ctx, expr->reflexive, name, replacement);
        break;

      case EXPR_SUBSTITUTE:
        expr_subst(ctx, expr->substitute.proof, name, replacement);
        expr_subst(ctx, expr->substitute.family, name, replacement);
        expr_subst(ctx, expr->substitute.instance, name, replacement);
        break;

      case EXPR_EXPLODE:
        expr_subst(ctx, expr->explode.void_instance, name, replacement);
        expr_subst(ctx, expr->explode.into_type, name, replacement);
        break;

      case EXPR_IFTHENELSE:
        expr_subst(ctx, expr->ifthenelse.predicate, name, replacement);
        expr_subst(ctx, expr->ifthenelse.then_, name, replacement);
        expr_subst(ctx, expr->ifthenelse.else_, name, replacement);
        break;

      case EXPR_NAT_IND:
        expr_subst(ctx, expr->nat_ind.natural, name, replacement);
        expr_subst(ctx, expr->nat_ind.base_val, name, replacement);
        expr_free_vars(ctx, replacement, &free_vars);
        if (symbol_set_contains(&free_vars, expr->nat_ind.ind_name)) {
            const char *new_ind_name = symbol_gensym(&ctx->interns,
                expr->nat_ind.ind_name);
            const Expr new_replacement = {
                  .tag = EXPR_IDENT
                , .ident = new_ind_name
            };
            expr->nat_ind.ind_name = new_ind_name;
            expr_subst(ctx, expr->nat_ind.ind_val,
                expr->nat_ind.ind_name, &new_replacement);
        }
        symbol_set_free(&free_vars);
        expr_subst(ctx, expr->nat_ind.ind_val, name, replacement);
        break;

      case EXPR_SIGMA:
        expr_sigma_subst(ctx, expr, name, replacement);
        break;

      case EXPR_PACK:
        if (expr->pack.as_type != NULL) {
            expr_subst(ctx, expr->pack.as_type, name, replacement);
        }
        for (size_t i = 0; i < expr->pack.num_fields; i++) {
            expr_subst(ctx, &expr->pack.field_values[i], name, replacement);
        }
        break;

      case EXPR_ACCESS:
        expr_subst(ctx, expr->access.record, name, replacement);
        break;
    }
}

/***** Freeing ast nodes *****************************************************/
void expr_free(Context *ctx, Expr *expr) {
    switch (expr->tag) {
      case EXPR_IDENT:
      case EXPR_TYPE:
      case EXPR_VOID:
      case EXPR_BOOL:
      case EXPR_BOOLEAN:
      case EXPR_NAT:
      case EXPR_NATURAL:
        break;

      case EXPR_FORALL:
        for (size_t i = 0; i < expr->forall.num_params; i++) {
            expr_free(ctx, &expr->forall.param_types[i]);
        }
        dealloc(expr->forall.param_types);
        dealloc(expr->forall.param_names);
        expr_free(ctx, expr->forall.ret_type);
        dealloc(expr->forall.ret_type);
        break;

      case EXPR_LAMBDA:
        for (size_t i = 0; i < expr->lambda.num_params; i++) {
            expr_free(ctx, &expr->lambda.param_types[i]);
        }
        dealloc(expr->lambda.param_types);
        dealloc(expr->lambda.param_names);
        expr_free(ctx, expr->lambda.body);
        dealloc(expr->lambda.body);
        break;

      case EXPR_CALL:
        expr_free(ctx, expr->call.func);
        dealloc(expr->call.func);
        for (size_t i = 0; i < expr->call.num_args; i++) {
            expr_free(ctx, &expr->call.args[i]);
        }
        dealloc(expr->call.args);
        break;

      case EXPR_ID:
        expr_free(ctx, expr->id.expr1);
        dealloc(expr->id.expr1);
        expr_free(ctx, expr->id.expr2);
        dealloc(expr->id.expr2);
        break;

      case EXPR_REFLEXIVE:
        expr_free(ctx, expr->reflexive);
        dealloc(expr->reflexive);
        break;

      case EXPR_SUBSTITUTE:
        expr_free(ctx, expr->substitute.proof);
        dealloc(expr->substitute.proof);
        expr_free(ctx, expr->substitute.family);
        dealloc(expr->substitute.family);
        expr_free(ctx, expr->substitute.instance);
        dealloc(expr->substitute.instance);
        break;

      case EXPR_EXPLODE:
        expr_free(ctx, expr->explode.void_instance);
        dealloc(expr->explode.void_instance);
        expr_free(ctx, expr->explode.into_type);
        dealloc(expr->explode.into_type);
        break;

      case EXPR_IFTHENELSE:
        expr_free(ctx, expr->ifthenelse.predicate);
        dealloc(expr->ifthenelse.predicate);
        expr_free(ctx, expr->ifthenelse.then_);
        dealloc(expr->ifthenelse.then_);
        expr_free(ctx, expr->ifthenelse.else_);
        dealloc(expr->ifthenelse.else_);
        break;

      case EXPR_NAT_IND:
        expr_free(ctx, expr->nat_ind.natural);
        dealloc(expr->nat_ind.natural);
        expr_free(ctx, expr->nat_ind.base_val);
        dealloc(expr->nat_ind.base_val);
        expr_free(ctx, expr->nat_ind.ind_val);
        dealloc(expr->nat_ind.ind_val);
        break;

      case EXPR_SIGMA:
        for (size_t i = 0; i < expr->sigma.num_fields; i++) {
            expr_free(ctx, &expr->sigma.field_types[i]);
        }
        dealloc(expr->sigma.field_types);
        dealloc(expr->sigma.field_names);
        break;

      case EXPR_PACK:
        if (expr->pack.as_type != NULL) {
            expr_free(ctx, expr->pack.as_type);
            dealloc(expr->pack.as_type);
        }
        for (size_t i = 0; i < expr->pack.num_fields; i++) {
            expr_free(ctx, &expr->pack.field_values[i]);
        }
        dealloc(expr->pack.field_values);
        break;

      case EXPR_ACCESS:
        expr_free(ctx, expr->access.record);
        dealloc(expr->access.record);
    }
    memset(expr, 0, sizeof *expr);
}

void top_level_free(Context *ctx, TopLevel *top_level) {
    switch (top_level->tag) {
      case TOP_LEVEL_EXPR_DECL:
        expr_free(ctx, &top_level->expr_decl.type);
        expr_free(ctx, &top_level->expr_decl.expr);
        break;
    }
    memset(top_level, 0, sizeof *top_level);
}

void translation_unit_free(Context *ctx, TranslationUnit *unit) {
    for (size_t i = 0; i < unit->num_top_levels; i++) {
        top_level_free(ctx, &unit->top_levels[i]);
    }
    dealloc(unit->top_levels);
    memset(unit, 0, sizeof *unit);
}

/***** Pretty-printing ast nodes *********************************************/
static void indent_pprint(FILE *to, unsigned indent) {
    for (unsigned i = 0; i < indent; i++) {
        fprintf(to, "    ");
    }
}

void location_pprint(Context *ctx, const char *file, const LocationInfo *info) {
    fprintf(stderr, "    At file %s, line %u, column %u.\n",
        file, info->line, info->column);
}

static void expr_pprint_(Context *ctx, FILE *to, unsigned indent,
        const Expr *expr) {
    bool simple = expr->tag == EXPR_IDENT
            || expr->tag == EXPR_TYPE
            || expr->tag == EXPR_VOID
            || expr->tag == EXPR_BOOL || expr->tag == EXPR_BOOLEAN
            || expr->tag == EXPR_NAT || expr->tag == EXPR_NATURAL
            || expr->tag == EXPR_SIGMA || expr->tag == EXPR_PACK;

    if (!simple) putc('(', to);
    expr_pprint(ctx, to, indent, expr);
    if (!simple) putc(')', to);
}


void expr_pprint(Context *ctx, FILE *to, unsigned indent, const Expr *expr) {
    switch (expr->tag) {
      case EXPR_TYPE:
        if (ctx->color_enabled) {
            fprintf(to, CYAN "Type" NORMAL);
        } else {
            fprintf(to, "Type");
        }
        break;

      case EXPR_VOID:
        if (ctx->color_enabled) {
            fprintf(to, CYAN "Void" NORMAL);
        } else {
            fprintf(to, "Void");
        }
        break;

      case EXPR_BOOL:
        if (ctx->color_enabled) {
            fprintf(to, CYAN "Bool" NORMAL);
        } else {
            fprintf(to, "Bool");
        }
        break;

      case EXPR_NAT:
        if (ctx->color_enabled) {
            fprintf(to, CYAN "Nat" NORMAL);
        } else {
            fprintf(to, "Nat");
        }
        break;

      case EXPR_IDENT:
        fprintf(to, "%s", expr->ident);
        break;

      case EXPR_FORALL:
        putc('[', to);
        for (size_t i = 0; i < expr->forall.num_params; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }
            if (expr->forall.param_names[i] != NULL) {
                fprintf(to, "%s ", expr->forall.param_names[i]);
                if (ctx->color_enabled) {
                    fprintf(to, RED ":" NORMAL " ");
                } else {
                    fprintf(to, ": ");
                }
            }
            expr_pprint(ctx, to, indent, &expr->forall.param_types[i]);
        }
        fprintf(to, "] ");
        if (ctx->color_enabled) {
            fprintf(to, RED "->" NORMAL " ");
        } else {
            fprintf(to, "-> ");
        }
        expr_pprint(ctx, to, indent, expr->forall.ret_type);
        break;

      case EXPR_LAMBDA:
        if (ctx->color_enabled) {
            fprintf(to, RED "λ" NORMAL "(");
        } else {
            fprintf(to, "\\(");
        }
        for (size_t i = 0; i < expr->lambda.num_params; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }
            fprintf(to, "%s ", expr->lambda.param_names[i]);
            if (ctx->color_enabled) {
                fprintf(to, RED ":" NORMAL " ");
            } else {
                fprintf(to, ": ");
            }
            expr_pprint(ctx, to, indent, &expr->lambda.param_types[i]);
        }
        fprintf(to, ") ");
        if (ctx->color_enabled) {
            fprintf(to, RED "=>" NORMAL " ");
        } else {
            fprintf(to, "=> ");
        }
        expr_pprint(ctx, to, indent, expr->lambda.body);
        break;

      case EXPR_CALL:
        expr_pprint_(ctx, to, indent, expr->call.func);
        putc('(', to);
        for (size_t i = 0; i < expr->call.num_args; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }
            expr_pprint(ctx, to, indent, &expr->call.args[i]);
        }
        putc(')', to);
        break;

      case EXPR_ID:
        efprintf(ctx, to, "$e = $e", ewrap(expr->id.expr1, expr->id.expr2));
        break;

      case EXPR_REFLEXIVE:
        efprintf(ctx, to, "reflexive($e)", ewrap(expr->reflexive));
        break;

      case EXPR_SUBSTITUTE:
        efprintf(ctx, to, "substitute($e, $e, $e)", ewrap(expr->substitute.proof,
            expr->substitute.family, expr->substitute.instance));
        break;

      case EXPR_EXPLODE:
        efprintf(ctx, to, "explode($e, $e)", ewrap(expr->explode.void_instance,
            expr->explode.into_type));
        break;

      case EXPR_BOOLEAN:
        if (ctx->color_enabled) {
            fprintf(to, CYAN "%s" NORMAL, expr->boolean ? "true" : "false");
        } else {
            fprintf(to, "%s", expr->boolean ? "true" : "false");
        }
        break;

      case EXPR_IFTHENELSE:
        if (ctx->color_enabled) {
            fprintf(to, RED "if" NORMAL " ");
            expr_pprint(ctx, to, indent, expr->ifthenelse.predicate);
            putc('\n', to); indent_pprint(to, indent);
            indent_pprint(to, indent + 1);
            fprintf(to, RED "then" NORMAL " ");
            expr_pprint(ctx, to, indent + 1, expr->ifthenelse.then_);
            putc('\n', to); indent_pprint(to, indent);
            indent_pprint(to, indent + 1);
            fprintf(to, RED "else" NORMAL " ");
            expr_pprint(ctx, to, indent + 1, expr->ifthenelse.else_);
        } else {
            fprintf(to, "if ");
            expr_pprint(ctx, to, indent, expr->ifthenelse.predicate);
            putc('\n', to); indent_pprint(to, indent);
            indent_pprint(to, indent + 1);
            fprintf(to, "then ");
            expr_pprint(ctx, to, indent + 1, expr->ifthenelse.then_);
            putc('\n', to); indent_pprint(to, indent);
            indent_pprint(to, indent + 1);
            fprintf(to, RED "else ");
            expr_pprint(ctx, to, indent + 1, expr->ifthenelse.else_);
        }
        break;

      case EXPR_NATURAL:
        if (ctx->color_enabled) {
            fprintf(to, CYAN "%" PRIu64 NORMAL, expr->natural);
        } else {
            fprintf(to, "%" PRIu64, expr->natural);
        }
        break;

      case EXPR_NAT_IND:
        if (ctx->color_enabled) {
            fprintf(to, RED "case" NORMAL " ");
            expr_pprint(ctx, to, indent, expr->nat_ind.natural);
            fprintf(to, " " RED "of" NORMAL "\n");
            indent_pprint(to, indent + 1);
            fprintf(to, "| " CYAN "%s" NORMAL " " RED "=>" NORMAL " ",
                expr->nat_ind.goes_down ? "0" : "NAT_MAX");
            expr_pprint(ctx, to, indent + 1, expr->nat_ind.base_val);
            putc('\n', to); indent_pprint(to, indent + 1);
            fprintf(to, "| %s %c " CYAN "1" NORMAL " " RED "=>" NORMAL " ",
                expr->nat_ind.ind_name, expr->nat_ind.goes_down ? '+' : '-');
            expr_pprint(ctx, to, indent + 1, expr->nat_ind.ind_val);
        } else {
            fprintf(to, "case ");
            expr_pprint(ctx, to, indent, expr->nat_ind.natural);
            fprintf(to, " of\n");
            indent_pprint(to, indent + 1);
            fprintf(to, "| %s => ",
                expr->nat_ind.goes_down ? "0" : "NAT_MAX");
            expr_pprint(ctx, to, indent + 1, expr->nat_ind.base_val);
            putc('\n', to); indent_pprint(to, indent + 1);
            fprintf(to, "| %s %c 1 => ",
                expr->nat_ind.ind_name, expr->nat_ind.goes_down ? '+' : '-');
            expr_pprint(ctx, to, indent + 1, expr->nat_ind.ind_val);
       }
        break;

      case EXPR_SIGMA:
        putc('{', to);
        for (size_t i = 0; i < expr->sigma.num_fields; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }
            if (expr->sigma.field_names[i] != NULL) {
                fprintf(to, "%s ", expr->sigma.field_names[i]);
                if (ctx->color_enabled) {
                    fprintf(to, RED ":" NORMAL " ");
                } else {
                    fprintf(to, ": ");
                }
            }
            expr_pprint(ctx, to, indent, &expr->sigma.field_types[i]);
        }
        putc('}', to);
        break;

      case EXPR_PACK:
        if (expr->pack.as_type != NULL) {
            putc('(', to);
        }
        putc('<', to);
        for (size_t i = 0; i < expr->pack.num_fields; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }
            expr_pprint(ctx, to, indent, &expr->pack.field_values[i]);
        }
        putc('>', to);
        if (expr->pack.as_type != NULL) {
            if (ctx->color_enabled) {
                fprintf(to, " " RED ":" NORMAL);
            } else {
                fprintf(to, " :");
            }
            putc(' ', to);
            expr_pprint(ctx, to, indent, expr->pack.as_type);
            putc(')', to);
        }
        break;

      case EXPR_ACCESS:
        expr_pprint_(ctx, to, indent, expr->access.record);
        if (ctx->color_enabled) {
            fprintf(to, "[" CYAN "%zu" NORMAL "]", expr->access.field_num);
        } else {
            fprintf(to, "[%zu]", expr->access.field_num);
        }
        break;
    }
}

void top_level_pprint(Context *ctx, FILE *to, const TopLevel *top_level) {
    switch (top_level->tag) {
       case TOP_LEVEL_EXPR_DECL:
        if (ctx->color_enabled) {
            efprintf(ctx, to, "%s " RED ":" NORMAL " $e\n"
                "%s " RED "=" NORMAL " $e\n",
                ewrap(&top_level->expr_decl.type, &top_level->expr_decl.expr),
                top_level->name, top_level->name);
        } else {
            efprintf(ctx, to, "%s : $e\n%s = $e\n",
                ewrap(&top_level->expr_decl.type, &top_level->expr_decl.expr),
                top_level->name, top_level->name);
        }
        break;
    }
}

void translation_unit_pprint(Context *ctx, FILE *to,
        const TranslationUnit *unit) {
    for (size_t i = 0; i < unit->num_top_levels; i++) {
        if (i > 0) {
            putc('\n', to);
        }

        top_level_pprint(ctx, to, &unit->top_levels[i]);
    }
}

/***** Specializations of printf *********************************************/

void efprintf(Context *ctx, FILE *file,
        const char *format, const void *eargs[], ...) {
    va_list vargs;
    va_start(vargs, eargs);

    size_t len = strlen(format);
    char *format_copy; alloc_array(format_copy, len + 1);
    strcpy(format_copy, format);

    size_t start = 0;
    size_t i = 0;
    while (format_copy[i] != '\0') {
        if (format_copy[i] == '$') {
            format_copy[i] = '\0';
            vfprintf(file, &format_copy[start], vargs);

            i += 1;
            switch (format_copy[i]) {
              case '$':
                putc('$', file);
                break;

              case 'e':
                expr_pprint(ctx, file, 0, (Expr*)*eargs);
                eargs += 1;
                break;

              case '(':
                i += 1;
                switch (format_copy[i]) {
                  case 'e':
                    expr_pprint_(ctx, file, 0, (Expr*)*eargs);
                    eargs += 1;
                    break;

                  // Swallow errors
                  case '\0':
                    i -= 1;
                    break;
                  default:
                    break;
                }
                break;

              // Swallow errors
              case '\0':
                i -= 1;
                break;
              default:
                break;
            }

            i += 1;
            start = i;
        } else {
            i += 1;
        }
    }
    vfprintf(file, &format_copy[start], vargs);

    dealloc(format_copy);
    va_end(vargs);
}
