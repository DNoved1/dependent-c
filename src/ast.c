#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "dependent-c/ast.h"
#include "dependent-c/general.h"
#include "dependent-c/memory.h"
#include "dependent-c/symbol_table.h"

/***** Expression Management *************************************************/
static bool literal_equal(Literal x, Literal y) {
    if (x.tag != y.tag) {
        return false;
    }

    switch (x.tag) {
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
      case LIT_BOOL:
        return true;

      case LIT_INTEGRAL:
        return x.integral == y.integral;

      case LIT_BOOLEAN:
        return x.boolean == y.boolean;
    }
}

bool expr_equal(Expr x, Expr y) {
    if (x.tag != y.tag) {
        return false;
    }

    switch (x.tag) {
      case EXPR_LITERAL:
        return literal_equal(x.literal, y.literal);

      case EXPR_IDENT:
        return x.ident == y.ident;

      case EXPR_BIN_OP:
        return x.bin_op.op != y.bin_op.op
            && expr_equal(*x.bin_op.expr1, *y.bin_op.expr1)
            && expr_equal(*x.bin_op.expr2, *y.bin_op.expr2);

      case EXPR_FUNC_TYPE:
        if (!expr_equal(*x.func_type.ret_type, *y.func_type.ret_type)
                || x.func_type.num_params != y.func_type.num_params) {
            return false;
        }
        for (size_t i = 0; i < x.func_type.num_params; i++) {
            if (!expr_equal(x.func_type.param_types[i],
                        y.func_type.param_types[i])
                    || x.func_type.param_names[i]
                        != y.func_type.param_names[i]) {
                return false;
            }
        }
        return true;

      case EXPR_CALL:
        if (!expr_equal(*x.call.func, *y.call.func)
                || x.call.num_args != y.call.num_args) {
            return false;
        }
        for (size_t i = 0; i < x.call.num_args; i++) {
            if (!expr_equal(x.call.args[i], y.call.args[i])) {
                return false;
            }
        }
        return true;

      case EXPR_STRUCT:
        if (x.struct_.num_fields != y.struct_.num_fields) {
            return false;
        }
        for (size_t i = 0; i < x.struct_.num_fields; i++) {
            if (!expr_equal(x.struct_.field_types[i],
                        y.struct_.field_types[i])
                    || x.struct_.field_names[i]
                        != y.struct_.field_names[i]) {
                return false;
            }
        }
        return true;

      case EXPR_UNION:
        if (x.union_.num_fields != y.union_.num_fields) {
            return false;
        }
        for (size_t i = 0; i < x.union_.num_fields; i++) {
            if (!expr_equal(x.union_.field_types[i],
                        y.union_.field_types[i])
                    || x.union_.field_names[i]
                        != y.union_.field_names[i]) {
                return false;
            }
        }
        return true;

      case EXPR_PACK:
        if (!expr_equal(*x.pack.type, *y.pack.type)
                || x.pack.num_assigns != y.pack.num_assigns) {
            return false;
        }
        for (size_t i = 0; i < x.pack.num_assigns; i++) {
            if (x.pack.field_names[i] != y.pack.field_names[i]
                    || !expr_equal(x.pack.assigns[i],
                        y.pack.assigns[i])) {
                return false;
            }
        }
        return true;

      case EXPR_MEMBER:
        if (!expr_equal(*x.member.record, *y.member.record)
                || x.member.field != y.member.field) {
            return false;
        }
        return true;

      case EXPR_POINTER:
        return expr_equal(*x.pointer, *y.pointer);

      case EXPR_REFERENCE:
        return expr_equal(*x.reference, *y.reference);

      case EXPR_DEREFERENCE:
        return expr_equal(*x.dereference, *y.dereference);
    }
}

static Literal literal_copy(Literal x) {
    switch (x.tag) {
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
      case LIT_BOOL:
      case LIT_INTEGRAL:
      case LIT_BOOLEAN:
        return x;
    }
}

Expr expr_copy(Expr x) {
    Expr y = {.location = x.location, .tag = x.tag};

    switch (x.tag) {
      case EXPR_LITERAL:
        y.literal = literal_copy(x.literal);
        break;

      case EXPR_IDENT:
        y.ident = x.ident;
        break;

      case EXPR_BIN_OP:
        y.bin_op.op = x.bin_op.op;
        alloc(y.bin_op.expr1);
        *y.bin_op.expr1 = expr_copy(*x.bin_op.expr1);
        alloc(y.bin_op.expr2);
        *y.bin_op.expr2 = expr_copy(*x.bin_op.expr2);
        break;

      case EXPR_FUNC_TYPE:
        alloc(y.func_type.ret_type);
        *y.func_type.ret_type = expr_copy(*x.func_type.ret_type);
        y.func_type.num_params = x.func_type.num_params;
        alloc_array(y.func_type.param_types, y.func_type.num_params);
        alloc_array(y.func_type.param_names, y.func_type.num_params);
        for (size_t i = 0; i < y.func_type.num_params; i++) {
            y.func_type.param_types[i] = expr_copy(x.func_type.param_types[i]);
            y.func_type.param_names[i] = x.func_type.param_names[i];
        }
        break;

      case EXPR_CALL:
        alloc(y.call.func);
        *y.call.func = expr_copy(*x.call.func);
        y.call.num_args = x.call.num_args;
        alloc_array(y.call.args, y.call.num_args);
        for (size_t i = 0; i < y.call.num_args; i++) {
            y.call.args[i] = expr_copy(x.call.args[i]);
        }
        break;

      case EXPR_STRUCT:
        y.struct_.num_fields = x.struct_.num_fields;
        alloc_array(y.struct_.field_types, y.struct_.num_fields);
        alloc_array(y.struct_.field_names, y.struct_.num_fields);
        for (size_t i = 0; i < y.struct_.num_fields; i++) {
            y.struct_.field_types[i] = expr_copy(x.struct_.field_types[i]);
            y.struct_.field_names[i] = x.struct_.field_names[i];
        }
        break;

      case EXPR_UNION:
        y.union_.num_fields = x.union_.num_fields;
        alloc_array(y.union_.field_types, y.union_.num_fields);
        alloc_array(y.union_.field_names, y.union_.num_fields);
        for (size_t i = 0; i < y.union_.num_fields; i++) {
            y.union_.field_types[i] = expr_copy(x.union_.field_types[i]);
            y.union_.field_names[i] = x.union_.field_names[i];
        }
        break;

      case EXPR_PACK:
        alloc(y.pack.type);
        *y.pack.type = expr_copy(*x.pack.type);
        y.pack.num_assigns = x.pack.num_assigns;
        alloc_array(y.pack.field_names, y.pack.num_assigns);
        alloc_array(y.pack.assigns, y.pack.num_assigns);
        for (size_t i = 0; i < y.pack.num_assigns; i++) {
            y.pack.field_names[i] = x.pack.field_names[i];
            y.pack.assigns[i] = expr_copy(x.pack.assigns[i]);
        }
        break;

      case EXPR_MEMBER:
        alloc(y.member.record);
        *y.member.record = expr_copy(*x.member.record);
        y.member.field = x.member.field;
        break;

      case EXPR_POINTER:
        alloc(y.pointer);
        *y.pointer = expr_copy(*x.pointer);
        break;

      case EXPR_REFERENCE:
        alloc(y.reference);
        *y.reference = expr_copy(*x.reference);
        break;

      case EXPR_DEREFERENCE:
        alloc(y.dereference);
        *y.dereference = expr_copy(*x.dereference);
        break;
    }

    return y;
}

void expr_free_vars(Expr expr, SymbolSet *free_vars) {
    SymbolSet free_vars_temp[1];

    switch (expr.tag) {
      case EXPR_LITERAL:
        *free_vars = symbol_set_empty();
        break;

      case EXPR_IDENT:
        *free_vars = symbol_set_empty();
        symbol_set_add(free_vars, expr.ident);
        break;

      case EXPR_BIN_OP:
        expr_free_vars(*expr.bin_op.expr1, free_vars);
        expr_free_vars(*expr.bin_op.expr2, free_vars_temp);
        symbol_set_union(free_vars, free_vars_temp);
        break;

      case EXPR_FUNC_TYPE:
        expr_free_vars(*expr.func_type.ret_type, free_vars);
        for (size_t i = 0; i < expr.func_type.num_params; i++) {
            if (expr.func_type.param_names[i] != NULL) {
                symbol_set_delete(free_vars, expr.func_type.param_names[i]);
            }
        }
        for (size_t i = 0; i < expr.func_type.num_params; i++) {
            expr_free_vars(expr.func_type.param_types[i], free_vars_temp);
            for (size_t j = 0; j < i; j++) {
                if (expr.func_type.param_names[j] != NULL) {
                    symbol_set_delete(free_vars_temp,
                        expr.func_type.param_names[j]);
                }
            }
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_CALL:
        expr_free_vars(*expr.call.func, free_vars);
        for (size_t i = 0; i < expr.call.num_args; i++) {
            expr_free_vars(expr.call.args[i], free_vars_temp);
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_STRUCT:
        *free_vars = symbol_set_empty();
        for (size_t i = 0; i < expr.struct_.num_fields; i++) {
            expr_free_vars(expr.struct_.field_types[i], free_vars_temp);
            for (size_t j = 0; j < i; j++) {
                symbol_set_delete(free_vars_temp, expr.struct_.field_names[j]);
            }
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_UNION:
        *free_vars = symbol_set_empty();
        for (size_t i = 0; i < expr.union_.num_fields; i++) {
            expr_free_vars(expr.union_.field_types[i], free_vars_temp);
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_PACK:
        expr_free_vars(*expr.pack.type, free_vars);
        for (size_t i = 0; i < expr.pack.num_assigns; i++) {
            expr_free_vars(expr.pack.assigns[i], free_vars_temp);
            symbol_set_union(free_vars, free_vars_temp);
        }
        break;

      case EXPR_MEMBER:
        expr_free_vars(*expr.member.record, free_vars);
        break;

      case EXPR_POINTER:
        expr_free_vars(*expr.pointer, free_vars);
        break;

      case EXPR_REFERENCE:
        expr_free_vars(*expr.reference, free_vars);
        break;

      case EXPR_DEREFERENCE:
        expr_free_vars(*expr.dereference, free_vars);
        break;
    }
}

static bool expr_func_type_subst(Context *context, Expr *expr, const char *name,
        Expr replacement) {
    assert(expr->tag == EXPR_FUNC_TYPE);
    bool ret_val = false;

    SymbolSet free_vars;
    expr_free_vars(replacement, &free_vars);

    for (size_t i = 0; i < expr->func_type.num_params; i++) {
        if (!expr_subst(context, &expr->func_type.param_types[i],
                name, replacement)) {
            goto end_of_function;
        }
        const char *old_param_name = expr->func_type.param_names[i];

        if (old_param_name != NULL) {
            if (old_param_name == name) {
                ret_val = true;
                goto end_of_function;
            }

            if (symbol_set_contains(&free_vars, old_param_name)) {
                const char *new_param_name = symbol_gensym(&context->interns,
                    old_param_name);
                Expr new_replacement = (Expr){
                      .tag = EXPR_IDENT
                    , .ident = new_param_name
                };
                expr->func_type.param_names[i] = new_param_name;

                for (size_t j = i + 1; j < expr->func_type.num_params; j++) {
                    if (!expr_subst(context,
                            &expr->func_type.param_types[i],
                            old_param_name, new_replacement)) {
                        goto end_of_function;
                    }
                }
                if (!expr_subst(context, expr->func_type.ret_type,
                        old_param_name, new_replacement)) {
                    goto end_of_function;
                }
            }
        }
    }
    if (!expr_subst(context, expr->func_type.ret_type, name, replacement)) {
        goto end_of_function;
    }

    ret_val = true;

end_of_function:
    symbol_set_free(&free_vars);
    return ret_val;
}

static bool expr_struct_subst(Context *context, Expr *expr, const char *name,
        Expr replacement) {
    assert(expr->tag == EXPR_STRUCT);
    bool ret_val = false;

    SymbolSet free_vars;
    expr_free_vars(replacement, &free_vars);

    for (size_t i = 0; i < expr->struct_.num_fields; i++) {
        if (!expr_subst(context, &expr->struct_.field_types[i],
                name, replacement)) {
            goto end_of_function;
        }
        const char *old_field_name = expr->struct_.field_names[i];

        if (old_field_name == name) {
            ret_val = true;
            goto end_of_function;
        }

        if (symbol_set_contains(&free_vars, old_field_name)) {
            goto end_of_function;
        }
    }

    ret_val = true;

end_of_function:
    symbol_set_free(&free_vars);
    return ret_val;
}

// This one's a bit weird since a pack could either be a struct or a union.
// In the case of a union we should ignore the field names entirely and just
// substitute through all assignments. In the case of a struct we need to look
// at field names to see if there is shadowing or capturing going on.
//
// Problem is, we don't know whether we're dealing with a struct or union here.
//
// Solution: Since unions *should* only have one assignment, use the struct
// algorithm, which coincides with the union one when only one field is being
// assigned. When type checking we should ensure that this in indeed the case
// by making sure union packings have exactly one assignment.
static bool expr_pack_subst(Context *context, Expr *expr, const char *name,
        Expr replacement) {
    assert(expr->tag == EXPR_PACK);
    bool ret_val = false;

    SymbolSet free_vars;
    expr_free_vars(replacement, &free_vars);

    for (size_t i = 0; i < expr->pack.num_assigns; i++) {
        if (!expr_subst(context, &expr->pack.assigns[i], name, replacement)) {
            goto end_of_function;
        }
        const char *old_field_name = expr->pack.field_names[i];

        if (old_field_name == name) {
            ret_val = true;
            goto end_of_function;
        }

        if (symbol_set_contains(&free_vars, old_field_name)) {
            goto end_of_function;
        }
    }

    ret_val = true;

end_of_function:
    symbol_set_free(&free_vars);
    return ret_val;
}

bool expr_subst(Context *context, Expr *expr,
        const char *name, Expr replacement) {
    switch (expr->tag) {
      case EXPR_LITERAL:
        return true;

      case EXPR_IDENT:
        expr_free(expr);
        *expr = expr_copy(replacement);
        return true;

      case EXPR_BIN_OP:
        return expr_subst(context, expr->bin_op.expr1, name, replacement)
            && expr_subst(context, expr->bin_op.expr2, name, replacement);

      case EXPR_FUNC_TYPE:
        return expr_func_type_subst(context, expr, name, replacement);

      case EXPR_CALL:
        if (!expr_subst(context, expr->call.func, name, replacement)) {
            return false;
        }
        for (size_t i = 0; i < expr->call.num_args; i++) {
            if (!expr_subst(context, &expr->call.args[i], name, replacement)) {
                return false;
            }
        }
        return true;

      case EXPR_STRUCT:
        return expr_struct_subst(context, expr, name, replacement);

      case EXPR_UNION:
        for (size_t i = 0; i < expr->union_.num_fields; i++) {
            if (!expr_subst(context, &expr->union_.field_types[i],
                    name, replacement)) {
                return false;
            }
        }
        return true;

      case EXPR_PACK:
        return expr_pack_subst(context, expr, name, replacement);

      case EXPR_MEMBER:
        return expr_subst(context, expr->member.record, name, replacement);

      case EXPR_POINTER:
        return expr_subst(context, expr->pointer, name, replacement);

      case EXPR_REFERENCE:
        return expr_subst(context, expr->reference, name, replacement);

      case EXPR_DEREFERENCE:
        return expr_subst(context, expr->dereference, name, replacement);
    }
}

/***** Freeing ast nodes *****************************************************/
void expr_free(Expr *expr) {
    switch (expr->tag) {
      case EXPR_LITERAL:
      case EXPR_IDENT:
        break;

      case EXPR_BIN_OP:
        expr_free(expr->bin_op.expr1);
        dealloc(expr->bin_op.expr1);
        expr_free(expr->bin_op.expr2);
        dealloc(expr->bin_op.expr2);
        break;

      case EXPR_FUNC_TYPE:
        expr_free(expr->func_type.ret_type);
        dealloc(expr->func_type.ret_type);
        for (size_t i = 0; i < expr->func_type.num_params; i++) {
            expr_free(&expr->func_type.param_types[i]);
        }
        dealloc(expr->func_type.param_types);
        dealloc(expr->func_type.param_names);
        break;

      case EXPR_CALL:
        expr_free(expr->call.func);
        dealloc(expr->call.func);
        for (size_t i = 0; i < expr->call.num_args; i++) {
            expr_free(&expr->call.args[i]);
        }
        dealloc(expr->call.args);
        break;

      case EXPR_STRUCT:
        for (size_t i = 0; i < expr->struct_.num_fields; i++) {
            expr_free(&expr->struct_.field_types[i]);
        }
        dealloc(expr->struct_.field_types);
        dealloc(expr->struct_.field_names);
        break;

      case EXPR_UNION:
        for (size_t i = 0; i < expr->union_.num_fields; i++) {
            expr_free(&expr->union_.field_types[i]);
        }
        dealloc(expr->union_.field_types);
        dealloc(expr->union_.field_names);
        break;

      case EXPR_PACK:
        expr_free(expr->pack.type);
        dealloc(expr->pack.type);
        for (size_t i = 0; i < expr->pack.num_assigns; i++) {
            expr_free(&expr->pack.assigns[i]);
        }
        dealloc(expr->pack.field_names);
        dealloc(expr->pack.assigns);
        break;

      case EXPR_MEMBER:
        expr_free(expr->member.record);
        dealloc(expr->member.record);
        break;

      case EXPR_POINTER:
        expr_free(expr->pointer);
        dealloc(expr->pointer);
        break;

      case EXPR_REFERENCE:
        expr_free(expr->reference);
        dealloc(expr->reference);
        break;

      case EXPR_DEREFERENCE:
        expr_free(expr->dereference);
        dealloc(expr->dereference);
        break;
    }
    memset(expr, 0, sizeof *expr);
}

void statement_free(Statement *statement) {
    switch (statement->tag) {
      case STATEMENT_EMPTY:
        break;

      case STATEMENT_EXPR:
      case STATEMENT_RETURN:
        expr_free(&statement->expr);
        break;

      case STATEMENT_BLOCK:
        block_free(&statement->block);
        break;

      case STATEMENT_DECL:
        expr_free(&statement->decl.type);
        if (statement->decl.is_initialized) {
            expr_free(&statement->decl.initial_value);
        }
        break;

      case STATEMENT_IFTHENELSE:
        for (size_t i = 0; i < statement->ifthenelse.num_ifs; i++) {
            expr_free(&statement->ifthenelse.ifs[i]);
            block_free(&statement->ifthenelse.thens[i]);
        }
        dealloc(statement->ifthenelse.ifs);
        dealloc(statement->ifthenelse.thens);
        block_free(&statement->ifthenelse.else_);
        break;
    }
    memset(statement, 0, sizeof *statement);
}

void block_free(Block *block) {
    for (size_t i = 0; i < block->num_statements; i++) {
        statement_free(&block->statements[i]);
    }
    dealloc(block->statements);
    memset(block, 0, sizeof *block);
}

void top_level_free(TopLevel *top_level) {
    switch (top_level->tag) {
      case TOP_LEVEL_FUNC:
        expr_free(&top_level->func.ret_type);
        for (size_t i = 0; i < top_level->func.num_params; i++) {
            expr_free(&top_level->func.param_types[i]);
        }
        dealloc(top_level->func.param_types);
        dealloc(top_level->func.param_names);
        block_free(&top_level->func.block);
        break;
    }
    memset(top_level, 0, sizeof *top_level);
}

void translation_unit_free(TranslationUnit *unit) {
    for (size_t i = 0; i < unit->num_top_levels; i++) {
        top_level_free(&unit->top_levels[i]);
    }
    dealloc(unit->top_levels);
    memset(unit, 0, sizeof *unit);
}

/***** Pretty-printing ast nodes *********************************************/
static void print_indentation_whitespace(FILE *to, int nesting) {
    for (int i = 0; i < nesting; i++) {
        fprintf(to, "    ");
    }
}

#define tag_to_string(tag, str) case tag: fprintf(to, str); break;
static void literal_pprint(FILE *to, int nesting, Literal literal) {
    switch (literal.tag) {
      tag_to_string(LIT_TYPE, "type")
      tag_to_string(LIT_VOID, "void")
      tag_to_string(LIT_U8, "u8")
      tag_to_string(LIT_S8, "s8")
      tag_to_string(LIT_U16, "u16")
      tag_to_string(LIT_S16, "s16")
      tag_to_string(LIT_U32, "u32")
      tag_to_string(LIT_S32, "s32")
      tag_to_string(LIT_U64, "u64")
      tag_to_string(LIT_S64, "s64")
      tag_to_string(LIT_BOOL, "bool")

      case LIT_INTEGRAL:
        fprintf(to, "%" PRIu64, literal.integral);
        break;

      case LIT_BOOLEAN:
        fprintf(to, literal.boolean ? "true" : "false");
        break;
    }
}

static void bin_op_pprint(FILE *to, BinaryOp bin_op) {
    switch (bin_op) {
      tag_to_string(BIN_OP_EQ,      " == ")
      tag_to_string(BIN_OP_NE,      " != ")
      tag_to_string(BIN_OP_LT,      " < ")
      tag_to_string(BIN_OP_LTE,     " <= ")
      tag_to_string(BIN_OP_GT,      " > ")
      tag_to_string(BIN_OP_GTE,     " >= ")
      tag_to_string(BIN_OP_ADD,     " + ")
      tag_to_string(BIN_OP_SUB,     " - ")
    }
}
#undef tag_to_string

static void expr_pprint_(FILE *to, int nesting, Expr expr) {
    bool simple = expr.tag == EXPR_LITERAL || expr.tag == EXPR_IDENT
            || expr.tag == EXPR_STRUCT || expr.tag == EXPR_UNION;

    if (!simple) putc('(', to);
    expr_pprint(to, nesting, expr);
    if (!simple) putc(')', to);
}

void expr_pprint(FILE *to, int nesting, Expr expr) {
    switch (expr.tag) {
      case EXPR_LITERAL:
        literal_pprint(to, nesting, expr.literal);
        break;

      case EXPR_IDENT:
        fputs(expr.ident, to);
        break;

      case EXPR_BIN_OP:
        expr_pprint_(to, nesting, *expr.bin_op.expr1);
        bin_op_pprint(to, expr.bin_op.op);
        expr_pprint_(to, nesting, *expr.bin_op.expr2);
        break;

      case EXPR_FUNC_TYPE:
        expr_pprint_(to, nesting, *expr.func_type.ret_type);
        putc('[', to);
        for (size_t i = 0; i < expr.func_type.num_params; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            expr_pprint(to, nesting, expr.func_type.param_types[i]);
            if (expr.func_type.param_names[i] != NULL) {
                fprintf(to, " %s", expr.func_type.param_names[i]);
            }
        }
        putc(']', to);
        break;

      case EXPR_CALL:
        expr_pprint_(to, nesting, *expr.call.func);
        putc('(', to);
        for (size_t i = 0; i < expr.call.num_args; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            expr_pprint(to, nesting, expr.call.args[i]);
        }
        putc(')', to);
        break;

      case EXPR_STRUCT:
        fprintf(to, "struct { ");
        for (size_t i = 0; i < expr.struct_.num_fields; i++) {
            expr_pprint(to, nesting, expr.struct_.field_types[i]);
            fprintf(to, " %s; ", expr.struct_.field_names[i]);
        }
        putc('}', to);
        break;

      case EXPR_UNION:
        fprintf(to, "union { ");
        for (size_t i = 0; i < expr.union_.num_fields; i++) {
            expr_pprint(to, nesting, expr.union_.field_types[i]);
            fprintf(to, " %s; ", expr.union_.field_names[i]);
        }
        putc('}', to);
        break;

      case EXPR_PACK:
        putc('(', to);
        expr_pprint_(to, nesting, *expr.pack.type);
        fprintf(to, "){");
        for (size_t i = 0; i < expr.pack.num_assigns; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            fprintf(to, ".%s = ", expr.pack.field_names[i]);
            expr_pprint(to, nesting, expr.pack.assigns[i]);
        }
        putc('}', to);
        break;

      case EXPR_MEMBER:
        expr_pprint_(to, nesting, *expr.member.record);
        fprintf(to, ".%s", expr.member.field);
        break;

      case EXPR_POINTER:
        expr_pprint_(to, nesting, *expr.pointer);
        putc('*', to);
        break;

      case EXPR_REFERENCE:
        putc('&', to);
        expr_pprint_(to, nesting, *expr.reference);
        break;

      case EXPR_DEREFERENCE:
        putc('*', to);
        expr_pprint_(to, nesting, *expr.dereference);
        break;
    }
}

static void block_pprint(FILE *to, int nesting, Block block) {
    for (size_t i = 0; i < block.num_statements; i++) {
        statement_pprint(to, nesting, block.statements[i]);
    }
}

void statement_pprint(FILE *to, int nesting, Statement statement) {
    print_indentation_whitespace(to, nesting);

    switch (statement.tag) {
      case STATEMENT_EMPTY:
        fprintf(to, ";\n");
        break;

      case STATEMENT_RETURN:
        fprintf(to, "return ");
        // Fallthrough

      case STATEMENT_EXPR:
        expr_pprint(to, nesting, statement.expr);
        fprintf(to, ";\n");
        break;

      case STATEMENT_BLOCK:
        fprintf(to, "{\n");
        for (size_t i = 0; i < statement.block.num_statements; i++) {
            statement_pprint(to, nesting + 1, statement.block.statements[i]);
        }
        print_indentation_whitespace(to, nesting);
        fprintf(to, "}\n");
        break;

      case STATEMENT_DECL:
        expr_pprint(to, nesting, statement.decl.type);
        fprintf(to, " %s", statement.decl.name);
        if (statement.decl.is_initialized) {
            fprintf(to, " = ");
            expr_pprint(to, nesting, statement.decl.initial_value);
        }
        fprintf(to, ";\n");
        break;

      case STATEMENT_IFTHENELSE:
        for (size_t i = 0; i < statement.ifthenelse.num_ifs; i++) {
            if (i != 0) {
                print_indentation_whitespace(to, nesting);
                fprintf(to, "} else ");
            }
            fprintf(to, "if (");
            expr_pprint(to, nesting, statement.ifthenelse.ifs[i]);
            fprintf(to, ") {\n");
            block_pprint(to, nesting + 1, statement.ifthenelse.thens[i]);
        }
        print_indentation_whitespace(to, nesting);
        fprintf(to, "} else {\n");
        block_pprint(to, nesting + 1, statement.ifthenelse.else_);
        print_indentation_whitespace(to, nesting);
        fprintf(to, "}\n");
        break;
    }
}

void top_level_pprint(FILE *to, TopLevel top_level) {
    switch (top_level.tag) {
       case TOP_LEVEL_FUNC:
        expr_pprint(to, 0, top_level.func.ret_type);
        fprintf(to, " %s(", top_level.name);

        for (size_t i = 0; i < top_level.func.num_params; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            expr_pprint(to, 0, top_level.func.param_types[i]);
            if (top_level.func.param_names[i] != NULL) {
                fprintf(to, " %s", top_level.func.param_names[i]);
            }
        }
        fprintf(to, ") {\n");
        block_pprint(to, 1, top_level.func.block);
        fprintf(to, "}\n");
        break;
    }
}

void translation_unit_pprint(FILE *to, TranslationUnit unit) {
    for (size_t i = 0; i < unit.num_top_levels; i++) {
        if (i > 0) {
            putc('\n', to);
        }

        top_level_pprint(to, unit.top_levels[i]);
    }
}
