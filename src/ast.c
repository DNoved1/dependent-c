#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "dependent-c/ast.h"

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
        return true;

      case LIT_INTEGRAL:
        return x.data.integral == y.data.integral;
    }
}

bool expr_equal(Expr x, Expr y) {
    if (x.tag != y.tag) {
        return false;
    }

    switch (x.tag) {
      case EXPR_LITERAL:
        return literal_equal(x.data.literal, y.data.literal);

      case EXPR_IDENT:
        return x.data.ident == y.data.ident;

      case EXPR_FUNC_TYPE:
        if (!expr_equal(*x.data.func_type.ret_type, *y.data.func_type.ret_type)
                || x.data.func_type.num_params != y.data.func_type.num_params) {
            return false;
        }
        for (size_t i = 0; i < x.data.func_type.num_params; i++) {
            if (!expr_equal(x.data.func_type.param_types[i],
                        y.data.func_type.param_types[i])
                    || x.data.func_type.param_names[i]
                        != y.data.func_type.param_names[i]) {
                return false;
            }
        }
        return true;

      case EXPR_CALL:
        if (!expr_equal(*x.data.call.func, *y.data.call.func)
                || x.data.call.num_args != y.data.call.num_args) {
            return false;
        }
        for (size_t i = 0; i < x.data.call.num_args; i++) {
            if (!expr_equal(x.data.call.args[i], y.data.call.args[i])) {
                return false;
            }
        }
        return true;

      case EXPR_STRUCT:
        if (x.data.struct_.num_fields != y.data.struct_.num_fields) {
            return false;
        }
        for (size_t i = 0; i < x.data.struct_.num_fields; i++) {
            if (!expr_equal(x.data.struct_.field_types[i],
                        y.data.struct_.field_types[i])
                    || x.data.struct_.field_names[i]
                        != y.data.struct_.field_names[i]) {
                return false;
            }
        }
        return true;

      case EXPR_UNION:
        if (x.data.union_.num_fields != y.data.union_.num_fields) {
            return false;
        }
        for (size_t i = 0; i < x.data.union_.num_fields; i++) {
            if (!expr_equal(x.data.union_.field_types[i],
                        y.data.union_.field_types[i])
                    || x.data.union_.field_names[i]
                        != y.data.union_.field_names[i]) {
                return false;
            }
        }
        return true;

      case EXPR_PACK:
        if (!expr_equal(*x.data.pack.type, *y.data.pack.type)
                || x.data.pack.num_assigns != y.data.pack.num_assigns) {
            return false;
        }
        for (size_t i = 0; i < x.data.pack.num_assigns; i++) {
            if (x.data.pack.field_names[i] != y.data.pack.field_names[i]
                    || !expr_equal(x.data.pack.assigns[i],
                        y.data.pack.assigns[i])) {
                return false;
            }
        }
        return true;

      case EXPR_MEMBER:
        if (!expr_equal(*x.data.member.record, *y.data.member.record)
                || x.data.member.field != y.data.member.field) {
            return false;
        }
        return true;

      case EXPR_POINTER:
        return expr_equal(*x.data.pointer, *y.data.pointer);

      case EXPR_REFERENCE:
        return expr_equal(*x.data.reference, *y.data.reference);

      case EXPR_DEREFERENCE:
        return expr_equal(*x.data.dereference, *y.data.dereference);
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
      case LIT_INTEGRAL:
        return x;
    }
}

Expr expr_copy(Expr x) {
    Expr y = {.tag = x.tag};

    switch (x.tag) {
      case EXPR_LITERAL:
        y.data.literal = literal_copy(x.data.literal);
        break;

      case EXPR_IDENT:
        y.data.ident = x.data.ident;
        break;

      case EXPR_FUNC_TYPE:
        y.data.func_type.ret_type = malloc(sizeof *y.data.func_type.ret_type);
        *y.data.func_type.ret_type = expr_copy(*x.data.func_type.ret_type);
        y.data.func_type.num_params = x.data.func_type.num_params;
        y.data.func_type.param_types = calloc(
            sizeof *y.data.func_type.param_types, y.data.func_type.num_params);
        y.data.func_type.param_names = calloc(
            sizeof *y.data.func_type.param_names, y.data.func_type.num_params);
        for (size_t i = 0; i < y.data.func_type.num_params; i++) {
            y.data.func_type.param_types[i] =
                expr_copy(x.data.func_type.param_types[i]);
            y.data.func_type.param_names[i] = x.data.func_type.param_names[i];
        }
        break;

      case EXPR_CALL:
        y.data.call.func = malloc(sizeof *y.data.call.func);
        *y.data.call.func = expr_copy(*x.data.call.func);
        y.data.call.num_args = x.data.call.num_args;
        y.data.call.args = calloc(sizeof *y.data.call.args,
            y.data.call.num_args);
        for (size_t i = 0; i < y.data.call.num_args; i++) {
            y.data.call.args[i] = expr_copy(x.data.call.args[i]);
        }
        break;

      case EXPR_STRUCT:
        y.data.struct_.num_fields = x.data.struct_.num_fields;
        y.data.struct_.field_types = calloc(sizeof *y.data.struct_.field_types,
            y.data.struct_.num_fields);
        y.data.struct_.field_names = calloc(sizeof *y.data.struct_.field_names,
            y.data.struct_.num_fields);
        for (size_t i = 0; i < y.data.struct_.num_fields; i++) {
            y.data.struct_.field_types[i] =
                expr_copy(x.data.struct_.field_types[i]);
            y.data.struct_.field_names[i] = x.data.struct_.field_names[i];
        }
        break;

      case EXPR_UNION:
        y.data.union_.num_fields = x.data.union_.num_fields;
        y.data.union_.field_types = calloc(sizeof *y.data.union_.field_types,
            y.data.union_.num_fields);
        y.data.union_.field_names = calloc(sizeof *y.data.union_.field_names,
            y.data.union_.num_fields);
        for (size_t i = 0; i < y.data.union_.num_fields; i++) {
            y.data.union_.field_types[i] =
                expr_copy(x.data.union_.field_types[i]);
            y.data.union_.field_names[i] = x.data.union_.field_names[i];
        }
        break;

      case EXPR_PACK:
        y.data.pack.type = malloc(sizeof *y.data.pack.type);
        *y.data.pack.type = expr_copy(*x.data.pack.type);
        y.data.pack.num_assigns = x.data.pack.num_assigns;
        y.data.pack.field_names = calloc(sizeof *y.data.pack.field_names,
            y.data.pack.num_assigns);
        y.data.pack.assigns = calloc(sizeof *y.data.pack.assigns,
            y.data.pack.num_assigns);
        for (size_t i = 0; i < y.data.pack.num_assigns; i++) {
            y.data.pack.field_names[i] = x.data.pack.field_names[i];
            y.data.pack.assigns[i] = expr_copy(x.data.pack.assigns[i]);
        }
        break;

      case EXPR_MEMBER:
        y.data.member.record = malloc(sizeof *y.data.member.record);
        *y.data.member.record = expr_copy(*x.data.member.record);
        y.data.member.field = x.data.member.field;
        break;

      case EXPR_POINTER:
        y.data.pointer = malloc(sizeof *y.data.pointer);
        *y.data.pointer = expr_copy(*x.data.pointer);
        break;

      case EXPR_REFERENCE:
        y.data.reference = malloc(sizeof *y.data.reference);
        *y.data.reference = expr_copy(*x.data.reference);
        break;

      case EXPR_DEREFERENCE:
        y.data.dereference = malloc(sizeof *y.data.dereference);
        *y.data.dereference = expr_copy(*x.data.dereference);
        break;
    }

    return y;
}

/***** Freeing ast nodes *****************************************************/
void expr_free(Expr expr) {
    switch (expr.tag) {
      case EXPR_LITERAL:
      case EXPR_IDENT:
        break;

      case EXPR_FUNC_TYPE:
        expr_free(*expr.data.func_type.ret_type);
        free(expr.data.func_type.ret_type);
        for (size_t i = 0; i < expr.data.func_type.num_params; i++) {
            expr_free(expr.data.func_type.param_types[i]);
        }
        free(expr.data.func_type.param_types);
        free(expr.data.func_type.param_names);
        break;

      case EXPR_CALL:
        expr_free(*expr.data.call.func);
        free(expr.data.call.func);
        for (size_t i = 0; i < expr.data.call.num_args; i++) {
            expr_free(expr.data.call.args[i]);
        }
        free(expr.data.call.args);
        break;

      case EXPR_STRUCT:
        for (size_t i = 0; i < expr.data.struct_.num_fields; i++) {
            expr_free(expr.data.struct_.field_types[i]);
        }
        free(expr.data.struct_.field_types);
        free(expr.data.struct_.field_names);
        break;

      case EXPR_UNION:
        for (size_t i = 0; i < expr.data.union_.num_fields; i++) {
            expr_free(expr.data.union_.field_types[i]);
        }
        free(expr.data.union_.field_types);
        free(expr.data.union_.field_names);
        break;

      case EXPR_PACK:
        expr_free(*expr.data.pack.type);
        free(expr.data.pack.type);
        for (size_t i = 0; i < expr.data.pack.num_assigns; i++) {
            expr_free(expr.data.pack.assigns[i]);
        }
        free(expr.data.pack.field_names);
        free(expr.data.pack.assigns);
        break;

      case EXPR_MEMBER:
        expr_free(*expr.data.member.record);
        free(expr.data.member.record);
        break;

      case EXPR_POINTER:
        expr_free(*expr.data.pointer);
        free(expr.data.pointer);
        break;

      case EXPR_REFERENCE:
        expr_free(*expr.data.reference);
        free(expr.data.reference);
        break;

      case EXPR_DEREFERENCE:
        expr_free(*expr.data.dereference);
        free(expr.data.dereference);
        break;
    }
}

void statement_free(Statement statement) {
    switch (statement.tag) {
      case STATEMENT_EMPTY:
        break;

      case STATEMENT_EXPR:
        expr_free(statement.data.expr);
        break;

      case STATEMENT_BLOCK:
        for (size_t i = 0; i < statement.data.block.num_statements; i++) {
            statement_free(statement.data.block.statements[i]);
        }
        free(statement.data.block.statements);
        break;

      case STATEMENT_DECL:
        expr_free(statement.data.decl.type);
        if (statement.data.decl.is_initialized) {
            expr_free(statement.data.decl.initial_value);
        }
        break;
    }
}

void top_level_free(TopLevel top_level) {
    switch (top_level.tag) {
      case TOP_LEVEL_FUNC:
        expr_free(top_level.data.func.ret_type);
        for (size_t i = 0; i < top_level.data.func.num_params; i++) {
            expr_free(top_level.data.func.param_types[i]);
        }
        free(top_level.data.func.param_types);
        free(top_level.data.func.param_names);
        for (size_t i = 0; i < top_level.data.func.num_statements; i++) {
            statement_free(top_level.data.func.statements[i]);
        }
        free(top_level.data.func.statements);
        break;
    }
}

void translation_unit_free(TranslationUnit unit) {
    for (size_t i = 0; i < unit.num_top_levels; i++) {
        top_level_free(unit.top_levels[i]);
    }
    free(unit.top_levels);
}

/***** Pretty-printing ast nodes *********************************************/
static void print_indentation_whitespace(FILE *to, int nesting) {
    for (int i = 0; i < nesting; i++) {
        fprintf(to, "    ");
    }
}

static void literal_pprint(FILE *to, int nesting, Literal literal) {
#define tag_to_string(tag, str) case tag: fprintf(to, str); break;

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

      case LIT_INTEGRAL:
        fprintf(to, "%" PRIu64, literal.data.integral);
        break;
    }

#undef tag_to_string
}

static void expr_pprint_(FILE *to, int nesting, Expr expr) {
    putc('(', to);
    expr_pprint(to, nesting, expr);
    putc(')', to);
}

void expr_pprint(FILE *to, int nesting, Expr expr) {
    switch (expr.tag) {
      case EXPR_LITERAL:
        literal_pprint(to, nesting, expr.data.literal);
        break;

      case EXPR_IDENT:
        fputs(expr.data.ident, to);
        break;

      case EXPR_FUNC_TYPE:
        expr_pprint_(to, nesting, *expr.data.func_type.ret_type);
        putc('[', to);
        for (size_t i = 0; i < expr.data.func_type.num_params; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            expr_pprint(to, nesting, expr.data.func_type.param_types[i]);
            if (expr.data.func_type.param_names[i] != NULL) {
                fprintf(to, " %s", expr.data.func_type.param_names[i]);
            }
        }
        putc(']', to);
        break;

      case EXPR_CALL:
        expr_pprint_(to, nesting, *expr.data.call.func);
        putc('(', to);
        for (size_t i = 0; i < expr.data.call.num_args; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            expr_pprint(to, nesting, expr.data.call.args[i]);
        }
        putc(')', to);
        break;

      case EXPR_STRUCT:
        fprintf(to, "struct { ");
        for (size_t i = 0; i < expr.data.struct_.num_fields; i++) {
            expr_pprint(to, nesting, expr.data.struct_.field_types[i]);
            fprintf(to, " %s; ", expr.data.struct_.field_names[i]);
        }
        putc('}', to);
        break;

      case EXPR_UNION:
        fprintf(to, "union { ");
        for (size_t i = 0; i < expr.data.union_.num_fields; i++) {
            expr_pprint(to, nesting, expr.data.union_.field_types[i]);
            fprintf(to, " %s; ", expr.data.union_.field_names[i]);
        }
        putc('}', to);
        break;

      case EXPR_PACK:
        expr_pprint_(to, nesting, *expr.data.pack.type);
        putc('{', to);
        for (size_t i = 0; i < expr.data.pack.num_assigns; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            fprintf(to, ".%s = ", expr.data.pack.field_names[i]);
            expr_pprint(to, nesting, expr.data.pack.assigns[i]);
        }
        putc('}', to);
        break;

      case EXPR_MEMBER:
        expr_pprint_(to, nesting, *expr.data.member.record);
        fprintf(to, ".%s", expr.data.member.field);
        break;

      case EXPR_POINTER:
        expr_pprint_(to, nesting, *expr.data.pointer);
        putc('*', to);
        break;

      case EXPR_REFERENCE:
        putc('&', to);
        expr_pprint_(to, nesting, *expr.data.reference);
        break;

      case EXPR_DEREFERENCE:
        putc('*', to);
        expr_pprint_(to, nesting, *expr.data.dereference);
        break;
    }
}

void statement_pprint(FILE *to, int nesting, Statement statement) {
    print_indentation_whitespace(to, nesting);

    switch (statement.tag) {
      case STATEMENT_EMPTY:
        fprintf(to, ";\n");
        break;

      case STATEMENT_EXPR:
        expr_pprint(to, nesting, statement.data.expr);
        fprintf(to, ";\n");
        break;

      case STATEMENT_BLOCK:
        fprintf(to, "{\n");
        for (size_t i = 0; i < statement.data.block.num_statements; i++) {
            statement_pprint(to, nesting + 1,
                statement.data.block.statements[i]);
        }
        print_indentation_whitespace(to, nesting);
        fprintf(to, "}\n");
        break;

      case STATEMENT_DECL:
        expr_pprint(to, nesting, statement.data.decl.type);
        fprintf(to, " %s", statement.data.decl.name);
        if (statement.data.decl.is_initialized) {
            fprintf(to, " = ");
            expr_pprint(to, nesting, statement.data.decl.initial_value);
        }
        fprintf(to, ";\n");
        break;
    }
}

void top_level_pprint(FILE *to, TopLevel top_level) {
    switch (top_level.tag) {
       case TOP_LEVEL_FUNC:
        expr_pprint(to, 0, top_level.data.func.ret_type);
        fprintf(to, " %s(", top_level.name);

        for (size_t i = 0; i < top_level.data.func.num_params; i++) {
            if (i > 0) {
                fprintf(to, ", ");
            }

            expr_pprint(to, 0, top_level.data.func.param_types[i]);
            if (top_level.data.func.param_names[i] != NULL) {
                fprintf(to, " %s", top_level.data.func.param_names[i]);
            }
        }
        fprintf(to, ") {\n");

        for (size_t i = 0; i < top_level.data.func.num_statements; i++) {
            statement_pprint(to, 1, top_level.data.func.statements[i]);
        }
        putc('}', to);
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
