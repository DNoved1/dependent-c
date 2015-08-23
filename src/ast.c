#include <stdbool.h>
#include <stdlib.h>

#include "dependent-c/ast.h"

void expr_free(Expr expr) {
    switch (expr.tag) {
      case EXPR_LITERAL:
        break;

      case EXPR_IDENT:
        free(expr.data.ident);
        break;

      case EXPR_FUNC_TYPE:
        expr_free(*expr.data.func_type.ret_type);
        free(expr.data.func_type.ret_type);
        for (size_t i = 0; i < expr.data.func_type.num_params; i++) {
            expr_free(expr.data.func_type.param_types[i]);
            free(expr.data.func_type.param_names[i]);
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
            free(expr.data.struct_.field_names[i]);
        }
        free(expr.data.struct_.field_types);
        free(expr.data.struct_.field_names);
        break;

      case EXPR_UNION:
        for (size_t i = 0; i < expr.data.union_.num_fields; i++) {
            expr_free(expr.data.union_.field_types[i]);
            free(expr.data.union_.field_names[i]);
        }
        free(expr.data.union_.field_types);
        free(expr.data.union_.field_names);
        break;

      case EXPR_PACK:
        expr_free(*expr.data.pack.type);
        free(expr.data.pack.type);
        for (size_t i = 0; i < expr.data.pack.num_assigns; i++) {
            free(expr.data.pack.field_names[i]);
            expr_free(expr.data.pack.assigns[i]);
        }
        free(expr.data.pack.field_names);
        free(expr.data.pack.assigns);
        break;

      case EXPR_MEMBER:
        expr_free(*expr.data.member.record);
        free(expr.data.member.record);
        free(expr.data.member.field);
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

      case EXPR_FUNC_TYPE_OR_CALL:
        expr_free(*expr.data.func_type_or_call.ret_type_or_func);
        free(expr.data.func_type_or_call.ret_type_or_func);
        for (size_t i = 0; i < expr.data.func_type_or_call.num_params_or_args;
                i++) {
            expr_free(expr.data.func_type_or_call.param_types_or_args[i]);
            free(expr.data.func_type_or_call.param_names[i]);
        }
        free(expr.data.func_type_or_call.param_types_or_args);
        free(expr.data.func_type_or_call.param_names);
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
        free(statement.data.decl.name);
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
        free(top_level.data.func.name);
        for (size_t i = 0; i < top_level.data.func.num_params; i++) {
            expr_free(top_level.data.func.param_types[i]);
            free(top_level.data.func.param_names[i]);
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
