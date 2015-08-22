#include <stdbool.h>
#include <stdlib.h>

#include "dependent-c/expr.h"

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
            if (expr.data.func_type.param_named[i]) {
                free(expr.data.func_type.param_names[i]);
            }
        }
        free(expr.data.func_type.param_types);
        free(expr.data.func_type.param_named);
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
    }
}
