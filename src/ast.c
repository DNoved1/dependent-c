#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "dependent-c/ast.h"

/***** Freeing ast nodes *****************************************************/
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

/***** Pretty-printing ast nodes *********************************************/
static void print_indentation_whitespace(int nesting) {
    for (int i = 0; i < nesting; i++) {
        printf("    ");
    }
}

static void literal_pprint(int nesting, Literal literal) {
#define tag_to_string(tag, str) case tag: printf(str); break;

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
        printf("%" PRIu64, literal.data.integral);
        break;
    }

#undef tag_to_string
}

static void expr_pprint_(int nesting, Expr expr) {
    putchar('(');
    expr_pprint(nesting, expr);
    putchar(')');
}

void expr_pprint(int nesting, Expr expr) {
    bool first = true;

    switch (expr.tag) {
      case EXPR_LITERAL:
        literal_pprint(nesting, expr.data.literal);
        break;

      case EXPR_IDENT:
        fputs(expr.data.ident, stdout);
        break;

      case EXPR_FUNC_TYPE:
        expr_pprint_(nesting, *expr.data.func_type.ret_type);
        putchar('(');
        for (size_t i = 0; i < expr.data.func_type.num_params; i++) {
            if (first) {
                first = false;
            } else {
                printf(", ");
            }

            expr_pprint(nesting, expr.data.func_type.param_types[i]);
            if (expr.data.func_type.param_names[i] != NULL) {
                printf(" %s", expr.data.func_type.param_names[i]);
            }
        }
        putchar(')');
        break;

      case EXPR_CALL:
        expr_pprint_(nesting, *expr.data.call.func);
        putchar('(');
        for (size_t i = 0; i < expr.data.call.num_args; i++) {
            if (first) {
                first = false;
            } else {
                printf(", ");
            }

            expr_pprint(nesting, expr.data.call.args[i]);
        }
        putchar(')');
        break;

      case EXPR_STRUCT:
        printf("struct { ");
        for (size_t i = 0; i < expr.data.struct_.num_fields; i++) {
            expr_pprint(nesting, expr.data.struct_.field_types[i]);
            printf(" %s; ", expr.data.struct_.field_names[i]);
        }
        putchar('}');
        break;

      case EXPR_UNION:
        printf("union { ");
        for (size_t i = 0; i < expr.data.union_.num_fields; i++) {
            expr_pprint(nesting, expr.data.union_.field_types[i]);
            printf(" %s; ", expr.data.union_.field_names[i]);
        }
        putchar('}');
        break;

      case EXPR_PACK:
        expr_pprint_(nesting, *expr.data.pack.type);
        putchar('{');
        for (size_t i = 0; i < expr.data.pack.num_assigns; i++) {
            if (first) {
                first = false;
            } else {
                printf(", ");
            }

            printf(".%s = ", expr.data.pack.field_names[i]);
            expr_pprint(nesting, expr.data.pack.assigns[i]);
        }
        putchar('}');
        break;

      case EXPR_MEMBER:
        expr_pprint_(nesting, *expr.data.member.record);
        printf(".%s", expr.data.member.field);
        break;

      case EXPR_POINTER:
        expr_pprint_(nesting, *expr.data.pointer);
        putchar('*');
        break;

      case EXPR_REFERENCE:
        putchar('&');
        expr_pprint_(nesting, *expr.data.reference);
        break;

      case EXPR_DEREFERENCE:
        putchar('*');
        expr_pprint_(nesting, *expr.data.dereference);
        break;

      case EXPR_FUNC_TYPE_OR_CALL:
        expr_pprint_(nesting, *expr.data.func_type_or_call.ret_type_or_func);
        putchar('(');
        for (size_t i = 0; i < expr.data.func_type_or_call.num_params_or_args;
                i++) {
            if (first) {
                first = false;
            } else {
                printf(", ");
            }

            expr_pprint(nesting,
                expr.data.func_type_or_call.param_types_or_args[i]);
            if (expr.data.func_type_or_call.param_names[i] != NULL) {
                printf(" %s", expr.data.func_type_or_call.param_names[i]);
            }
        }
        putchar(')');
        break;
    }
}

void statement_pprint(int nesting, Statement statement) {
    print_indentation_whitespace(nesting);

    switch (statement.tag) {
      case STATEMENT_EMPTY:
        printf(";\n");
        break;

      case STATEMENT_EXPR:
        expr_pprint(nesting, statement.data.expr);
        printf(";\n");
        break;

      case STATEMENT_BLOCK:
        printf("{\n");
        for (size_t i = 0; i < statement.data.block.num_statements; i++) {
            statement_pprint(nesting + 1, statement.data.block.statements[i]);
        }
        print_indentation_whitespace(nesting);
        printf("}\n");
        break;

      case STATEMENT_DECL:
        expr_pprint(nesting, statement.data.decl.type);
        printf(" %s", statement.data.decl.name);
        if (statement.data.decl.is_initialized) {
            printf(" = ");
            expr_pprint(nesting, statement.data.decl.initial_value);
        }
        printf(";\n");
        break;
    }
}

void top_level_pprint(TopLevel top_level) {
    switch (top_level.tag) {
       case TOP_LEVEL_FUNC:
        expr_pprint(0, top_level.data.func.ret_type);
        printf(" %s(", top_level.data.func.name);

        bool first_param = true;
        for (size_t i = 0; i < top_level.data.func.num_params; i++) {
            if (first_param) {
                first_param = false;
            } else {
                printf(", ");
            }

            expr_pprint(0, top_level.data.func.param_types[i]);
            if (top_level.data.func.param_names[i] != NULL) {
                printf(" %s", top_level.data.func.param_names[i]);
            }
        }
        printf(") {\n");

        for (size_t i = 0; i < top_level.data.func.num_statements; i++) {
            statement_pprint(1, top_level.data.func.statements[i]);
        }
        putchar('}');
        break;
    }
}

void translation_unit_pprint(TranslationUnit unit) {
    bool first = true;
    for (size_t i = 0; i < unit.num_top_levels; i++) {
        if (first) {
            first = false;
        } else {
            putchar('\n');
        }

        top_level_pprint(unit.top_levels[i]);
    }
}
