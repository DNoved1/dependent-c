%{
#include <ctype.h>  /* isspace, isalpha, isalnum, isdigit */
#include <stdbool.h> /* true, false */
#include <stdint.h> /* unint64_t */
#include <stdlib.h> /* malloc, realloc, free */
#include <string.h> /* strcmp */

#include "dependent-c/lex.h"
#include "dependent-c/ast.h"
#include "dependent-c/general.h"
%}

%define api.pure full
%param {Context *context}
%locations

%union {
    /* Lexer values */
    uint64_t integral;
    const char *ident;

    /* Parser values */
    Literal literal;
    Expr expr;
    Statement statement;
    TopLevel top_level;
    TranslationUnit unit;

    struct {
        size_t len;
        Expr *types;
        const char **idents;
    } type_ident_list;
    struct {
        Expr type;
        const char *ident;
    } type_ident;

    struct {
        size_t len;
        Expr *types;
        const char **names; /* Vales may be null if not named */
    } param_list;
    struct {
        Expr type;
        const char *name; /* May be null if not named */
    } param;
    struct {
        size_t len;
        Expr *args;
    } arg_list;

    struct {
        size_t len;
        const char **field_names;
        Expr *assigns;
    } pack_init_list;
    struct {
        const char *field_name;
        Expr assign;
    } pack_init;

    struct {
        size_t len;
        Statement *statements;
    } statement_list;
}

%{
int yylex(YYSTYPE *lval, YYLTYPE *lloc, Context *context);
void yyerror(YYLTYPE *lloc, Context *context, const char *error_message);
%}

    /* Reserved Words / Multicharacter symbols */
%token TOK_TYPE     "type"
%token TOK_VOID     "void"
%token TOK_U8       "u8"
%token TOK_S8       "s8"
%token TOK_U16      "u16"
%token TOK_S16      "s16"
%token TOK_U32      "u32"
%token TOK_S32      "s32"
%token TOK_U64      "u64"
%token TOK_S64      "s64"
%token TOK_STRUCT   "struct"
%token TOK_UNION    "union"
%token TOK_RETURN   "return"

    /* Integers */
%token <integral> TOK_INTEGRAL

    /* Identifiers */
%token <ident> TOK_IDENT

    /* Result types of each rule */
%type <literal> literal
%type <expr> simple_expr postfix_expr prefix_expr expr
%type <statement> statement
%type <top_level> top_level
%type <unit> translation_unit

%type <type_ident_list> type_ident_list
%type <type_ident> type_ident

%type <param_list> param_list param_list_
%type <param> param
%type <arg_list> arg_list arg_list_

%type <pack_init_list> pack_init_list pack_init_list_
%type <pack_init> pack_init

%type <statement_list> statement_list block

%%

main:
      translation_unit {
        context->ast = $1; }
    ;

literal:
      "type"        { $$.tag = LIT_TYPE;                                      }
    | "void"        { $$.tag = LIT_VOID;                                      }
    | "u8"          { $$.tag = LIT_U8;                                        }
    | "s8"          { $$.tag = LIT_S8;                                        }
    | "u16"         { $$.tag = LIT_U16;                                       }
    | "s16"         { $$.tag = LIT_S16;                                       }
    | "u32"         { $$.tag = LIT_U32;                                       }
    | "s32"         { $$.tag = LIT_S32;                                       }
    | "u64"         { $$.tag = LIT_U64;                                       }
    | "s64"         { $$.tag = LIT_S64;                                       }
    | TOK_INTEGRAL  { $$.tag = LIT_INTEGRAL; $$.data.integral = $1;           }
    ;

simple_expr:
      '(' expr ')'  {
        $$ = $2; }
    | literal {
        $$.tag = EXPR_LITERAL;
        $$.data.literal = $1; }
    | TOK_IDENT {
        $$.tag = EXPR_IDENT;
        $$.data.ident = $1; }
    | "struct" '{' type_ident_list '}' {
        $$.tag = EXPR_STRUCT;
        $$.data.struct_.num_fields = $3.len;
        $$.data.struct_.field_types = $3.types;
        $$.data.struct_.field_names = $3.idents; }
    | "union" '{' type_ident_list '}' {
        $$.tag = EXPR_UNION;
        $$.data.union_.num_fields = $3.len;
        $$.data.union_.field_types = $3.types;
        $$.data.union_.field_names = $3.idents; }
    ;

postfix_expr:
      simple_expr
    | postfix_expr '*' {
        $$.tag = EXPR_POINTER;
        $$.data.pointer = malloc(sizeof $1);
        *$$.data.pointer = $1; }
    | postfix_expr '.' TOK_IDENT {
        $$.tag = EXPR_MEMBER;
        $$.data.member.record = malloc(sizeof $1);
        *$$.data.member.record = $1;
        $$.data.member.field = $3; }
    | postfix_expr '(' arg_list ')' {
        $$.tag = EXPR_CALL;
        $$.data.call.func = malloc(sizeof $1);
        *$$.data.call.func = $1;
        $$.data.call.num_args = $3.len;
        $$.data.call.args = $3.args; }
    | postfix_expr '[' param_list ']' {
        $$.tag = EXPR_FUNC_TYPE;
        $$.data.func_type.ret_type = malloc(sizeof $1);
        *$$.data.func_type.ret_type = $1;
        $$.data.func_type.num_params = $3.len;
        $$.data.func_type.param_types = $3.types;
        $$.data.func_type.param_names = $3.names; }
    | '(' expr ')' '{' pack_init_list '}' {
        $$.tag = EXPR_PACK;
        $$.data.pack.type = malloc(sizeof $2);
        *$$.data.pack.type = $2;
        $$.data.pack.num_assigns = $5.len;
        $$.data.pack.field_names = $5.field_names;
        $$.data.pack.assigns = $5.assigns; }
    ;

prefix_expr:
      postfix_expr
    | '&' prefix_expr {
        $$.tag = EXPR_REFERENCE;
        $$.data.reference = malloc(sizeof $2);
        *$$.data.reference = $2; }
    | '*' prefix_expr {
        $$.tag = EXPR_DEREFERENCE;
        $$.data.dereference = malloc(sizeof $2);
        *$$.data.dereference = $2; }
    ;

expr:
      prefix_expr
    ;

statement:
      ';' {
        $$.tag = STATEMENT_EMPTY; }
    | expr ';' {
        $$.tag = STATEMENT_EXPR;
        $$.data.expr = $1; }
    | "return" expr ';' {
        $$.tag = STATEMENT_RETURN;
        $$.data.expr = $2; }
    | block {
        $$.tag = STATEMENT_BLOCK;
        $$.data.block.num_statements = $1.len;
        $$.data.block.statements = $1.statements; }
    | expr TOK_IDENT ';' {
        $$.tag = STATEMENT_DECL;
        $$.data.decl.type = $1;
        $$.data.decl.name = $2;
        $$.data.decl.is_initialized = false; }
    | expr TOK_IDENT '=' expr ';' {
        $$.tag = STATEMENT_DECL;
        $$.data.decl.type = $1;
        $$.data.decl.name = $2;
        $$.data.decl.is_initialized = true;
        $$.data.decl.initial_value = $4; }
    ;

top_level:
    /* TODO: note that param_list allows non-named params, which we don't
             want here. */
      expr TOK_IDENT '(' param_list ')' block {
        $$.tag = TOP_LEVEL_FUNC;
        $$.data.func.ret_type = $1;
        $$.name = $2;
        $$.data.func.num_params = $4.len;
        $$.data.func.param_types = $4.types;
        $$.data.func.param_names = $4.names;
        $$.data.func.num_statements = $6.len;
        $$.data.func.statements = $6.statements; }
    ;

translation_unit:
      %empty {
        $$.num_top_levels = 0;
        $$.top_levels = NULL; }
    | translation_unit top_level {
        $$ = $1;
        $$.top_levels = realloc($$.top_levels,
            ($$.num_top_levels + 1) * sizeof *$$.top_levels);
        $$.top_levels[$$.num_top_levels] = $2;
        $$.num_top_levels += 1; }
    ;

type_ident_list:
      %empty {
        $$.len = 0;
        $$.types = NULL;
        $$.idents = NULL; }
    | type_ident_list type_ident {
        $$ = $1;
        $$.types = realloc($$.types, ($$.len + 1) * sizeof *$$.types);
        $$.types[$$.len] = $2.type;
        $$.idents = realloc($$.idents, ($$.len + 1) * sizeof *$$.idents);
        $$.idents[$$.len] = $2.ident;
        $$.len += 1; }
    ;
type_ident:
    expr TOK_IDENT ';' {
        $$.type = $1;
        $$.ident = $2;
    };

param_list:
      %empty {
        $$.len = 0;
        $$.types = NULL;
        $$.names = NULL; }
    | param_list_
    ;
param_list_:
      param  {
        $$.len = 1;
        $$.types = malloc(sizeof *$$.types);
        $$.types[0] = $1.type;
        $$.names = malloc(sizeof *$$.names);
        $$.names[0] = $1.name; }
    | param_list_ ',' param {
        $$ = $1;
        $$.types = realloc($$.types, ($$.len + 1) * sizeof *$$.types);
        $$.types[$$.len] = $3.type;
        $$.names = realloc($$.names, ($$.len + 1) * sizeof *$$.names);
        $$.names[$$.len] = $3.name;
        $$.len += 1; }
    ;
param:
      expr {
        $$.type = $1;
        $$.name = NULL; }
    | expr TOK_IDENT {
        $$.type = $1;
        $$.name = $2; }
    ;

arg_list:
      %empty {
        $$.len = 0;
        $$.args = NULL; }
    | arg_list_
    ;
arg_list_:
      expr {
        $$.len = 1;
        $$.args = malloc(sizeof *$$.args);
        $$.args[0] = $1; }
    | arg_list_ ',' expr {
        $$ = $1;
        $$.args = realloc($$.args, ($$.len + 1) * sizeof *$$.args);
        $$.args[$$.len] = $3;
        $$.len += 1; }
    ;

pack_init_list:
      %empty {
        $$.len = 0;
        $$.field_names = NULL;
        $$.assigns = NULL; }
    | pack_init_list_
    ;
pack_init_list_:
      pack_init {
        $$.len = 1;
        $$.field_names = malloc(sizeof *$$.field_names);
        $$.field_names[0] = $1.field_name;
        $$.assigns = malloc(sizeof *$$.assigns);
        $$.assigns[0] = $1.assign; }
    | pack_init_list_ ',' pack_init {
        $$ = $1;
        $$.field_names = realloc($$.field_names,
            ($$.len + 1) * sizeof *$$.field_names);
        $$.field_names[$$.len] = $3.field_name;
        $$.assigns = realloc($$.assigns, ($$.len + 1) * sizeof *$$.assigns);
        $$.assigns[$$.len] = $3.assign;
        $$.len += 1; }
    ;
pack_init:
      '.' TOK_IDENT '=' expr {
        $$.field_name = $2;
        $$.assign = $4; }
    ;

block:
      '{' statement_list '}' {
        $$ = $2; }
    ;

statement_list:
      %empty {
        $$.len = 0;
        $$.statements = NULL; }
    | statement_list statement {
        $$ = $1;
        $$.statements = realloc($$.statements,
            ($$.len + 1) * sizeof *$$.statements);
        $$.statements[$$.len] = $2;
        $$.len += 1; }
    ;

%%

static int token_stream_pop_char(TokenStream *stream) {
    int c = char_stream_pop(&stream->source);

    if (c == '\n') {
        stream->line += 1;
        stream->column = 1;
    } else if (c != EOF ){
        stream->column += 1;
    }

    return c;
}

// Note: does not reverse the line/column update performed in pop_char.
//       As such, line/column readings should be taken before popping/pushing
//       characters.
//
//       Can be fixed by keeping a stack of column numbers which are pushed/
//       popped on a '\n' character. Reward/Effort ratio is a bit low though.
static void token_stream_push_char(TokenStream *stream, int c) {
    char_stream_push(&stream->source, c);
}

static void skip_whitespace(TokenStream *stream) {
    while (true) {
        int c = token_stream_pop_char(stream);

        if (!isspace(c)) {
            token_stream_push_char(stream, c);
            break;
        }
    }
}

int yylex(YYSTYPE *lval, YYLTYPE *lloc, Context *context) {
start_of_function:;
    TokenStream *stream = &context->tokens;
    skip_whitespace(stream);

    lloc->first_line = stream->line;
    lloc->first_column = stream->column;

    int c = token_stream_pop_char(stream);
    if (isalpha(c) || c == '_') {
        size_t len = 0;
        char *ident = NULL;
        token_stream_push_char(stream, c);

        while (true) {
            c = token_stream_pop_char(stream);

            if (isalnum(c) || c == '_') {
                ident = realloc(ident, len + 1);
                ident[len] = c;
                len += 1;
            } else {
                token_stream_push_char(stream, c);
                break;
            }
        }

        ident = realloc(ident, len + 1);
        ident[len] = '\0';

#define check_is_reserved(word, then) \
    else if (strcmp(#word, ident) == 0) { \
        free(ident); \
        return then; \
    }

        if (false) {}
        check_is_reserved(type,     TOK_TYPE)
        check_is_reserved(void,     TOK_VOID)
        check_is_reserved(u8,       TOK_U8)
        check_is_reserved(s8,       TOK_S8)
        check_is_reserved(u16,      TOK_U16)
        check_is_reserved(s16,      TOK_S16)
        check_is_reserved(u32,      TOK_U32)
        check_is_reserved(s32,      TOK_S32)
        check_is_reserved(u64,      TOK_U64)
        check_is_reserved(s64,      TOK_S64)
        check_is_reserved(struct,   TOK_STRUCT)
        check_is_reserved(union,    TOK_UNION)
        check_is_reserved(return,   TOK_RETURN)
        else {
            const char *interned_ident = symbol_intern(&context->interns, ident);
            lval->ident = interned_ident;
            return TOK_IDENT;
        }

#undef check_is_reserved
    } else if (isdigit(c)) {
        uint64_t integral = 0;
        token_stream_push_char(stream, c);

        while (true) {
            c = token_stream_pop_char(stream);

            if (isdigit(c)) {
                integral = (integral * 10) + (c - '0');
            } else {
                token_stream_push_char(stream, c);
                break;
            }
        }

        lval->integral = integral;
        return TOK_INTEGRAL;
    } else if (c == '(' || c == ')'
            || c == '[' || c == ']'
            || c == '{' || c == '}'
            || c == ';'
            || c == '.'
            || c == ','
            || c == '*'
            || c == '&'
            || c == '=') {
        return c;
    } else if (c == EOF) {
        return 0;
    } else {
        fprintf(stderr, "Lexer encountered unexpected character '%c' at line "
            "%d, column %d. Skipping.\n", c,
            lloc->first_line, lloc->first_column);
        // In the presence of guarenteed tail-calls, such 'barbaric' measures
        // are not necessary. C does not have those however.
        goto start_of_function;
    }
}

void yyerror(YYLTYPE *lloc, Context *context, const char *error_message) {
    fprintf(stdout, "Parser error at line %d, column %d: %s\n",
        lloc->first_line, lloc->first_column, error_message);
}
