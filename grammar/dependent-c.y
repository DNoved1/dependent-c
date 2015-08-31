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

    struct {
        size_t len;
        Expr *ifs;
        Block *thens;
    } if_list;
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
%token TOK_BOOL     "bool"
%token TOK_TRUE     "true"
%token TOK_FALSE    "false"
%token TOK_STRUCT   "struct"
%token TOK_UNION    "union"
%token TOK_RETURN   "return"
%token TOK_IF       "if"
%token TOK_ELSE     "else"
%token TOK_EQ       "=="
%token TOK_NE       "!="
%token TOK_LTE      "<="
%token TOK_GTE      ">="

    /* Integers */
%token <integral> TOK_INTEGRAL

    /* Identifiers */
%token <ident> TOK_IDENT

    /* Result types of each rule */
%type <literal> literal
%type <expr> simple_expr postfix_expr prefix_expr add_expr relational_expr
%type <expr> equality_expr expr
%type <statement> statement
%type <if_list> else_if_parts
%type <statement_list> else_part
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
    | "bool"        { $$.tag = LIT_BOOL;                                      }
    | "true"        { $$.tag = LIT_BOOLEAN; $$.boolean = true;                }
    | "false"       { $$.tag = LIT_BOOLEAN; $$.boolean = false;               }
    | TOK_INTEGRAL  { $$.tag = LIT_INTEGRAL; $$.integral = $1;                }
    ;

simple_expr:
      '(' expr ')'  {
        $$ = $2; }
    | literal {
        $$.tag = EXPR_LITERAL;
        $$.literal = $1; }
    | TOK_IDENT {
        $$.tag = EXPR_IDENT;
        $$.ident = $1; }
    | "struct" '{' type_ident_list '}' {
        $$.tag = EXPR_STRUCT;
        $$.struct_.num_fields = $3.len;
        $$.struct_.field_types = $3.types;
        $$.struct_.field_names = $3.idents; }
    | "union" '{' type_ident_list '}' {
        $$.tag = EXPR_UNION;
        $$.union_.num_fields = $3.len;
        $$.union_.field_types = $3.types;
        $$.union_.field_names = $3.idents; }
    ;

postfix_expr:
      simple_expr
    | postfix_expr '*' {
        $$.tag = EXPR_POINTER;
        $$.pointer = malloc(sizeof $1);
        *$$.pointer = $1; }
    | postfix_expr '.' TOK_IDENT {
        $$.tag = EXPR_MEMBER;
        $$.member.record = malloc(sizeof $1);
        *$$.member.record = $1;
        $$.member.field = $3; }
    | postfix_expr '(' arg_list ')' {
        $$.tag = EXPR_CALL;
        $$.call.func = malloc(sizeof $1);
        *$$.call.func = $1;
        $$.call.num_args = $3.len;
        $$.call.args = $3.args; }
    | postfix_expr '[' param_list ']' {
        $$.tag = EXPR_FUNC_TYPE;
        $$.func_type.ret_type = malloc(sizeof $1);
        *$$.func_type.ret_type = $1;
        $$.func_type.num_params = $3.len;
        $$.func_type.param_types = $3.types;
        $$.func_type.param_names = $3.names; }
    | '(' expr ')' '{' pack_init_list '}' {
        $$.tag = EXPR_PACK;
        $$.pack.type = malloc(sizeof $2);
        *$$.pack.type = $2;
        $$.pack.num_assigns = $5.len;
        $$.pack.field_names = $5.field_names;
        $$.pack.assigns = $5.assigns; }
    ;

prefix_expr:
      postfix_expr
    | '&' prefix_expr {
        $$.tag = EXPR_REFERENCE;
        $$.reference = malloc(sizeof $2);
        *$$.reference = $2; }
    | '*' prefix_expr {
        $$.tag = EXPR_DEREFERENCE;
        $$.dereference = malloc(sizeof $2);
        *$$.dereference = $2; }
    ;

add_expr:
      prefix_expr
    | add_expr '+' prefix_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_ADD;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    | add_expr '-' prefix_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_SUB;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    ;

relational_expr:
      add_expr
    | relational_expr '<' add_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_LT;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    | relational_expr "<=" add_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_LTE;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    | relational_expr '>' add_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_GT;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    | relational_expr ">=" add_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_GTE;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    ;

equality_expr:
      relational_expr
    | equality_expr "==" relational_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_EQ;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    | equality_expr "!=" relational_expr {
        $$.tag = EXPR_BIN_OP;
        $$.bin_op.op = BIN_OP_NE;
        $$.bin_op.expr1 = malloc(sizeof $1);
        *$$.bin_op.expr1 = $1;
        $$.bin_op.expr2 = malloc(sizeof $3);
        *$$.bin_op.expr2 = $3; }
    ;

expr:
      equality_expr
    ;

statement:
      ';' {
        $$.tag = STATEMENT_EMPTY; }
    | expr ';' {
        $$.tag = STATEMENT_EXPR;
        $$.expr = $1; }
    | "return" expr ';' {
        $$.tag = STATEMENT_RETURN;
        $$.expr = $2; }
    | block {
        $$.tag = STATEMENT_BLOCK;
        $$.block.num_statements = $1.len;
        $$.block.statements = $1.statements; }
    | expr TOK_IDENT ';' {
        $$.tag = STATEMENT_DECL;
        $$.decl.type = $1;
        $$.decl.name = $2;
        $$.decl.is_initialized = false; }
    | expr TOK_IDENT '=' expr ';' {
        $$.tag = STATEMENT_DECL;
        $$.decl.type = $1;
        $$.decl.name = $2;
        $$.decl.is_initialized = true;
        $$.decl.initial_value = $4; }
    | "if" '(' expr ')' block else_if_parts else_part {
        $$.tag = STATEMENT_IFTHENELSE;
        $$.ifthenelse.ifs = realloc($6.ifs,
            ($6.len + 1) * sizeof *$$.ifthenelse.ifs);
        memmove(&$$.ifthenelse.ifs[1], &$$.ifthenelse.ifs[0],
            $6.len * sizeof *$$.ifthenelse.ifs);
        $$.ifthenelse.ifs[0] = $3;
        $$.ifthenelse.thens = realloc($6.thens,
            ($6.len + 1) * sizeof *$$.ifthenelse.thens);
        $$.ifthenelse.thens[0].num_statements = $5.len;
        $$.ifthenelse.thens[0].statements = $5.statements;
        $$.ifthenelse.else_.num_statements = $7.len;
        $$.ifthenelse.else_.statements = $7.statements;
        $$.ifthenelse.num_ifs = $6.len + 1; }
    ;

else_if_parts:
      %empty {
        $$.len = 0;
        $$.ifs = NULL;
        $$.thens = NULL; }
    | else_if_parts "else" "if" '(' expr ')' block {
        $$ = $1;
        $$.ifs = realloc($$.ifs, ($$.len + 1) * sizeof *$$.ifs);
        $$.ifs[$$.len] = $5;
        $$.thens = realloc($$.thens, ($$.len + 1) * sizeof *$$.thens);
        $$.thens[$$.len].num_statements = $7.len;
        $$.thens[$$.len].statements = $7.statements;
        $$.len += 1; }
    ;

else_part:
      %empty {
        $$.len = 0;
        $$.statements = NULL; }
    | "else" block {
        $$ = $2; }
    ;

top_level:
    /* TODO: note that param_list allows non-named params, which we don't
             want here. */
      expr TOK_IDENT '(' param_list ')' block {
        $$.tag = TOP_LEVEL_FUNC;
        $$.func.ret_type = $1;
        $$.name = $2;
        $$.func.num_params = $4.len;
        $$.func.param_types = $4.types;
        $$.func.param_names = $4.names;
        $$.func.num_statements = $6.len;
        $$.func.statements = $6.statements; }
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
        check_is_reserved(bool,     TOK_BOOL)
        check_is_reserved(true,     TOK_TRUE)
        check_is_reserved(false,    TOK_FALSE)
        check_is_reserved(struct,   TOK_STRUCT)
        check_is_reserved(union,    TOK_UNION)
        check_is_reserved(return,   TOK_RETURN)
        check_is_reserved(if,       TOK_IF)
        check_is_reserved(else,     TOK_ELSE)
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
    } else if (c == '=') {
        c = token_stream_pop_char(stream);
        if (c == '=') {
            return TOK_EQ;
        } else {
            token_stream_push_char(stream, c);
            return '=';
        }
    } else if (c == '!') {
        c = token_stream_pop_char(stream);
        if (c == '=') {
            return TOK_NE;
        } else {
            token_stream_push_char(stream, c);
            fprintf(stderr, "Lexer encountered unexpected character '!' at "
                "line %d, column %d. Skipping.\n",
                lloc->first_line, lloc->first_column);
            goto start_of_function;
        }
    } else if (c == '<') {
        c = token_stream_pop_char(stream);
        if (c == '=') {
            return TOK_LTE;
        } else {
            token_stream_push_char(stream, c);
            return '<';
        }
    } else if (c == '>') {
        c = token_stream_pop_char(stream);
        if (c == '=') {
            return TOK_GTE;
        } else {
            token_stream_push_char(stream, c);
            return '>';
        }
    } else if (c == '(' || c == ')'
            || c == '[' || c == ']'
            || c == '{' || c == '}'
            || c == ';'
            || c == '.'
            || c == ','
            || c == '*'
            || c == '&'
            || c == '+'
            || c == '-') {
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
