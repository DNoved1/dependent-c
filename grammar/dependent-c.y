%{
#include <ctype.h>  /* isspace, isalpha, isalnum, isdigit */
#include <stdbool.h> /* true, false */
#include <stdint.h> /* unint64_t */
#include <string.h> /* strcmp */

#include "dependent-c/ast.h"
#include "dependent-c/general.h"
#include "dependent-c/lex.h"
#include "dependent-c/memory.h"

#define PARSER_CONSTRUCT_BIN_OP(__op__) \
    do { \
        yyval.expr.tag = EXPR_BIN_OP; \
        yyval.expr.bin_op.op = __op__; \
        alloc(yyval.expr.bin_op.expr1); \
        *yyval.expr.bin_op.expr1 = yyvsp[-2].expr; \
        alloc(yyval.expr.bin_op.expr2); \
        *yyval.expr.bin_op.expr2 = yyvsp[0].expr; \
    } while (0)
%}

%define api.pure full
%param {Context *context}
%locations
%define parse.error verbose

%union {
    /* Lexer values */
    uint64_t integral;
    const char *ident;

    /* Parser values */
    Literal literal;
    Expr expr;
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
}

%{
int yylex(YYSTYPE *lval, YYLTYPE *lloc, Context *context);
void yyerror(YYLTYPE *lloc, Context *context, const char *error_message);
%}

    /* Reserved Words / Multicharacter symbols */
%token TOK_TYPE     "type"
%token TOK_VOID     "void"
%token TOK_U64      "u64"
%token TOK_BOOL     "bool"
%token TOK_TRUE     "true"
%token TOK_FALSE    "false"
%token TOK_STRUCT   "struct"
%token TOK_UNION    "union"
%token TOK_IF       "if"
%token TOK_THEN     "then"
%token TOK_ELSE     "else"
%token TOK_REFLEXIVE "reflexive"
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
%type <expr> equality_expr identity_expr expr
%type <top_level> top_level top_level_
%type <unit> translation_unit

%type <type_ident_list> type_ident_list
%type <type_ident> type_ident

%type <param_list> param_list param_list_
%type <param> param
%type <arg_list> arg_list arg_list_

%type <pack_init_list> pack_init_list pack_init_list_
%type <pack_init> pack_init

%%

main:
      translation_unit {
        context->ast = $1; }
    ;

literal:
      "type"        { $$.tag = LIT_TYPE;                                      }
    | "void"        { $$.tag = LIT_VOID;                                      }
    | "u64"         { $$.tag = LIT_U64;                                       }
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
        alloc($$.pointer);
        *$$.pointer = $1; }
    | postfix_expr '.' TOK_IDENT {
        $$.tag = EXPR_MEMBER;
        alloc($$.member.record);
        *$$.member.record = $1;
        $$.member.field = $3; }
    | postfix_expr '(' arg_list ')' {
        $$.tag = EXPR_CALL;
        alloc($$.call.func);
        *$$.call.func = $1;
        $$.call.num_args = $3.len;
        $$.call.args = $3.args; }
    | "reflexive" '(' expr ')' {
        $$.tag = EXPR_REFLEXIVE;
        alloc($$.pointer);
        *$$.pointer = $3; }
    | postfix_expr '[' param_list ']' {
        $$.tag = EXPR_FUNC_TYPE;
        alloc($$.func_type.ret_type);
        *$$.func_type.ret_type = $1;
        $$.func_type.num_params = $3.len;
        $$.func_type.param_types = $3.types;
        $$.func_type.param_names = $3.names; }
    | '[' expr ']' '{' pack_init_list '}' {
        $$.tag = EXPR_PACK;
        alloc($$.pack.type);
        *$$.pack.type = $2;
        $$.pack.num_assigns = $5.len;
        $$.pack.field_names = $5.field_names;
        $$.pack.assigns = $5.assigns; }
    ;

prefix_expr:
      postfix_expr
    | "if" expr "then" expr "else" prefix_expr {
        $$.tag = EXPR_IFTHENELSE;
        alloc($$.ifthenelse.predicate);
        *$$.ifthenelse.predicate = $2;
        alloc($$.ifthenelse.then_);
        *$$.ifthenelse.then_ = $4;
        alloc($$.ifthenelse.else_);
        *$$.ifthenelse.else_ = $6; }
    | '&' prefix_expr {
        $$.tag = EXPR_REFERENCE;
        alloc($$.pointer);
        *$$.pointer = $2; }
    | '*' prefix_expr {
        $$.tag = EXPR_DEREFERENCE;
        alloc($$.pointer);
        *$$.pointer = $2; }
    ;

add_expr:
      prefix_expr
    | add_expr '+' prefix_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_ADD); }
    | add_expr '-' prefix_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_SUB); }
    ;

relational_expr:
      add_expr
    | relational_expr '<' add_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_LT); }
    | relational_expr "<=" add_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_LTE); }
    | relational_expr '>' add_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_GT); }
    | relational_expr ">=" add_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_GTE); }
    ;

equality_expr:
      relational_expr
    | equality_expr "==" relational_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_EQ); }
    | equality_expr "!=" relational_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_NE); }
    ;

identity_expr:
      equality_expr
    | equality_expr '=' equality_expr {
        PARSER_CONSTRUCT_BIN_OP(BIN_OP_ID); }
    ;

expr:
      identity_expr {
        $$.location.line = @1.first_line;
        $$.location.column = @1.first_column; }
    ;

top_level:
      top_level_ {
        $$.location.line = @1.first_line;
        $$.location.column = @1.first_column; }
    ;

top_level_:
    /* TODO: note that param_list allows non-named params, which we don't
             want here. */
      expr TOK_IDENT '(' param_list ')' '=' expr ';' {
        $$.tag = TOP_LEVEL_FUNC;
        $$.func.ret_type = $1;
        $$.name = $2;
        $$.func.num_params = $4.len;
        $$.func.param_types = $4.types;
        $$.func.param_names = $4.names;
        $$.func.body = $7; }
    ;

translation_unit:
      %empty {
        $$.num_top_levels = 0;
        $$.top_levels = NULL; }
    | translation_unit top_level {
        $$ = $1;
        realloc_array($$.top_levels, $$.num_top_levels + 1);
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
        realloc_array($$.types, $$.len + 1);
        $$.types[$$.len] = $2.type;
        realloc_array($$.idents, $$.len + 1);
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
        alloc_array($$.types, 1);
        $$.types[0] = $1.type;
        alloc_array($$.names, 1);
        $$.names[0] = $1.name; }
    | param_list_ ',' param {
        $$ = $1;
        realloc_array($$.types, $$.len + 1);
        $$.types[$$.len] = $3.type;
        realloc_array($$.names, $$.len + 1);
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
        alloc_array($$.args, 1);
        $$.args[0] = $1; }
    | arg_list_ ',' expr {
        $$ = $1;
        realloc_array($$.args, $$.len + 1);
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
        alloc_array($$.field_names, 1);
        $$.field_names[0] = $1.field_name;
        alloc_array($$.assigns, 1);
        $$.assigns[0] = $1.assign; }
    | pack_init_list_ ',' pack_init {
        $$ = $1;
        realloc_array($$.field_names, $$.len + 1);
        $$.field_names[$$.len] = $3.field_name;
        realloc_array($$.assigns, $$.len + 1);
        $$.assigns[$$.len] = $3.assign;
        $$.len += 1; }
    ;
pack_init:
      '.' TOK_IDENT '=' expr {
        $$.field_name = $2;
        $$.assign = $4; }
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
                realloc_array(ident, len + 1);
                ident[len] = c;
                len += 1;
            } else {
                token_stream_push_char(stream, c);
                break;
            }
        }

        realloc_array(ident, len + 1);
        ident[len] = '\0';

#define check_is_reserved(word, then) \
    else if (strcmp(#word, ident) == 0) { \
        dealloc(ident); \
        return then; \
    }

        if (false) {}
        check_is_reserved(type,     TOK_TYPE)
        check_is_reserved(void,     TOK_VOID)
        check_is_reserved(u64,      TOK_U64)
        check_is_reserved(bool,     TOK_BOOL)
        check_is_reserved(true,     TOK_TRUE)
        check_is_reserved(false,    TOK_FALSE)
        check_is_reserved(struct,   TOK_STRUCT)
        check_is_reserved(union,    TOK_UNION)
        check_is_reserved(if,       TOK_IF)
        check_is_reserved(then,     TOK_THEN)
        check_is_reserved(else,     TOK_ELSE)
        check_is_reserved(reflexive, TOK_REFLEXIVE)
        else {
            const char *interned_ident = symbol_intern(&context->interns, ident);
            dealloc(ident);
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
