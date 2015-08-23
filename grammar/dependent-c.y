%{
#include <ctype.h>  /* isspace, isalpha, isalnum, isdigit */
#include <stdint.h> /* unint64_t */
#include <stdlib.h> /* malloc, realloc, free */
#include <string.h> /* strcmp */

#include "dependent-c/lex.h"
#include "dependent-c/expr.h"
%}

%define api.pure full
%param {TokenStream *stream}
%locations

%union {
    /* Lexer values */
    uint64_t integral;
    char *ident;

    /* Parser values */
    Expr expr;
    Literal literal;

    struct {
        size_t len;
        Expr *types;
        char **idents;
    } type_ident_list;
    struct {
        Expr type;
        char *ident;
    } type_ident;

    struct {
        size_t len;
        Expr *types;
        char **names; /* Vales may be null if not named */
    } param_list;
    struct {
        Expr type;
        char *name; /* May be null if not named */
    } param;

    struct {
        size_t len;
        char **field_names;
        Expr *assigns;
    } pack_init_list;
    struct {
        char *field_name;
        Expr assign;
    } pack_init;
}

%{
int yylex(YYSTYPE *lval, YYLTYPE *lloc, TokenStream *stream);
void yyerror(YYLTYPE *lloc, TokenStream *stream, const char *error_message);
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

    /* Integers */
%token <integral> TOK_INTEGRAL

    /* Identifiers */
%token <ident> TOK_IDENT

    /* Result types of each rule */
%type <expr> simple_expr postfix_expr prefix_expr expr
%type <literal> literal

%type <type_ident_list> type_ident_list
%type <type_ident> type_ident

%type <param_list> param_list param_list_
%type <param> param

%type <pack_init_list> pack_init_list pack_init_list_
%type <pack_init> pack_init

%%

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
    | postfix_expr '(' param_list ')' {
        $$.tag = EXPR_FUNC_TYPE_OR_CALL;
        $$.data.func_type_or_call.ret_type_or_func = malloc(sizeof $1);
        *$$.data.func_type_or_call.ret_type_or_func = $1;
        $$.data.func_type_or_call.num_params_or_args = $3.len;
        $$.data.func_type_or_call.param_types_or_args = $3.types;
        $$.data.func_type_or_call.param_names = $3.names; }
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
    | '&' expr {
        $$.tag = EXPR_REFERENCE;
        $$.data.reference = malloc(sizeof $2);
        *$$.data.reference = $2; }
    | '*' expr {
        $$.tag = EXPR_DEREFERENCE;
        $$.data.dereference = malloc(sizeof $2);
        *$$.data.dereference = $2; }
    ;

expr:
      prefix_expr
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

type_ident_list:
      %empty {
        $$.len = 0;
        $$.types = NULL;
        $$.idents = NULL; }
    | type_ident_list type_ident {
        $$ = $1;
        $$.types = realloc($$.types, ($$.len + 1) * sizeof *$$.types);
        $$.types[$$.len] = $2.type;
        $$.idents = realloc($$.types, ($$.len + 1) * sizeof *$$.idents);
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

int yylex(YYSTYPE *lval, YYLTYPE *lloc, TokenStream *stream) {
start_of_function:
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
        else {
            lval->ident = ident;
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

void yyerror(YYLTYPE *lloc, TokenStream *stream, const char *error_message) {
    fprintf(stdout, "Parser error at line %d, column %d: %s\n",
        lloc->first_line, lloc->first_column, error_message);
}
