%{
#include <ctype.h>  /* isspace, isalpha, isalnum, isdigit */
#include <string.h> /* strcmp */

#include "dependent-c/general.h"
#include "dependent-c/memory.h"
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
        bool is_zero;
    } nat_ind_base_pattern;
    struct {
        bool adds;
        const char *name;
    } nat_ind_ind_pattern;
}

%{
int yylex(YYSTYPE *lval, YYLTYPE *lloc, Context *context);
void yyerror(YYLTYPE *lloc, Context *context, const char *error_message);
%}

    /* Reserved Words / Multicharacter symbols */
%token TOK_TYPE         "Type"
%token TOK_SINGLE_ARROW "->"
%token TOK_BACK_ARROW   "<-"
%token TOK_REFLEXIVE    "reflexive"
%token TOK_SUBSTITUTE   "substitute"
%token TOK_VOID         "Void"
%token TOK_EXPLODE      "explode"
%token TOK_BOOL         "Bool"
%token TOK_TRUE         "true"
%token TOK_FALSE        "false"
%token TOK_IF           "if"
%token TOK_THEN         "then"
%token TOK_ELSE         "else"
%token TOK_NAT          "Nat"
%token TOK_CASE         "case"
%token TOK_OF           "of"
%token TOK_DOUBLE_ARROW "=>"
%token TOK_NAT_MAX      "NAT_MAX"

    /* Integers */
%token <integral> TOK_INTEGRAL

    /* Identifiers */
%token <ident> TOK_IDENT

    /* Result types of each rule */
%type <expr> simple_expr postfix_expr prefix_expr identity_expr expr
%type <top_level> top_level top_level_
%type <unit> translation_unit

%type <type_ident_list> type_ident_list type_ident_list_
%type <type_ident> type_ident

%type <param_list> maybe_type_ident_list maybe_type_ident_list_
%type <param> maybe_type_ident
%type <arg_list> arg_list arg_list_

%type <nat_ind_base_pattern> nat_ind_base_pattern
%type <nat_ind_ind_pattern> nat_ind_ind_pattern

%%

main:
      translation_unit {
        context->ast = $1; }
    ;

simple_expr:
      '(' expr ')'  {
        $$ = $2; }
    | TOK_IDENT {
        $$.tag = EXPR_IDENT;
        $$.ident = $1; }
    | "Type" {
        $$.tag = EXPR_TYPE; }
    | "reflexive" '(' expr ')' {
        $$.tag = EXPR_REFLEXIVE;
        alloc_assign($$.reflexive, $3); }
    | "substitute" '(' expr[proof] ',' expr[family] ',' expr[instance] ')' {
        $$.tag = EXPR_SUBSTITUTE;
        alloc_assign($$.substitute.proof, $proof);
        alloc_assign($$.substitute.family, $family);
        alloc_assign($$.substitute.instance, $instance); }
    | "Void" {
        $$.tag = EXPR_VOID; }
    | "explode" '(' expr[instance] ',' expr[type] ')' {
        $$.tag = EXPR_EXPLODE;
        alloc_assign($$.explode.void_instance, $instance);
        alloc_assign($$.explode.into_type, $type); }
    | "Bool" {
        $$.tag = EXPR_BOOL; }
    | "true" {
        $$.tag = EXPR_BOOLEAN;
        $$.boolean = true; }
    | "false" {
        $$.tag = EXPR_BOOLEAN;
        $$.boolean = false; }
    | "Nat" {
        $$.tag = EXPR_NAT; }
    | TOK_INTEGRAL {
        $$.tag = EXPR_NATURAL;
        $$.natural = $1; }
    | '{' maybe_type_ident_list[fields] '}' {
        $$.tag = EXPR_SIGMA;
        $$.sigma.num_fields = $fields.len;
        $$.sigma.field_names = $fields.names;
        $$.sigma.field_types = $fields.types; }
    | '<' arg_list[values] '>' {
        $$.tag = EXPR_PACK;
        $$.pack.as_type = NULL;
        $$.pack.num_fields = $values.len;
        $$.pack.field_values = $values.args; }
    | '(' '<' arg_list[values] '>' ':' expr[type] ')' {
        $$.tag = EXPR_PACK;
        alloc_assign($$.pack.as_type, $type);
        $$.pack.num_fields = $values.len;
        $$.pack.field_values = $values.args; }
    ;

postfix_expr:
      simple_expr
    | postfix_expr[func] '(' arg_list[args] ')' {
        $$.tag = EXPR_CALL;
        alloc_assign($$.call.func, $func);
        $$.call.num_args = $args.len;
        $$.call.args = $args.args; }
    | postfix_expr[record] '[' TOK_INTEGRAL[field_num] ']' {
        $$.tag = EXPR_ACCESS;
        alloc_assign($$.access.record, $record);
        $$.access.field_num = $field_num; }
    ;

prefix_expr:
      postfix_expr
    | '[' maybe_type_ident_list[params] ']' "->" prefix_expr[ret_type] {
        $$.tag = EXPR_FORALL;
        $$.forall.num_params = $params.len;
        $$.forall.param_types = $params.types;
        $$.forall.param_names = $params.names;
        alloc($$.forall.ret_type); *$$.forall.ret_type = $ret_type; }
    | '\\' '(' type_ident_list[params] ')' "=>" prefix_expr[body] {
        $$.tag = EXPR_LAMBDA;
        $$.lambda.num_params = $params.len;
        $$.lambda.param_types = $params.types;
        $$.lambda.param_names = $params.idents;
        alloc_assign($$.lambda.body, $body); }
    | "if" expr[pred]
          "then" expr[then_]
          "else" prefix_expr[else_] {
        $$.tag = EXPR_IFTHENELSE;
        alloc_assign($$.ifthenelse.predicate, $pred);
        alloc_assign($$.ifthenelse.then_, $then_);
        alloc_assign($$.ifthenelse.else_, $else_); }
    | "case" expr[natural] "of"
          '|' nat_ind_base_pattern[base_pat] "=>" expr[base_val]
          '|' nat_ind_ind_pattern[ind_pat] "=>" prefix_expr[ind_val] {
        if ($base_pat.is_zero != $ind_pat.adds) {
            if ($base_pat.is_zero) {
                yyerror(&@ind_pat, context, "Expected inductive step to go "
                    "downwards when base case was \"0\".");
            } else {
                yyerror(&@ind_pat, context, "Expected inductive step to go "
                    "upwards when base case was \"NAT_MAX\"");
            }
            YYERROR;
        }
        $$.tag = EXPR_NAT_IND;
        alloc_assign($$.nat_ind.natural, $natural);
        $$.nat_ind.goes_down = $base_pat.is_zero;
        alloc_assign($$.nat_ind.base_val, $base_val);
        $$.nat_ind.ind_name = $ind_pat.name;
        alloc_assign($$.nat_ind.ind_val, $ind_val); }
    ;

nat_ind_base_pattern:
      TOK_INTEGRAL[base] {
        if ($base != 0) {
            yyerror(&@1, context, "Expected either \"0\" or \"NAT_MAX\" "
                "for the base case of natural induction.");
            YYERROR;
        }
        $$.is_zero = true; }
    | "NAT_MAX" {
        $$.is_zero = false; }
    ;
nat_ind_ind_pattern:
      TOK_IDENT[name] '+' TOK_INTEGRAL[step] {
        if ($step != 1) {
            yyerror(&@3, context, "Expected \"1\" for size of inductive "
                "step.");
            YYERROR;
        }
        $$.adds = true;
        $$.name = $name; }
    | TOK_IDENT[name] '-' TOK_INTEGRAL[step] {
        if ($step != 1) {
            yyerror(&@3, context, "Expected \"1\" for size of inductive "
                "step.");
            YYERROR;
        }
        $$.adds = false;
        $$.name = $name; }
    ;

identity_expr:
      prefix_expr
    | prefix_expr '=' prefix_expr {
        $$.tag = EXPR_ID;
        alloc_assign($$.id.expr1, $1);
        alloc_assign($$.id.expr2, $3); }
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
      expr[ret_type] "<-" TOK_IDENT[name] '(' type_ident_list[params] ')' '='
          expr[body] ';' {
        $$.tag = TOP_LEVEL_EXPR_DECL;
        $$.name = $name;

        $$.expr_decl.type.tag = EXPR_FORALL;
        $$.expr_decl.type.forall.num_params = $params.len;
        alloc_array($$.expr_decl.type.forall.param_types, $params.len);
        alloc_array($$.expr_decl.type.forall.param_names, $params.len);
        for (size_t i = 0; i < $params.len; i++) {
            $$.expr_decl.type.forall.param_types[i] = expr_copy(
                context, &$params.types[i]);
            $$.expr_decl.type.forall.param_names[i] = $params.idents[i];
        }
        alloc_assign($$.expr_decl.type.forall.ret_type, $ret_type);

        $$.expr_decl.expr.tag = EXPR_LAMBDA;
        $$.expr_decl.expr.lambda.num_params = $params.len;
        $$.expr_decl.expr.lambda.param_types = $params.types;
        $$.expr_decl.expr.lambda.param_names = $params.idents;
        alloc_assign($$.expr_decl.expr.lambda.body, $body); }
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
    | type_ident_list_
    ;
type_ident_list_:
      type_ident {
        $$.len = 1;
        alloc_array($$.types, 1);  $$.types[0] = $1.type;
        alloc_array($$.idents, 1); $$.idents[0] = $1.ident; }
    | type_ident_list ',' type_ident {
        $$ = $1;
        realloc_array($$.types, $$.len + 1);
        $$.types[$$.len] = $3.type;
        realloc_array($$.idents, $$.len + 1);
        $$.idents[$$.len] = $3.ident;
        $$.len += 1; }
    ;
type_ident:
    TOK_IDENT[ident] ':' expr[type] {
        $$.type = $type;
        $$.ident = $ident;
    };

maybe_type_ident_list:
      %empty {
        $$.len = 0;
        $$.types = NULL;
        $$.names = NULL; }
    | maybe_type_ident_list_
    ;
maybe_type_ident_list_:
      maybe_type_ident  {
        $$.len = 1;
        alloc_array($$.types, 1);
        $$.types[0] = $1.type;
        alloc_array($$.names, 1);
        $$.names[0] = $1.name; }
    | maybe_type_ident_list_ ',' maybe_type_ident {
        $$ = $1;
        realloc_array($$.types, $$.len + 1);
        $$.types[$$.len] = $3.type;
        realloc_array($$.names, $$.len + 1);
        $$.names[$$.len] = $3.name;
        $$.len += 1; }
    ;
maybe_type_ident:
      expr {
        $$.type = $1;
        $$.name = NULL; }
    | TOK_IDENT ':' expr {
        $$.type = $3;
        $$.name = $1; }
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

%%

static int token_stream_pop_char(TokenStream *stream) {
    int c = char_stream_pop(&stream->source);

    if (c == '\n') {
        stream->line += 1;
        stream->column = 1;
    } else if (c != EOF) {
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

    if (c == '\n') {
        stream->line -= 1;
    } else if (c != EOF) {
        stream->column -= 1;
    }
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
        check_is_reserved(Type,         TOK_TYPE)
        check_is_reserved(reflexive,    TOK_REFLEXIVE)
        check_is_reserved(substitute,   TOK_SUBSTITUTE)
        check_is_reserved(Void,         TOK_VOID)
        check_is_reserved(explode,      TOK_EXPLODE)
        check_is_reserved(Bool,         TOK_BOOL)
        check_is_reserved(true,         TOK_TRUE)
        check_is_reserved(false,        TOK_FALSE)
        check_is_reserved(if,           TOK_IF)
        check_is_reserved(then,         TOK_THEN)
        check_is_reserved(else,         TOK_ELSE)
        check_is_reserved(Nat,          TOK_NAT)
        check_is_reserved(case,         TOK_CASE)
        check_is_reserved(of,           TOK_OF)
        check_is_reserved(NAT_MAX,      TOK_NAT_MAX)
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
        if (c == '>') {
            return TOK_DOUBLE_ARROW;
        } else {
            token_stream_push_char(stream, c);
            return '=';
        }
    } else if (c == '-') {
        c = token_stream_pop_char(stream);
        if (c == '>') {
            return TOK_SINGLE_ARROW;
        } else {
            token_stream_push_char(stream, c);
            return '-';
        }
    } else if (c == '<') {
        c = token_stream_pop_char(stream);
        if (c == '-') {
            return TOK_BACK_ARROW;
        } else {
            token_stream_push_char(stream, c);
            return '<';
        }
    } else if (c == '(' || c == ')'
            || c == '[' || c == ']'
            || c == '{' || c == '}'
                        || c == '>'
            || c == ','
            || c == ';'
            || c == '+'
            || c == ':'
            || c == '\\'
            || c == '|') {
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
