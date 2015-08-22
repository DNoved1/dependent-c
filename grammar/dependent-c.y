%{
#include <ctype.h>  /* isspace, isalpha, isalnum, isdigit */
#include <stdint.h> /* unint64_t */
#include <stdlib.h> /* realloc */
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
}

%{
int yylex(YYSTYPE *lval, YYLTYPE *lloc, TokenStream *stream);
void yyerror(YYLTYPE *lloc, TokenStream *stream, const char *error_message);
%}

    /* Reserved Words */
%token TOK_TYPE     "type"
%token TOK_VOID     "void"
%token TOK_U8       "u8"
%token TOK_S8       "s8"
%token TOK_U16      TOK_S16
%token TOK_U32      TOK_S32
%token TOK_U64      TOK_S64
%token TOK_STRUCT   "struct"
%token TOK_UNION    "union"

    /* Symbols */
%token TOK_LPAREN   TOK_RPAREN
%token TOK_LSQUARE  TOK_RSQUARE
%token TOK_LBRACE   TOK_RBRACE
%token TOK_SEMICOLON
%token TOK_FULLSTOP
%token TOK_ASTERISK
%token TOK_AMPERSAND

    /* Integers */
%token <integral> TOK_INTEGRAL

    /* Identifiers */
%token <ident> TOK_IDENT

    /* Result types of each rule */
%type <expr> expr
%type <literal> literal

%%

expr:
      literal       { $$.tag = EXPR_LITERAL; $$.data.literal = $1;            }
    | TOK_IDENT     { $$.tag = EXPR_IDENT;   $$.data.ident = $1;              }
    | struct_type   { $$.tag = EXPR_STRUCT;                                   }
    | union_type    { $$.tag = EXPR_UNION;                                    }
    ;

literal:
      "type"        { $$.tag = LIT_TYPE;                                      }
    | "void"        { $$.tag = LIT_VOID;                                      }
    | "u8"          { $$.tag = LIT_U8;                                        }
    | "s8"          { $$.tag = LIT_S8;                                        }
    | TOK_U16       { $$.tag = LIT_U16;                                       }
    | TOK_S16       { $$.tag = LIT_S16;                                       }
    | TOK_U32       { $$.tag = LIT_U32;                                       }
    | TOK_S32       { $$.tag = LIT_S32;                                       }
    | TOK_U64       { $$.tag = LIT_U64;                                       }
    | TOK_S64       { $$.tag = LIT_S64;                                       }
    | TOK_INTEGRAL  { $$.tag = LIT_INTEGRAL; $$.data.integral = $1;           }
    ;

struct_type: "struct" struct_or_union_body;
union_type:  "union"  struct_or_union_body;

struct_or_union_body:
    TOK_LBRACE type_ident_decls TOK_RBRACE;
type_ident_decls:
    | type_ident_decls type_ident_decl;
type_ident_decl:
    expr TOK_IDENT TOK_SEMICOLON;

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
            || c == '&') {
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
