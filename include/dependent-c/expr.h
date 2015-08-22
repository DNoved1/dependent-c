#ifndef DEPENDENT_C_EXPR
#define DEPENDENT_C_EXPR

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef enum {
    // The type of types
      LIT_TYPE
    // Types representing integrals with a certain number of bits.
    , LIT_VOID
    , LIT_U8,       LIT_S8
    , LIT_U16,      LIT_S16
    , LIT_U32,      LIT_S32
    , LIT_U64,      LIT_S64
    // Literal integers
    , LIT_INTEGRAL
} LiteralTag;

typedef struct {
    LiteralTag tag;
    union {
        uint64_t integral;
    } data;
} Literal;

typedef enum {
    // Misc.
      EXPR_LITERAL      // type, u8, s64, 42, etc
    , EXPR_IDENT        // foo, bar, baz, etc

    // Function type and destructor. Constructor is a declaration, not an
    // expression.
    , EXPR_FUNC_TYPE    // Expr '(' { Expr | Expr Ident }(',')
    , EXPR_CALL         // Expr '(' { Expr }(',') ')'

    // Product/Union type, constructor, and destructor.
    , EXPR_STRUCT       // 'struct' '{' { Expr Ident ';' }() '}'
    , EXPR_UNION        // 'union' '{' { Expr Ident ';' }() '}'
    , EXPR_PACK         // '(' Expr ')' '{' { '.' Ident '=' Expr }(',') '}'
    , EXPR_MEMBER       // Expr '.' Ident

    // Pointer type, constructor, and destructor.
    , EXPR_POINTER      // Expr '*'
    , EXPR_REFERENCE    // '&' Expr
    , EXPR_DEREFERENCE  // '*' Expr

    // Ambiguous nodes
    , EXPR_FUNC_TYPE_OR_CALL
} ExprTag;

typedef struct Expr Expr;
struct Expr {
    ExprTag tag;
    union {
        Literal literal;
        char *ident;

        struct {
            Expr *ret_type;
            size_t num_params;
            Expr *param_types;
            bool *param_named;
            char **param_names;
        } func_type;
        struct {
            Expr *func;
            size_t num_args;
            Expr *args;
        } call;

        struct {
            size_t num_fields;
            Expr *field_types;
            char **field_names;
        } struct_;
        struct {
            size_t num_fields;
            Expr *field_types;
            char **field_names;
        } union_;
        struct {
            size_t num_assigns;
            char **field_names;
            Expr *assigns;
        } pack;
        struct {
            Expr *record;
            char *field;
        } member;

        Expr *pointer;
        Expr *reference;
        Expr *dereference;

        struct {
            Expr *ret_type_or_func;
            size_t num_params_or_args;
            Expr *param_types_or_args;
        } func_type_or_call;
    } data;
};

/* Free any resources associated with an expression. */
void expr_free(Expr expr);

#endif /* DEPENDENT_C_EXPR */
