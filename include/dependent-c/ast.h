#ifndef DEPENDENT_C_AST
#define DEPENDENT_C_AST

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/***** Literals **************************************************************/
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

/***** Expressions ***********************************************************/
typedef enum {
    // Misc.
      EXPR_LITERAL      // type, u8, s64, 42, etc
    , EXPR_IDENT        // foo, bar, baz, etc

    // Function type and destructor. Constructor is a declaration, not an
    // expression.
    , EXPR_FUNC_TYPE    // Expr '(' { Expr | Expr Ident }(',') ')'
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
            char **param_names; // Values may be NULL if params not named.
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
            Expr *type;
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
            char **param_names;
        } func_type_or_call;
    } data;
};

/***** Statements ************************************************************/
typedef enum {
      STATEMENT_BLOCK
    , STATEMENT_DECL
} StatementTag;

typedef struct Statement Statement;
struct Statement {
    StatementTag tag;
    union {
        struct {
            size_t num_statements;
            Statement *statements;
        } block;

        struct {
            Expr *type;
            char *name;
            bool is_initialized;
            Expr *initial_value;
        } decl;
    } data;
};

/* Free any resources associated with an expression. */
void expr_free(Expr expr);

/* Free any resources associated with an expression. */
void statement_free(Statement statement);

#endif /* DEPENDENT_C_AST */
