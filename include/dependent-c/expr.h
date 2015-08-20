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
    , EXPR_FUNC_TYPE    // type int_filter = bool(int arg);
    , EXPR_CALL         // ((int_filter)foo)(42);

    // Product/Union type, constructor, and destructor.
    , EXPR_STRUCT       // type pair_t = struct { int fst; char snd; }
    , EXPR_UNION        // type either_t = union { int fst; char snd; }
    , EXPR_PACK         // pair_t pair = (pair){.fst = 1, .snd = 2}
    , EXPR_MEMBER       // char snd = pair.snd;

    // Pointer type, constructor, and destructor.
    , EXPR_POINTER
    , EXPR_REFERENCE
    , EXPR_DEREFERENCE
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
    } data;
};

/* Free any resources associated with an expression. */
void expr_free(Expr expr);

#endif /* DEPENDENT_C_EXPR */
