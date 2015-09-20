#ifndef DEPENDENT_C_AST_SYNTAX_H
#define DEPENDENT_C_AST_SYNTAX_H

typedef struct {
    unsigned line;
    unsigned column;
} LocationInfo;

typedef enum {
    // Misc.
      EXPR_IDENT
    , EXPR_TYPE

    // Function type, constructor, and destructor.
    , EXPR_FORALL
    , EXPR_LAMBDA
    , EXPR_CALL

    // Identity type, constructor, and destructor.
    , EXPR_ID         // [A, B] -> Type
    , EXPR_REFLEXIVE  // [A : Type, x : A] -> x = x
    , EXPR_SUBSTITUTE // [x = y, T : [A] -> Type, T(x)] -> T(y)

    // Void type and destructor. (Obviously there is no constructor).
    , EXPR_VOID    // Type
    , EXPR_EXPLODE // [Void, A : Type] -> A

    // Boolean type, constructors, and destructor.
    , EXPR_BOOL       // Type
    , EXPR_BOOLEAN    // Bool
    , EXPR_IFTHENELSE // [b : Bool, T : [Bool] -> Type, T(true), T(false)] -> T(b)

    , EXPR_NAT     // Type
    , EXPR_NATURAL // Nat
    , EXPR_NAT_IND // One of:
// [n : Nat, T(0),   [x : Nat, [x = 0]   -> Void, T(x - 1)] -> T(x)] -> T(n)
// [n : Nat, T(MAX), [x : Nat, [x = MAX] -> Void, T(x + 1)] -> T(x)] -> T(n)

    // Sigma type, constructor, and destructor
    , EXPR_SIGMA
    , EXPR_PACK
    , EXPR_ACCESS
} ExprTag;

typedef struct Expr Expr;
struct Expr {
    LocationInfo location;

    ExprTag tag;
    union {
        const char *ident;
        // struct {} type;

        struct {
            size_t num_params;
            Expr *param_types;
            const char **param_names; // Values may be NULL if params not named.
            Expr *ret_type;
        } forall;
        struct {
            size_t num_params;
            Expr *param_types;
            const char **param_names;
            Expr *body;
        } lambda;
        struct {
            Expr *func;
            size_t num_args;
            Expr *args;
        } call;

        struct {
            Expr *expr1;
            Expr *expr2;
        } id;
        Expr *reflexive;
        struct {
            Expr *proof;
            Expr *family;
            Expr *instance;
        } substitute;

        // struct {} void_;
        struct {
            Expr *void_instance;
            Expr *into_type;
        } explode;

        // struct {} bool_;
        bool boolean;
        struct {
            Expr *predicate;
            Expr *then_;
            Expr *else_;
        } ifthenelse;

        // struct {} nat;
        uint64_t natural;
        struct {
            Expr *natural;
            bool goes_down;
            Expr *base_val;
            const char *ind_name;
            Expr *ind_val;
        } nat_ind;

        struct {
            size_t num_fields;
            const char **field_names; // May be NULL if params not named.
            Expr *field_types;
        } sigma;
        struct {
            Expr *as_type; // May be NULL if fields are non-dependent
            size_t num_fields;
            Expr *field_values;
        } pack;
        struct {
            Expr *record;
            size_t field_num;
        } access;
    };
};

#define literal_expr_type \
    ((Expr){ \
          .tag = EXPR_TYPE \
    })

#define literal_expr_void \
    ((Expr){ \
          .tag = EXPR_VOID \
    })

#define literal_expr_bool \
    ((Expr){ \
          .tag = EXPR_BOOL \
    })

#define literal_expr_nat \
    ((Expr){ \
          .tag = EXPR_NAT \
    })

typedef enum {
      TOP_LEVEL_EXPR_DECL
} TopLevelTag;

typedef struct {
    LocationInfo location;
    const char *name;

    TopLevelTag tag;
    union {
        struct {
            Expr type;
            Expr expr;
        } expr_decl;
    };
} TopLevel;

typedef struct {
    size_t num_top_levels;
    TopLevel *top_levels;
} TranslationUnit;

#endif /* DEPENDENT_C_AST_SYNTAX_H */
