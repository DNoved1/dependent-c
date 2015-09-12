#ifndef DEPENDENT_C_AST
#define DEPENDENT_C_AST

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef struct Expr             Expr;
typedef struct Block            Block;
typedef struct TopLevel         TopLevel;
typedef struct TranslationUnit  TranslationUnit;

/***** Location Information **************************************************/
typedef struct {
    unsigned line;
    unsigned column;
} LocationInfo;

/* For printing error messages. */
void location_pprint(const char *file, const LocationInfo *info);

/***** Expressions ***********************************************************/
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

void expr_free(Expr *expr);
Expr expr_copy(const Expr *x);

void expr_pprint(FILE *to, const Expr *expr);

/* Determine if two expressions are exactly equivalent. Does not take into
 * account alpha equivalence.
 */
bool expr_equal(const Expr *x, const Expr *y);

// Cyclical include problems...
#include "dependent-c/symbol_table.h"

/* Calculate the set of free variables in an expression. */
void expr_free_vars(const Expr *expr, SymbolSet *set);

/***** Top-Level Definitions *************************************************/
typedef enum {
      TOP_LEVEL_EXPR_DECL
} TopLevelTag;

struct TopLevel {
    LocationInfo location;
    const char *name;

    TopLevelTag tag;
    union {
        struct {
            Expr type;
            Expr expr;
        } expr_decl;
    };
};

void top_level_free(TopLevel *top_level);
void top_level_pprint(FILE *to, const TopLevel *top_level);

/***** Translation Units *****************************************************/

struct TranslationUnit {
    size_t num_top_levels;
    TopLevel *top_levels;
};

void translation_unit_free(TranslationUnit *unit);
void translation_unit_pprint(FILE *to, const TranslationUnit *unit);



// Cyclical include problems... again
#include "dependent-c/general.h"

/* Substitute a variable in an expression for an expression. */
void expr_subst(Context *context, Expr *expr,
    const char *name, const Expr *replacement);

/***** Specializations of printf *********************************************/

#define ewrap(...) \
    ((const void*[]){__VA_ARGS__})

#if __GNUC__
__attribute__((__format__ (__printf__, 2, 4)))
#endif

/* 'e' for extended.
 *
 * Prints a formatted string in the same manner as the printf family of
 * functions, except some additional escapes are permitted. These are:
 *
 * $$ - print '$'.
 * $e - print an expression.
 * $(e - print an expression, parenthesized if it is complex (eg `1 + 2` would
 *       be parenthesized, but `3` would not).
 *
 * Arguments to these additional escapes should be passed through an array
 * of pointers rather than directly as arguments. For example:
 *
 * Expr expr1 = expr_literal_type;
 * Expr expr2 = expr_literal_bool;
 * efprintf(stderr, "($e) does not equal ($e).\n", (void*[]){&expr1, &expr2});
 */
void efprintf(FILE *file, const char *format, const void *eargs[], ...);

#endif /* DEPENDENT_C_AST */
