#ifndef DEPENDENT_C_AST
#define DEPENDENT_C_AST

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/***** Location Information **************************************************/
typedef struct {
    unsigned line;
    unsigned column;
} LocationInfo;

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
    , LIT_BOOL
    // Literal integers
    , LIT_INTEGRAL
    // Literal boolean values (true & false)
    , LIT_BOOLEAN
} LiteralTag;

typedef struct {
    LiteralTag tag;
    union {
        uint64_t integral;
        bool boolean;
    };
} Literal;

/***** Expressions ***********************************************************/
typedef enum {
      BIN_OP_EQ
    , BIN_OP_NE
    , BIN_OP_LT
    , BIN_OP_LTE
    , BIN_OP_GT
    , BIN_OP_GTE
    , BIN_OP_ADD
    , BIN_OP_SUB
} BinaryOp;

typedef enum {
    // Misc.
      EXPR_LITERAL
    , EXPR_IDENT
    , EXPR_BIN_OP

    // Function type and destructor. Constructor is a declaration, not an
    // expression.
    , EXPR_FUNC_TYPE
    , EXPR_CALL

    // Product/Union type, constructor, and destructor.
    , EXPR_STRUCT
    , EXPR_UNION
    , EXPR_PACK
    , EXPR_MEMBER

    // Pointer type, constructor, and destructor.
    , EXPR_POINTER
    , EXPR_REFERENCE
    , EXPR_DEREFERENCE
} ExprTag;

typedef struct Expr Expr;
struct Expr {
    LocationInfo location;

    ExprTag tag;
    union {
        Literal literal;
        const char *ident;
        struct {
            BinaryOp op;
            Expr *expr1;
            Expr *expr2;
        } bin_op;

        struct {
            Expr *ret_type;
            size_t num_params;
            Expr *param_types;
            const char **param_names; // Values may be NULL if params not named.
        } func_type;
        struct {
            Expr *func;
            size_t num_args;
            Expr *args;
        } call;

        struct {
            size_t num_fields;
            Expr *field_types;
            const char **field_names;
        } struct_;
        struct {
            size_t num_fields;
            Expr *field_types;
            const char **field_names;
        } union_;
        struct {
            Expr *type;
            size_t num_assigns;
            const char **field_names;
            Expr *assigns;
        } pack;
        struct {
            Expr *record;
            const char *field;
        } member;

        Expr *pointer;
        Expr *reference;
        Expr *dereference;
    };
};

#define literal_expr_type \
    ((Expr){ \
          .tag = EXPR_LITERAL \
        , .literal = (Literal){.tag = LIT_TYPE} \
    })

#define literal_expr_bool \
    ((Expr){ \
          .tag = EXPR_LITERAL \
        , .literal = (Literal){.tag = LIT_BOOL} \
    })

void expr_free(Expr *expr);
void expr_pprint(FILE *to, const Expr *expr);

/* Determine if two expressions are exactly equivalent. Does not take into
 * account alpha equivalence.
 */
bool expr_equal(const Expr *x, const Expr *y);

/* Make a deep copy of an expression. */
Expr expr_copy(const Expr *x);

// Cyclical include problems...
#include "dependent-c/symbol_table.h"

/* Calculate the set of free variables in an expression. */
void expr_free_vars(const Expr *expr, SymbolSet *set);


/***** Statements ************************************************************/
typedef enum {
      STATEMENT_EMPTY
    , STATEMENT_EXPR
    , STATEMENT_RETURN
    , STATEMENT_BLOCK
    , STATEMENT_DECL
    , STATEMENT_IFTHENELSE
} StatementTag;

typedef struct Statement Statement;

typedef struct {
    size_t num_statements;
    Statement *statements;
} Block;

struct Statement {
    LocationInfo location;

    StatementTag tag;
    union {
        Expr expr;

        Block block;

        struct {
            Expr type;
            const char *name;
            bool is_initialized;
            Expr initial_value;
        } decl;

        struct {
            size_t num_ifs;
            Expr *ifs;
            Block *thens;
            Block else_;
        } ifthenelse;
    };
};

void statement_free(Statement *statement);
void statement_pprint(FILE *to, int nesting, const Statement *statement);

void block_free(Block *block);

/***** Top-Level Definitions *************************************************/
typedef enum {
      TOP_LEVEL_FUNC
} TopLevelTag;

typedef struct {
    LocationInfo location;
    const char *name;

    TopLevelTag tag;
    union {
        struct {
            Expr ret_type;
            size_t num_params;
            Expr *param_types;
            const char **param_names;
            Block block;
        } func;
    };
} TopLevel;


void top_level_free(TopLevel *top_level);
void top_level_pprint(FILE *to, const TopLevel *top_level);

/***** Translation Units *****************************************************/

typedef struct {
    size_t num_top_levels;
    TopLevel *top_levels;
} TranslationUnit;

void translation_unit_free(TranslationUnit *unit);
void translation_unit_pprint(FILE *to, const TranslationUnit *unit);



// Cyclical include problems... again
#include "dependent-c/general.h"

/* Substitute a variable in an expression for an expression. */
bool expr_subst(Context *context, Expr *expr,
    const char *name, const Expr *replacement);

#endif /* DEPENDENT_C_AST */
