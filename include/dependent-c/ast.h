#ifndef DEPENDENT_C_AST
#define DEPENDENT_C_AST

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef struct Literal          Literal;
typedef struct Expr             Expr;
typedef struct Statement        Statement;
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

struct Literal {
    LiteralTag tag;
    union {
        uint64_t integral;
        bool boolean;
    };
};

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
    , BIN_OP_ANDTHEN
} BinaryOp;

typedef enum {
    // Misc.
      EXPR_LITERAL
    , EXPR_IDENT
    , EXPR_BIN_OP
    , EXPR_IFTHENELSE

    // Function type, constructor, and destructor.
    , EXPR_FUNC_TYPE
    , EXPR_LAMBDA
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

    // Statements viewed as expressions
    , EXPR_STATEMENT
} ExprTag;

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
            Expr *predicate;
            Expr *then_;
            Expr *else_;
        } ifthenelse;

        struct {
            Expr *ret_type;
            size_t num_params;
            Expr *param_types;
            const char **param_names; // Values may be NULL if params not named.
        } func_type;
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

        // Same field for reference and dereference
        Expr *pointer;

        Statement *statement;
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

#define literal_expr_void \
    ((Expr){ \
          .tag = EXPR_LITERAL \
        , .literal = (Literal){.tag = LIT_VOID} \
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

/***** Statements ************************************************************/
struct Block {
    size_t num_statements;
    Statement *statements;
};

typedef enum {
      STATEMENT_EMPTY
    , STATEMENT_EXPR
    , STATEMENT_RETURN
    , STATEMENT_BLOCK
    , STATEMENT_DECL
    , STATEMENT_IFTHENELSE
} StatementTag;

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
Statement statement_copy(const Statement *statement);

void statement_pprint(FILE *to, int nesting, const Statement *statement);
void statement_free_vars(const Statement *statement, SymbolSet *free_vars);

void block_free(Block *block);
Block block_copy(const Block *block);

// Note: does not print any braces
void block_pprint(FILE *to, int nesting, const Block *block);
void block_free_vars(const Block *block, SymbolSet *free_vars);

/***** Top-Level Definitions *************************************************/
typedef enum {
      TOP_LEVEL_FUNC
} TopLevelTag;

struct TopLevel {
    LocationInfo location;
    const char *name;

    TopLevelTag tag;
    union {
        struct {
            Expr ret_type;
            size_t num_params;
            Expr *param_types;
            const char **param_names;
            Expr body;
        } func;
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
bool expr_subst(Context *context, Expr *expr,
    const char *name, const Expr *replacement);

bool statement_subst(Context *context, Statement *statement,
    const char *name, const Expr *replacement);

bool block_subst(Context *context, Block *block,
    const char *name, const Expr *replacement);

#endif /* DEPENDENT_C_AST */
