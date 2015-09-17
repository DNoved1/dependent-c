#ifndef DEPENDENT_C_AST
#define DEPENDENT_C_AST

struct Context;

/***** Location Information **************************************************/

/* For printing error messages. */
void location_pprint(struct Context*, const char *file, const LocationInfo *info);

/***** Expressions ***********************************************************/
void expr_free(struct Context*, Expr *expr);
Expr expr_copy(struct Context*, const Expr *x);

void expr_pprint(struct Context*, FILE *to, const Expr *expr);

/* Determine if two expressions are exactly equivalent. Does not take into
 * account alpha equivalence.
 */
bool expr_equal(struct Context*, const Expr *x, const Expr *y);

/* Calculate the set of free variables in an expression. */
void expr_free_vars(struct Context*, const Expr *expr, SymbolSet *set);

/***** Top-Level Definitions *************************************************/
void top_level_free(struct Context*, TopLevel *top_level);
void top_level_pprint(struct Context*, FILE *to, const TopLevel *top_level);

/***** Translation Units *****************************************************/
void translation_unit_free(struct Context*, TranslationUnit *unit);
void translation_unit_pprint(struct Context*, FILE *to,
    const TranslationUnit *unit);

/* Substitute a variable in an expression for an expression. */
void expr_subst(struct Context*, Expr *expr,
    const char *name, const Expr *replacement);

/***** Specializations of printf *********************************************/

#define ewrap(...) \
    ((const void*[]){__VA_ARGS__})

#if __GNUC__
__attribute__((__format__ (__printf__, 3, 5)))
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
void efprintf(struct Context*, FILE *file,
    const char *format, const void *eargs[], ...);

#endif /* DEPENDENT_C_AST */
