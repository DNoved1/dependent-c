#ifndef DEPENDENT_C_SYMBOL_TABLE_H
#define DEPENDENT_C_SYMBOL_TABLE_H

#include <stddef.h>

#include "dependent-c/ast.h"

/***** Symbol Interning ******************************************************/

typedef struct {
    size_t len;
    size_t cap;
    struct InternedSymbolsTable {
        uint64_t hash;
        char *symbol; // Is NULL if the spot is unoccupied
    } *symbols;
} InternedSymbols;

InternedSymbols symbol_new(void);
void symbol_free_all(InternedSymbols *interns);

const char *symbol_intern(InternedSymbols *interns, const char *str);

/* Returns a fresh symbol based upon the given string. */
const char *symbol_gensym(InternedSymbols *interns, const char *str);

/***** Symbol Table (aka map from Symbol -> Type) ****************************/
typedef struct {
    size_t num_globals;
    const char **global_names;
    Expr *global_types;

    size_t locals_stack_size;
    struct {
        size_t num_locals;
        const char **local_names;
        Expr *local_types;
    } *locals_stack;
} SymbolTable;

SymbolTable symbol_table_new(void);
void symbol_table_free(SymbolTable *symbols);

/* Enter and leave local scopes. */
void symbol_table_enter_scope(SymbolTable *symbols);
void symbol_table_leave_scope(SymbolTable *symbols);

/* Attempt to register a symbol and its type. If the symbol is already defined
 * in the current scope it is not registered and false is returned.
 */
bool symbol_table_register_global(SymbolTable *symbols,
    const char *name, Expr type);
bool symbol_table_register_local(SymbolTable *symbols,
    const char *name, Expr type);

/* Lookup a symbol's type. Returns false if the symbol is not in scope. */
bool symbol_table_lookup(SymbolTable *symbols,
    const char *name, Expr *result);

/***** Symbol Sets ***********************************************************/

/* Remove a symbol from a symbol set if present. */
void symbol_set_delete(size_t *set_size, const char ***set,
    const char *delete);

/* Add a symbol to a symbol set if not present. */
void symbol_set_add(size_t *set_size, const char ***set,
    const char *add);

/* Checks if a symbol is in the symbol set. */
bool symbol_set_contains(size_t *set_size, const char ***set,
    const char *symbol);

/* Checks if the intersection of two sets is non-empty. */
bool symbol_set_contains_any(size_t *set1_size, const char ***set1,
    size_t *set2_size, const char ***set2);

/* Union two sets. The result is placed into the first set and the second set
 * is freed.
 */
void symbol_set_union(size_t *set1_size, const char ***set1,
    size_t *set2_size, const char ***set2);

#endif /* DEPENDENT_C_SYMBOL_TABLE_H */
