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

/***** Symbol Table (aka map from Symbol -> Type) ****************************/
typedef struct {
    size_t num_globals;
    char **global_names;
    Expr *global_types;

    size_t locals_stack_size;
    struct {
        size_t num_locals;
        char **local_names;
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
bool symbol_table_register_global(SymbolTable *symbols, char *name, Expr type);
bool symbol_table_register_local(SymbolTable *symbols, char *name, Expr type);

#endif /* DEPENDENT_C_SYMBOL_TABLE_H */
