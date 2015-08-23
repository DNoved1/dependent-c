#ifndef DEPENDENT_C_SYMBOL_TABLE_H
#define DEPENDENT_C_SYMBOL_TABLE_H

#include <stddef.h>

#include "dependent-c/ast.h"

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
