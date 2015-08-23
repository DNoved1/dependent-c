#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "dependent-c/ast.h"
#include "dependent-c/symbol_table.h"

SymbolTable symbol_table_new(void) {
    return (SymbolTable){
          .num_globals = 0
        , .global_names = NULL
        , .global_types = NULL

        , .locals_stack_size = 0
        , .locals_stack = NULL
    };
}

void symbol_table_free(SymbolTable *symbols) {
    free(symbols->global_names);
    free(symbols->global_types);

    for (size_t i = 0; i < symbols->locals_stack_size; i++) {
        free(symbols->locals_stack[i].local_names);
        free(symbols->locals_stack[i].local_types);
    }
    free(symbols->locals_stack);

    memset(symbols, 0, sizeof *symbols);
}

void symbol_table_enter_scope(SymbolTable *symbols) {
    symbols->locals_stack = realloc(symbols->locals_stack,
        (symbols->locals_stack_size + 1) * sizeof *symbols->locals_stack);
    symbols->locals_stack[symbols->locals_stack_size].num_locals = 0;
    symbols->locals_stack[symbols->locals_stack_size].local_names = NULL;
    symbols->locals_stack[symbols->locals_stack_size].local_types = NULL;
    symbols->locals_stack_size += 1;
}

void symbol_table_leave_scope(SymbolTable *symbols) {
    assert(symbols->locals_stack_size > 0);

    free(symbols->locals_stack[symbols->locals_stack_size - 1].local_names);
    free(symbols->locals_stack[symbols->locals_stack_size - 1].local_types);
    // Not reallocing to smaller size here since we'll likely reuse the the
    // space later.
}

bool symbol_table_register_global(SymbolTable *symbols,
        char *name, Expr type) {
    for (size_t i = 0; i < symbols->num_globals; i++) {
        if (strcmp(name, symbols->global_names[i]) == 0) {
            return false;
        }
    }

    symbols->global_names = realloc(symbols->global_names,
        (symbols->num_globals + 1) * sizeof *symbols->global_names);
    symbols->global_names[symbols->num_globals] = name;
    symbols->global_types = realloc(symbols->global_types,
        (symbols->num_globals + 1) * sizeof *symbols->global_types);
    symbols->global_types[symbols->num_globals] = type;

    symbols->num_globals += 1;
    return true;
}

bool symbol_table_register_local(SymbolTable *symbols,
        char *name, Expr type) {
    assert(symbols->locals_stack_size > 0);

    size_t index = symbols->locals_stack_size - 1;
    size_t num_locals = symbols->locals_stack[index].num_locals;

    for (size_t i = 0; i < num_locals; i++) {
        if (strcmp(name, symbols->locals_stack[index].local_names[i]) == 0) {
            return false;
        }
    }

    symbols->locals_stack[index].local_names = realloc(
        symbols->locals_stack[index].local_names,
        (num_locals + 1) * sizeof *symbols->locals_stack[index].local_names);
    symbols->locals_stack[index].local_names[num_locals] = name;
    symbols->locals_stack[index].local_types = realloc(
        symbols->locals_stack[index].local_types,
        (num_locals + 1) * sizeof *symbols->locals_stack[index].local_types);
    symbols->locals_stack[index].local_types[num_locals] = type;

    symbols->locals_stack[index].num_locals = num_locals + 1;
    return true;
}
