#include <assert.h>
#include <string.h>

#include "dependent-c/ast.h"
#include "dependent-c/memory.h"
#include "dependent-c/symbol_table.h"

/***** Symbol Interning ******************************************************/
static size_t size_t_min(size_t x, size_t y) {
    return x < y ? x : y;
}

// Using the first few characters as the hash here since most symbols tend to
// be short. Might be worthwhile changing later since many other symbols will
// be longer with the same prefix, as exhibited by the code here.
static uint64_t symbol_hash(const char *str) {
    uint64_t result = 0;
    memcpy(&result, str, size_t_min(strlen(str), sizeof(uint64_t)));
    return result;
}

static void symbol_resize_if_needed(InternedSymbols *interns) {
    if ((interns->len + 1) * 2 >= interns->cap) {
        size_t old_cap = interns->cap;
        struct InternedSymbolsTable *old_symbols = interns->symbols;

        size_t new_cap = old_cap * 2 + 1;
        struct InternedSymbolsTable *new_symbols;
        alloc_array(new_symbols, new_cap);
        memset(new_symbols, 0, sizeof *new_symbols * new_cap);

        for (size_t i = 0; i < old_cap; i++) {
            if (old_symbols[i].symbol != NULL) {
                size_t index = old_symbols[i].hash % new_cap;

                while (true) {
                    if (new_symbols[index].symbol == NULL) {
                        new_symbols[index] = old_symbols[i];
                        break;
                    } else {
                        index = (index + 1) % new_cap;
                    }
                }
            }
        }

        interns->cap = new_cap;
        interns->symbols = new_symbols;

        memset(old_symbols, 0, old_cap * sizeof *old_symbols);
        dealloc(old_symbols);
    }
}

InternedSymbols symbol_new(void) {
    return (InternedSymbols){
          .len = 0
        , .cap = 0
        , .symbols = NULL
    };
}

void symbol_free_all(InternedSymbols *interns) {
    for (size_t i = 0; i < interns->cap; i++) {
        if (interns->symbols[i].symbol != NULL) {
            dealloc(interns->symbols[i].symbol);
            memset(&interns->symbols[i], 0, sizeof interns->symbols[i]);
        }
    }

    dealloc(interns->symbols);
    memset(interns, 0, sizeof *interns);
}

const char *symbol_intern(InternedSymbols *interns, const char *str) {
    symbol_resize_if_needed(interns);

    uint64_t hash = symbol_hash(str);
    size_t index = hash % interns->cap;

    while (true) {
        if (interns->symbols[index].symbol == NULL) {
            char *str_copy;
            alloc_array(str_copy, strlen(str) + 1);
            strcpy(str_copy, str);

            interns->symbols[index].hash = hash;
            interns->symbols[index].symbol = str_copy;
            interns->len += 1;
            return interns->symbols[index].symbol;
        } else if (hash == interns->symbols[index].hash
                && strcmp(str, interns->symbols[index].symbol) == 0) {
            return interns->symbols[index].symbol;
        } else {
            index = (index + 1) % interns->cap;
        }
    }
}

static const char *symbol_gensym_(InternedSymbols *interns, char *str) {
start_of_function:;
    uint64_t hash = symbol_hash(str);
    size_t index = hash % interns->cap;

    while (true) {
        if (interns->symbols[index].symbol == NULL) {
            const char *result = symbol_intern(interns, str);
            dealloc(str);
            return result;
        } else if (hash == interns->symbols[index].hash
                && strcmp(str, interns->symbols[index].symbol) == 0) {
            size_t len = strlen(str);
            realloc_array(str, len + 2);
            str[len] = '_';
            str[len + 1] = '\0';
            goto start_of_function;
        } else {
            index = (index + 1) % interns->cap;
        }
    }
}

const char *symbol_gensym(InternedSymbols *interns, const char *str) {
    char *str_copy;
    alloc_array(str_copy, strlen(str) + 1);
    strcpy(str_copy, str);
    return symbol_gensym_(interns, str_copy);
}

/***** Symbol Table **********************************************************/
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
    dealloc(symbols->global_names);
    dealloc(symbols->global_types);

    for (size_t i = 0; i < symbols->locals_stack_size; i++) {
        dealloc(symbols->locals_stack[i].local_names);
        dealloc(symbols->locals_stack[i].local_types);
    }
    dealloc(symbols->locals_stack);

    memset(symbols, 0, sizeof *symbols);
}

void symbol_table_enter_scope(SymbolTable *symbols) {
    realloc_array(symbols->locals_stack, symbols->locals_stack_size + 1);
    symbols->locals_stack[symbols->locals_stack_size].num_locals = 0;
    symbols->locals_stack[symbols->locals_stack_size].local_names = NULL;
    symbols->locals_stack[symbols->locals_stack_size].local_types = NULL;
    symbols->locals_stack_size += 1;
}

void symbol_table_leave_scope(SymbolTable *symbols) {
    assert(symbols->locals_stack_size > 0);

    dealloc(symbols->locals_stack[symbols->locals_stack_size - 1].local_names);
    dealloc(symbols->locals_stack[symbols->locals_stack_size - 1].local_types);
    symbols->locals_stack_size -= 1;
    // Not reallocing to smaller size here since we'll likely reuse the the
    // space later.
}

bool symbol_table_register_global(SymbolTable *symbols,
        const char *name, Expr type) {
    for (size_t i = 0; i < symbols->num_globals; i++) {
        if (strcmp(name, symbols->global_names[i]) == 0) {
            return false;
        }
    }

    realloc_array(symbols->global_names, symbols->num_globals + 1);
    symbols->global_names[symbols->num_globals] = name;
    realloc_array(symbols->global_types, symbols->num_globals + 1);
    symbols->global_types[symbols->num_globals] = type;

    symbols->num_globals += 1;
    return true;
}

bool symbol_table_register_local(SymbolTable *symbols,
        const char *name, Expr type) {
    assert(symbols->locals_stack_size > 0);

    size_t index = symbols->locals_stack_size - 1;
    size_t num_locals = symbols->locals_stack[index].num_locals;

    for (size_t i = 0; i < num_locals; i++) {
        if (strcmp(name, symbols->locals_stack[index].local_names[i]) == 0) {
            return false;
        }
    }

    realloc_array(symbols->locals_stack[index].local_names, num_locals + 1);
    symbols->locals_stack[index].local_names[num_locals] = name;
    realloc_array(symbols->locals_stack[index].local_types, num_locals + 1);
    symbols->locals_stack[index].local_types[num_locals] = type;

    symbols->locals_stack[index].num_locals = num_locals + 1;
    return true;
}

bool symbol_table_lookup(SymbolTable *symbols,
        const char *name, Expr *result) {
    for (size_t i_ = 0; i_ < symbols->locals_stack_size; i_++) {
        size_t i = symbols->locals_stack_size - i_ - 1;

        for (size_t j = 0; j < symbols->locals_stack[i].num_locals; j++) {
            if (strcmp(name, symbols->locals_stack[i].local_names[j]) == 0) {
                *result = symbols->locals_stack[i].local_types[j];
                return true;
            }
        }
    }

    for (size_t i = 0; i < symbols->num_globals; i++) {
        if (strcmp(name, symbols->global_names[i]) == 0) {
            *result = symbols->global_types[i];
            return true;
        }
    }

    return false;
}

/***** Symbol Sets ***********************************************************/
SymbolSet symbol_set_empty(void) {
    return (SymbolSet){
          .size = 0
        , .symbols = NULL
    };
}

void symbol_set_free(SymbolSet *set) {
    dealloc(set->symbols);
    memset(set, 0, sizeof *set);
}

/* Remove a symbol at an index from a symbol set. */
static void symbol_set_remove(SymbolSet *set, size_t index) {
    assert(set->size > index);

    memmove(&set->symbols[index], &set->symbols[index + 1],
        (set->size - index - 1) * sizeof set->symbols[0]);
    realloc_array(set->symbols, set->size - 1);
    set->size -= 1;
}

void symbol_set_delete(SymbolSet *set, const char *symbol) {
    for (size_t i = 0; i < set->size; i++) {
        if (symbol == set->symbols[i]) {
            symbol_set_remove(set, i);
            break;
        }
    }
}

void symbol_set_add(SymbolSet *set, const char *symbol) {
    if (!symbol_set_contains(set, symbol)) {
        realloc_array(set->symbols, set->size + 1);
        set->symbols[set->size] = symbol;
        set->size += 1;
    }
}

bool symbol_set_contains(const SymbolSet *set, const char *symbol) {
    for (size_t i = 0; i < set->size; i++) {
        if (set->symbols[i] == symbol) {
            return true;
        }
    }

    return false;
}

void symbol_set_union(SymbolSet *set1, SymbolSet *set2) {
    for (size_t i = 0; i < set2->size; i++) {
        symbol_set_add(set1, set2->symbols[i]);
    }

    symbol_set_free(set2);
}
