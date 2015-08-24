#ifndef DEPENDENT_C_GENERAL_H
#define DEPENDENT_C_GENERAL_H

#include "dependent-c/ast.h"
#include "dependent-c/lex.h"
#include "dependent-c/symbol_table.h"

/* A compilation context. */
typedef struct {
    char *source_name;
    TokenStream tokens;
    InternedSymbols interns;
    SymbolTable symbol_table;
    TranslationUnit ast;
} Context;

Context context_new(const char *source_name, CharStream source);
void context_free(Context *context);

#endif /* DEPENDENT_C_GENERAL_H */
