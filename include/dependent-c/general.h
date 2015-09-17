#ifndef DEPENDENT_C_GENERAL_H
#define DEPENDENT_C_GENERAL_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

#include "dependent-c/ast_syntax.h"   /* No dependencies */
#include "dependent-c/lex.h"          /* No dependencies */
#include "dependent-c/symbol_table.h" /* ast_syntax */
#include "dependent-c/type.h"         /* ast_syntax */
#include "dependent-c/ast.h"          /* ast_syntax, symbol_table */

typedef struct Context Context;

/* A compilation context. */
struct Context {
    char *source_name;
    TokenStream tokens;
    InternedSymbols interns;
    SymbolTable symbol_table;
    TranslationUnit ast;
};

Context context_new(const char *source_name, CharStream source);
void context_free(Context *context);

#endif /* DEPENDENT_C_GENERAL_H */
