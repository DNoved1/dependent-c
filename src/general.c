#include <string.h>

#include "dependent-c/ast.h"
#include "dependent-c/general.h"
#include "dependent-c/lex.h"
#include "dependent-c/memory.h"
#include "dependent-c/symbol_table.h"

Context context_new(const char *source_name, CharStream source) {
    char *source_name_copy;
    alloc_array(source_name_copy, strlen(source_name) + 1);
    strcpy(source_name_copy, source_name);

    TokenStream tokens = token_stream_new(source);

    return (Context){
          .source_name = source_name_copy
        , .tokens = tokens
        , .interns = symbol_new()
        , .symbol_table = symbol_table_new()
        , .ast = (TranslationUnit){0}
    };
}

void context_free(Context *context) {
    dealloc(context->source_name);
    token_stream_free(&context->tokens);
    symbol_free_all(&context->interns);
    symbol_table_free(&context->symbol_table);
    translation_unit_free(&context->ast);
    memset(context, 0, sizeof *context);
}
