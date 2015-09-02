#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dependent-c/memory.h"

typedef struct {
    size_t size;
    size_t len;
    const char *file;
    int line;
    double _[]; // Using double so that user data is aligned for the worst case.
} MemInfo;

void *_alloc(const char *file, int line, size_t size) {
    if (size == 0) {
        return NULL;
    }

    MemInfo *info = malloc(sizeof(MemInfo) + size);
    void *result = info + 1;

    if (info == NULL) {
        fprintf(stderr, "\n"
            "****************************************\n"
            "Error: Failed to allocate %zu bytes.\n"
            "    At file %s line %d.\n"
            "****************************************\n"
            "\n", size, file, line);
        exit(EXIT_FAILURE);
    }

    info->size = size;
    info->len = 1;
    info->file = file;
    info->line = line;

    memset(result, 0, size);
    return result;
}

void *_alloc_array(const char *file, int line, size_t size, size_t len) {
    size_t overall_size = size * len;
    if (overall_size == 0) {
        return NULL;
    }

    MemInfo *info = malloc(sizeof(MemInfo) + overall_size);
    void *result = info + 1;

    if (info == NULL) {
        fprintf(stderr, "\n"
            "****************************************\n"
            "Error: Failed to allocate array of %zu * %zu = %zu bytes.\n"
            "    At file %s line %d.\n"
            "****************************************\n"
            "\n", size, len, overall_size, file, line);
        exit(EXIT_FAILURE);
    }

    info->size = size;
    info->len = len;
    info->file = file;
    info->line = line;

    memset(result, 0, overall_size);
    return result;
}

void *_realloc_array(const char *file, int line, void *array,
        size_t size, size_t len) {
    if (array == NULL) {
        return _alloc_array(file, line, size, len);
    }

    size_t new_overall_size = size * len;
    if (new_overall_size == 0) {
        return _dealloc(file, line, array, size);
    }

    MemInfo *old_info = (MemInfo*)array - 1;
    size_t old_overall_size = old_info->size * old_info->len;

    if (old_info->size != size) {
        fprintf(stderr, "\n"
            "****************************************\n"
            "Error: Reallocating array of %zu * %zu"
                " into one of %zu * %zu bytes.\n"
            "    Originally allocated at file %s line %d.\n"
            "    Reallocated at file %s line %d.\n"
            "****************************************\n"
            "\n", old_info->size, old_info->len, size, len,
                old_info->file, old_info->line, file, line);
        exit(EXIT_FAILURE);
    }

    MemInfo *info = realloc(old_info, sizeof(MemInfo) + new_overall_size);
    void *result = info + 1;

    if (info == NULL) {
        fprintf(stderr, "\n"
            "****************************************\n"
            "Error: Failed to reallocate array of %zu * %zu = %zu"
                " into one of %zu * %zu = %zu bytes.\n"
            "    Originally allocated at file %s line %d.\n"
            "    Reallocated at file %s line %d.\n"
            "****************************************\n"
            "\n", old_info->size, old_info->len, old_overall_size,
                size, len, new_overall_size,
                old_info->file, old_info->line, file, line);
        exit(EXIT_FAILURE);
    }

    info->size = size;
    info->len = len;
    info->file = file;
    info->line = line;

    if (new_overall_size > old_overall_size) {
        memset((char*)result + old_overall_size, 0,
            new_overall_size - old_overall_size);
    }
    return result;
}

void *_dealloc(const char *file, int line, void *ptr, size_t size) {
    if (ptr == NULL) {
        return NULL;
    }

    MemInfo *info = (MemInfo*)ptr - 1;

    if (info->size != size) {
        if (info->len == 1) {
            fprintf(stderr, "\n"
                "****************************************\n"
                "Error: Deallocating pointer to %zu bytes"
                    " as if it were %zu bytes.\n"
                "    Originally allocated at file %s line %d.\n"
                "    Deallocated at file %s line %d.\n"
                "****************************************\n"
                "\n", info->size, size, info->file, info->line, file, line);
        } else {
            fprintf(stderr, "\n"
                "****************************************\n"
                "Error: Deallocating array of %zu * %zu bytes"
                    " as if it were %zu * %zu bytes.\n"
                "    Originally allocated at file %s line %d.\n"
                "    Deallocated at file %s line %d.\n"
                "****************************************\n"
                "\n", info->size, info->len, size, info->len,
                info->file, info->line, file, line);
        }

        exit(EXIT_FAILURE);
    }

    size_t overall_size = info->size * info->len;
    memset(info, 0, sizeof(MemInfo) + overall_size);
    free(info);
    return NULL;
}
