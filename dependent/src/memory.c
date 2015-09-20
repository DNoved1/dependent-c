#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dependent-c/memory.h"

typedef struct {
    size_t size;
    size_t len;
    const char *file;
    int line;
    bool mark;
    double _[]; // Using double so that user data is aligned for the worst case.
} MemInfo;

size_t allocated_len;
MemInfo **allocated_ptrs;

static void register_ptr(const char *file, int line, MemInfo *ptr) {
    MemInfo **new_allocated_ptrs = realloc(allocated_ptrs,
        (allocated_len + 1) * sizeof *allocated_ptrs);

    if (new_allocated_ptrs == NULL) {
        fprintf(stderr, "\n"
            "****************************************\n"
            "Warning: Could not register newly allocated pointer due to"
                " to malloc returning NULL.\n"
            "    Resulting from allocation at file %s line %d.\n"
            "****************************************\n"
            "\n", file, line);
        return;
    }

    allocated_ptrs = new_allocated_ptrs;
    allocated_ptrs[allocated_len] = ptr;
    allocated_len += 1;
}

static void unregister_ptr(const char *file, int line, MemInfo *ptr) {
    for (size_t i = 0; i < allocated_len; i++) {
        if (ptr == allocated_ptrs[i]) {
            memmove(&allocated_ptrs[i], &allocated_ptrs[i + 1],
                (allocated_len - i - 1) * sizeof *allocated_ptrs);
            MemInfo **new_allocated_ptrs = realloc(allocated_ptrs,
                (allocated_len - 1) * sizeof *allocated_ptrs);

            if (new_allocated_ptrs == NULL) {
                // Do nothing, since we're decreasing the size it is not
                // problem just keeping the old allocation.
            } else {
                allocated_ptrs = new_allocated_ptrs;
            }

            allocated_len -= 1;
            return;
        }
    }

    fprintf(stderr, "\n"
        "****************************************\n"
        "Warning: Attempted to deregister pointer which"
            " was not registered.\n"
        "    Resulting from deallocation at file %s line %d.\n"
        "****************************************\n"
        "\n", file, line);
}

static void update_ptr(const char *file, int line, MemInfo *old, MemInfo *new) {
    for (size_t i = 0; i < allocated_len; i++) {
        if (old == allocated_ptrs[i]) {
            allocated_ptrs[i] = new;
            return;
        }
    }

    fprintf(stderr, "\n"
        "****************************************\n"
        "Warning: Attempted to update point which was not registered.\n"
        "    Resulting from reallocation at file %s line %d.\n"
        "****************************************\n"
        "\n", file, line);
}

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
    register_ptr(file, line, info);
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
            "\n", len, size, overall_size, file, line);
        exit(EXIT_FAILURE);
    }

    info->size = size;
    info->len = len;
    info->file = file;
    info->line = line;

    memset(result, 0, overall_size);
    register_ptr(file, line, info);
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
            "\n", old_info->len, old_info->size, len, size,
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
            "\n", old_info->len, old_info->size, old_overall_size,
                len, size, new_overall_size,
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
    update_ptr(file, line, old_info, info);
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
                "\n", info->len, info->size, info->len, size,
                info->file, info->line, file, line);
        }

        exit(EXIT_FAILURE);
    }

    size_t overall_size = info->size * info->len;
    memset(info, 0, sizeof(MemInfo) + overall_size);
    free(info);
    unregister_ptr(file, line, info);
    return NULL;
}

size_t amount_allocated(void) {
    size_t count = 0;

    for (size_t i = 0; i < allocated_len; i++) {
        count += allocated_ptrs[i]->size * allocated_ptrs[i]->len;
    }

    return count;
}

void print_allocation_info(FILE *to) {
    size_t total_alloc = 0;

    for (size_t i = 0; i < allocated_len; i++) {
        allocated_ptrs[i]->mark = false;
    }

    while (true) {
        const char *file = NULL;
        size_t total_file_alloc = 0;

        for (size_t i = 0; i < allocated_len; i++) {
            if (!allocated_ptrs[i]->mark) {
                file = allocated_ptrs[i]->file;
                break;
            }
        }

        if (file == NULL) {
            break;
        }

        fprintf(to, "File \"%s\"\n", file);

        while (true) {
            int lowest_line = INT_MAX;
            size_t total_line_alloc = 0;

            for (size_t i = 0; i < allocated_len; i++) {
                if (allocated_ptrs[i]->file == file
                        && !allocated_ptrs[i]->mark
                        && allocated_ptrs[i]->line < lowest_line) {
                    lowest_line = allocated_ptrs[i]->line;
                }
            }

            if (lowest_line == INT_MAX) {
                break;
            }

            fprintf(to, "    Line %5d\n", lowest_line);

            for (size_t i = 0; i < allocated_len; i++) {
                if (allocated_ptrs[i]->file == file
                        && allocated_ptrs[i]->line == lowest_line) {
                    MemInfo *info = allocated_ptrs[i];

                    if (info->len == 1) {
                        fprintf(to, "        %zu bytes.\n", info->size);
                    } else {
                        fprintf(to, "        %zu * %zu = %zu bytes.\n",
                            info->len, info->size, info->size * info->len);
                    }

                    total_line_alloc += info->size * info->len;
                    info->mark = true;
                }
            }

            fprintf(to, "        Line total = %zu bytes.\n", total_line_alloc);
            total_file_alloc += total_line_alloc;
        }

        fprintf(to, "    File total = %zu bytes.\n", total_file_alloc);
        total_alloc += total_file_alloc;
    }

    fprintf(to, "Total allocation = %zu bytes.\n", total_alloc);
}
