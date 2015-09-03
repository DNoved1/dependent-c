#ifndef DEPENDENT_C_MEMORY_H
#define DEPENDENT_C_MEMORY_H

#ifdef NDEBUG

# define alloc(variable) \
    do { \
        variable = malloc(sizeof *variable); \
    } while (0)

# define alloc_array(variable, len) \
    do { \
        variable = malloc(sizeof *variable * len); \
    } while (0)

# define realloc_array(variable, new_len) \
    do { \
        variable = realloc(variable, sizeof *variable * len); \
    } while (0)

# define dealloc(variable) \
    do { \
        variable = free(variable), NULL; \
    } while (0)

#else /* NDEBUG */

# define alloc(variable) \
    do { \
        variable = _alloc(__FILE__, __LINE__, sizeof *variable); \
    } while (0)

# define alloc_array(variable, len) \
    do { \
        variable = _alloc_array(__FILE__, __LINE__, sizeof *variable, len); \
    } while (0)

# define realloc_array(variable, new_len) \
    do { \
        variable = _realloc_array(__FILE__, __LINE__, variable, \
            sizeof *variable, new_len); \
    } while (0)

# define dealloc(variable) \
    do { \
        variable = _dealloc(__FILE__, __LINE__, variable, sizeof *variable); \
    } while (0)

#endif /* NDEBUG */

void *_alloc(const char *file, int line, size_t size);
void *_alloc_array(const char *file, int line, size_t size, size_t len);
void *_realloc_array(const char *file, int line, void *array,
    size_t size, size_t len);
void *_dealloc(const char *file, int line, void *ptr, size_t size);

/* Useful for checking if memory has been leaked if you return to a point at
 * which all allocated memory should have been released. */
size_t amount_allocated(void);

/* Useful for identifying the sources of those leaks, or just in general to
 * see how much memory everything is using. */
void print_allocation_info(FILE *to);

#endif /* DEPENDENT_C_MEMORY_H */
