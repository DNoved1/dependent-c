#ifndef DEPENDENT_C_TYPE_H
#define DEPENDENT_C_TYPE_H

#include <stdbool.h>

#include "dependent-c/ast.h"

/* Analyze the dependencies in the type signatures of top level declarations,
 * returning true and the order if successful. If there is a cyclical
 * dependency in the types a message is printed to stderr and false is
 * returned.
 */
bool top_level_topological_sort(size_t len, const TopLevel top_levels[len],
    size_t order[len]);

/* Check if an expression is a kind in System-F omega. Inductively the set K
 * of kinds is defined as:
 *     'type' is a member of K
 *     K(K, K, ...) is a member of K
 */
bool type_check_is_kind(Expr expr);

#endif /* DEPENDENT_C_TYPE_H */
