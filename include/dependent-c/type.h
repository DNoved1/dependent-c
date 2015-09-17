#ifndef DEPENDENT_C_TYPE_H
#define DEPENDENT_C_TYPE_H

#include <stdbool.h>

#include "dependent-c/ast.h"
#include "dependent-c/general.h"

bool type_check(Context*, const Expr *expr, const Expr *type);
bool type_infer(Context*, const Expr *expr, Expr *result);
bool type_equal(Context*, const Expr *type1, const Expr *type2);
bool type_eval(Context*, const Expr *type, Expr *result);

bool type_check_top_level(Context*, const TopLevel *top_level);

#endif /* DEPENDENT_C_TYPE_H */
