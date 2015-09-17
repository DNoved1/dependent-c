#ifndef DEPENDENT_C_TYPE_H
#define DEPENDENT_C_TYPE_H

struct Context;

bool type_check(struct Context*, const Expr *expr, const Expr *type);
bool type_infer(struct Context*, const Expr *expr, Expr *result);
bool type_equal(struct Context*, const Expr *type1, const Expr *type2);
bool type_eval(struct Context*, const Expr *type, Expr *result);

bool type_check_top_level(struct Context*, const TopLevel *top_level);

#endif /* DEPENDENT_C_TYPE_H */
