#include "system-f-c/ast.h"
#include "system-f-c/util.h"

using std::string;
using std::unordered_set;

using boost::static_visitor;
using boost::apply_visitor;

using util::set_contains;
using util::set_union;

namespace ast {

namespace expr {

/***** Free Variable Set *****************************************************/
unordered_set<string> free_vars(const Ident& ident) {
    return unordered_set<string>({ident.ident});
}

unordered_set<string> free_vars(const Type& type) {
    return unordered_set<string>();
}

unordered_set<string> free_vars(const Forall& forall) {
    unordered_set<string> free;

    for (size_t i = 0; i < forall.params.size(); i++) {
        unordered_set<string> param_type_free = free_vars(forall.params[i].type);

        for (size_t j = 0; j < i; j++) {
            if (forall.params[j].name) {
                param_type_free.erase(forall.params[j].name.get());
            }
        }

        set_union(free, param_type_free);
    }

    unordered_set<string> return_type_free = free_vars(forall.return_type);
    for (MaybeNamedType param : forall.params) {
        if (param.name) {
            return_type_free.erase(param.name.get());
        }
    }
    set_union(free, return_type_free);

    return free;
}

unordered_set<string> free_vars(const Lambda& lambda) {
    unordered_set<string> free;

    for (size_t i = 0; i < lambda.params.size(); i++) {
        unordered_set<string> param_type_free = free_vars(lambda.params[i].type);

        for (size_t j = 0; j < i; j++) {
            param_type_free.erase(lambda.params[j].name);
        }

        set_union(free, param_type_free);
    }

    unordered_set<string> body_free = free_vars(lambda.body);
    for (NamedType param : lambda.params) {
        body_free.erase(param.name);
    }
    set_union(free, body_free);

    return free;
}

unordered_set<string> free_vars(const Call& call) {
    unordered_set<string> free = free_vars(call.func);

    for (Expr arg : call.args) {
        set_union(free, free_vars(arg));
    }

    return free;
}

struct FreeVarsVisitor : public static_visitor<unordered_set<string>> {
    template <typename T>
    unordered_set<string> operator()(T& expr) const {
        return free_vars(expr);
    }
};

unordered_set<string> free_vars(const Expr& expr) {
    return apply_visitor(FreeVarsVisitor(), expr);
}

/***** Substitution **********************************************************/
void subst(Type& type, const string& name, const Expr& with) {
    ;
}

void subst(Forall& forall, const string& name, const Expr& with) {
    unordered_set<string> with_free_vars = free_vars(with);

    for (size_t i = 0; i < forall.params.size(); i++) {
        subst(forall.params[i].type, name, with);

        if (forall.params[i].name) {
            string old_param_name = forall.params[i].name.get();

            if (old_param_name == name) {
                break;
            }

            if (set_contains(with_free_vars, old_param_name)) {
                string new_param_name; // TODO
                Expr replacement = Ident(new_param_name);

                forall.params[i].name = new_param_name;
                for (size_t j = i + 1; j < forall.params.size(); j++) {
                    subst(forall.params[j].type, old_param_name, replacement);
                }
                subst(forall.return_type, old_param_name, replacement);
            }
        }
    }

    subst(forall.return_type, name, with);
}

void subst(Lambda& lambda, const string& name, const Expr& with) {
    unordered_set<string> with_free_vars = free_vars(with);

    for (size_t i = 0; i < lambda.params.size(); i++) {
        subst(lambda.params[i].type, name, with);

        string old_param_name = lambda.params[i].name;

        if (old_param_name == name) {
            break;
        }

        if (set_contains(with_free_vars, old_param_name)) {
            string new_param_name; // TODO
            Expr replacement = Ident(new_param_name);

            lambda.params[i].name = new_param_name;
            for (size_t j = i + 1; j < lambda.params.size(); j++) {
                subst(lambda.params[j].type, old_param_name, replacement);
            }
            subst(lambda.body, old_param_name, replacement);
        }
    }

    subst(lambda.body, name, with);
}

void subst(Call& call, const string& name, const Expr& with) {
    subst(call.func, name, with);

    for (Expr& arg : call.args) {
        subst(arg, name, with);
    }
}

struct SubstVisitor : public static_visitor<void> {
    Expr& self;
    const string& name;
    const Expr& with;

    SubstVisitor(Expr &self, const string& name, const Expr& with)
        : self(self), name(name), with(with) {}

    template <typename T>
    void operator()(T& expr) const {
        subst(expr, name, with);
    }

};

template <>
void SubstVisitor::operator()<Ident>(Ident& ident) const {
    if (ident.ident == name) {
        self = with;
    }
}

void subst(Expr& expr, const string& name, const Expr& with) {
    apply_visitor(SubstVisitor(expr, name, with), expr);
}

} /* namespace ast::expr */

} /* namespace ast */
