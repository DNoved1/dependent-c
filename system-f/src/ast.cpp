#include "system-f-c/ast.h"
#include "system-f-c/util.h"

using std::ostream;
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
    unordered_set<string> operator()(const T& expr) const {
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

/***** Alpha Equality Testing ************************************************/
bool alpha_eq(Ident& ident1, const Ident& ident2) {
    return ident1.ident == ident2.ident;
}

bool alpha_eq(Type& type1, const Type& type2) {
    return true;
}

bool alpha_eq(Forall& forall1, const Forall& forall2) {
    if (forall1.params.size() != forall2.params.size()) {
        return false;
    }

    for (size_t i = 0; i < forall1.params.size(); i++) {
        MaybeNamedType& param1 = forall1.params[i];
        const MaybeNamedType& param2 = forall2.params[i];

        if (!alpha_eq(param1.type, param2.type)) {
            return false;
        }

        if (param1.name && param2.name
                && param1.name.get() != param2.name.get()) {
            Expr replacement = Ident(param2.name.get());

            param1.name = param2.name.get();
            for (size_t j = i + 1; j < forall1.params.size(); j++) {
                subst(forall1.params[j].type, param1.name.get(), replacement);
            }
            subst(forall1.return_type, param1.name.get(), replacement);
        }

    }

    return alpha_eq(forall1.return_type, forall2.return_type);
}

bool alpha_eq(Lambda& lambda1, const Lambda& lambda2) {
    if (lambda1.params.size() != lambda2.params.size()) {
        return false;
    }

    for (size_t i = 0; i < lambda1.params.size(); i++) {
        NamedType& param1 = lambda1.params[i];
        const NamedType& param2 = lambda2.params[i];

        if (!alpha_eq(param1.type, param2.type)) {
            return false;
        }

        if (param1.name != param2.name) {
            Expr replacement = Ident(param2.name);

            param1.name = param2.name;
            for (size_t j = i + 1; j < lambda1.params.size(); j++) {
                subst(lambda1.params[j].type, param1.name, replacement);
            }
            subst(lambda1.body, param1.name, replacement);
        }
    }

    return alpha_eq(lambda1.body, lambda2.body);
}

bool alpha_eq(Call& call1, const Call& call2) {
    if (call1.args.size() != call2.args.size()) {
        return false;
    }

    for (size_t i = 0; i < call1.args.size(); i++) {
        if (!alpha_eq(call1.args[i], call2.args[i])) {
            return false;
        }
    }

    return alpha_eq(call1.func, call2.func);
}

struct AlphaEqVisitor : public static_visitor<bool> {
    template <typename T, typename S>
    bool operator()(T& expr1, const S& expr2) const {
        return false;
    }

    template <typename T>
    bool operator()(T& expr1, const T& expr2) const {
        return alpha_eq(expr1, expr2);
    }
};

bool alpha_eq(Expr& expr1, const Expr& expr2) {
    return apply_visitor(AlphaEqVisitor(), expr1, expr2);
}

bool alpha_eq(const Expr& expr1, const Expr& expr2) {
    Expr expr1_copy = expr1;
    return alpha_eq(expr1_copy, expr2);
}

/***** Pretty-Printing *******************************************************/
ostream& operator<<(ostream& os, const Ident& ident) {
    return os << ident.ident;
}

ostream& operator<<(ostream& os, const Type& type) {
    return os << "Type";
}

ostream& operator<<(ostream& os, const Forall& forall) {
    os << '(';

    bool first = true;
    for (const MaybeNamedType& param : forall.params) {
        if (first) {
            first = false;
        } else {
            os << ", ";
        }

        if (param.name) {
            os << param.name.get() << " : ";
        }
        os << param.type;
    }

    return os << ") -> " << forall.return_type;
}

ostream& operator<<(ostream& os, const Lambda& lambda) {
    os << '(';

    bool first = true;
    for (const NamedType& param : lambda.params) {
        if (first) {
            first = false;
        } else {
            os << ", ";
        }

        os << param.name << " : " << param.type;
    }

    return os << ") -> "<< lambda.body;
}

ostream& operator<<(ostream& os, const Call& call) {
    os << call.func << '(';

    bool first = true;
    for (const Expr& arg : call.args) {
        if (first) {
            first = false;
        } else {
            os << ", ";
        }

        os << arg;
    }

    return os << ')';
}

// Note: using & as a return type causes errors in the current version of
//       boost.
struct PPrintVisitor /*: public static_visitor<ostream&>*/ {
    ostream& os;

    PPrintVisitor(ostream& os) : os(os) {}

    template <typename T>
    ostream& operator()(const T& expr) const {
        return os << expr;
    }
};

ostream& operator<<(ostream& os, const Expr& expr) {
    return apply_visitor(PPrintVisitor(os), expr);
}

} /* namespace ast::expr */

} /* namespace ast */
