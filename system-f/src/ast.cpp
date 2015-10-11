#include "system-f-c/ast.h"
#include "system-f-c/context.h"
#include "system-f-c/util.h"

#include <boost/variant/get.hpp>

using std::ostream;
using std::string;
using std::unordered_set;
using std::vector;

using boost::apply_visitor;
using boost::strict_get;
using boost::optional;
using boost::static_visitor;

using context::Context;
using context::ContextMarker;
using util::gensym;
using util::set_contains;
using util::set_union;

namespace ast {

namespace expr {

/***** Check an Expression's Constructor *************************************/
template <typename DesiredType>
struct ExprTypeCheckVisitor : public static_visitor<bool> {
    bool operator()(const DesiredType& expr) const {
        return true;
    }

    template <typename OtherType>
    bool operator()(const OtherType& expr) const {
        return false;
    }
};

bool is_ident(const Expr& expr) {
    return apply_visitor(ExprTypeCheckVisitor<Ident>(), expr);
}

bool is_type(const Expr& expr) {
    return apply_visitor(ExprTypeCheckVisitor<Type>(), expr);
}

bool is_forall(const Expr& expr) {
    return apply_visitor(ExprTypeCheckVisitor<Forall>(), expr);
}

bool is_lambda(const Expr& expr) {
    return apply_visitor(ExprTypeCheckVisitor<Lambda>(), expr);
}

bool is_call(const Expr& expr) {
    return apply_visitor(ExprTypeCheckVisitor<Call>(), expr);
}

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
                string new_param_name = gensym(old_param_name, with_free_vars);
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
            string new_param_name = gensym(old_param_name, with_free_vars);
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

/***** Sort Checking *********************************************************/
bool sort_infer(Context& context, const Ident& ident) {
    return false;
}

bool sort_infer(Context& context, const Type& type) {
    return true;
}

bool sort_infer(Context& context, const Forall& forall) {
    for (const MaybeNamedType& param : forall.params) {
        if (!sort_infer(context, param.type)) {
            return false;
        }
    }

    return sort_infer(context, forall.return_type);
}

bool sort_infer(Context& context, const Lambda& lambda) {
    return false;
}

bool sort_infer(Context& context, const Call& call) {
    return false;
}

struct SortInferVisitor : public static_visitor<bool> {
    Context& context;

    SortInferVisitor(Context& context) : context(context) {}

    template <typename T>
    bool operator()(const T& expr) const {
        return sort_infer(context, expr);
    }
};

bool sort_infer(Context& context, const Expr& expr) {
    return apply_visitor(SortInferVisitor(context), expr);
}

/***** Kind Checking *********************************************************/
optional<Expr> kind_infer(Context& context, const Ident& ident) {
    return context.lookup_type(ident.ident);
}

optional<Expr> kind_infer(Context& context, const Type& type) {
    return optional<Expr>();
}

optional<Expr> kind_infer(Context& context, const Forall& forall) {
    ContextMarker mark = context.mark();

    for (const MaybeNamedType& param : forall.params) {
        if (sort_infer(context, param.type)) {
            if (param.name) {
                context.register_type(param.name.get(), param.type);
            }
            continue;
        }

        if (kind_infer(context, param.type)) {
            continue;
        }

        context.reset(mark);
        return optional<Expr>();
    }

    bool valid = (bool)kind_infer(context, forall.return_type);
    context.reset(mark);
    if (valid) {
        return optional<Expr>(Type());
    } else {
        return optional<Expr>();
    }
}

optional<Expr> kind_infer(Context& context, const Lambda& lambda) {
    ContextMarker mark = context.mark();

    for (const NamedType& param : lambda.params) {
        if (sort_infer(context, param.type)) {
            context.register_type(param.name, param.type);
            continue;
        }

        context.reset(mark);
        return optional<Expr>();
    }

    optional<Expr> return_kind = kind_infer(context, lambda.body);
    context.reset(mark);
    if (!return_kind) {
        return optional<Expr>();
    }

    vector<MaybeNamedType> kind_params;
    for (const NamedType& param : lambda.params) {
        kind_params.push_back(MaybeNamedType(param.type));
    }

    return optional<Expr>(Forall(kind_params, return_kind.get()));
}

optional<Expr> kind_infer(Context& context, const Call& call) {
    optional<Expr> func_kind_ = kind_infer(context, call.func);
    if (!func_kind_ || !is_forall(func_kind_.get())) {
        return optional<Expr>();
    }

    Forall& func_kind = strict_get<Forall>(func_kind_.get());

    if (func_kind.params.size() != call.args.size()) {
        return optional<Expr>();
    }

    for (size_t i = 0; i < func_kind.params.size(); i++) {
        optional<Expr> arg_kind = kind_infer(context, call.args[i]);
        if (!arg_kind || !kind_equal(context,
                func_kind.params[i].type, arg_kind.get())) {
            return optional<Expr>();
        }
    }

    return optional<Expr>(func_kind.return_type);
}

struct KindInferVisitor : public static_visitor<optional<Expr>> {
    Context& context;

    KindInferVisitor(Context& context) : context(context) {}

    template <typename T>
    optional<Expr> operator()(const T& expr) const {
        return kind_infer(context, expr);
    }
};

optional<Expr> kind_infer(Context& context, const Expr& expr) {
    return apply_visitor(KindInferVisitor(context), expr);
}

/***** Type Checking *********************************************************/
optional<Expr> type_infer(Context& context, const Ident& ident) {
    return context.lookup_term(ident.ident);
}

optional<Expr> type_infer(Context& context, const Type& type) {
    return optional<Expr>();
}

optional<Expr> type_infer(Context& context, const Forall& forall) {
    return optional<Expr>();
}

optional<Expr> type_infer(Context& context, const Lambda& lambda) {
    ContextMarker mark = context.mark();

    for (const NamedType& param : lambda.params) {
        if (sort_infer(context, param.type)) {
            context.register_type(param.name, param.type);
            continue;
        }

        if (kind_infer(context, param.type)) {
            context.register_term(param.name, param.type);
            continue;
        }

        context.reset(mark);
        return optional<Expr>();
    }

    optional<Expr> body_type = type_infer(context, lambda.body);
    context.reset(mark);
    if (!body_type) {
        return optional<Expr>();
    }

    vector<MaybeNamedType> type_kind_params;
    for (const NamedType& param : lambda.params) {
        // TODO: We only need to put the name if it's a kind.
        type_kind_params.push_back(MaybeNamedType(param.name, param.type));
    }

    return optional<Expr>(Forall(type_kind_params, body_type.get()));
}

optional<Expr> type_infer(Context& context, const Call& call) {
    optional<Expr> func_type_ = type_infer(context, call.func);
    if (!func_type_ || !is_forall(func_type_.get())) {
        return optional<Expr>();
    }

    Forall& func_type = strict_get<Forall>(func_type_.get());

    if (func_type.params.size() != call.args.size()) {
        return optional<Expr>();
    }

    Expr return_type = func_type.return_type;
    for (size_t i = 0; i < func_type.params.size(); i++) {
        optional<Expr> arg_kind = kind_infer(context, call.args[i]);
        if (arg_kind) {
            if (kind_equal(context, func_type.params[i].type, arg_kind.get())) {
                if (func_type.params[i].name) {
                    subst(return_type, func_type.params[i].name.get(),
                        call.args[i]);
                }
                continue;
            } else {
                return optional<Expr>();
            }
        }

        optional<Expr> arg_type = type_infer(context, call.args[i]);
        if (arg_type) {
            if (type_equal(context, func_type.params[i].type, arg_type.get())) {
                continue;
            } else {
                return optional<Expr>();
            }
        }

        return optional<Expr>();
    }

    return optional<Expr>(return_type);
}

struct TypeInferVisitor : public static_visitor<optional<Expr>> {
    Context& context;

    TypeInferVisitor(Context& context) : context(context) {}

    template <typename T>
    optional<Expr> operator()(const T& expr) const {
        return type_infer(context, expr);
    }
};

optional<Expr> type_infer(Context& context, const Expr& expr) {
    return apply_visitor(TypeInferVisitor(context), expr);
}

/***** Type Equality *********************************************************/
bool type_equal(Context& context, const Ident& type1, Ident& type2) {
    return type1.ident == type2.ident;
}

bool type_equal(Context& context, const Type& type1, Type& type2) {
    assert(!"'Type' is not a valid type.");
}

bool type_equal(Context& context, const Forall& type1, Forall& type2) {
    if (type1.params.size() != type2.params.size()) return false;

    for (size_t i = 0; i < type1.params.size(); i++) {
        auto param1 = type1.params[i];
        auto param2 = type2.params[i];

        bool param1_kind = sort_infer(context, param1.type);
        bool param2_kind = sort_infer(context, param2.type);
        if (param1_kind != param2_kind) return false;

        if (param1_kind) {
            if (!kind_equal(context, param1.type, param2.type)) return false;
        } else {
            if (!type_equal(context, param1.type, param2.type)) return false;
        }

        if (param1.name && param2.name
                && param1.name.get() != param2.name.get()) {
            Expr replacement = Ident(param1.name.get());

            param2.name = param1.name.get();
            for (size_t j = i + 1; j < type2.params.size(); j++) {
                subst(type2.params[j].type, param2.name.get(), replacement);
            }
            subst(type2.return_type, param2.name.get(), replacement);
        }
    }

    return type_equal(context, type1.return_type, type2.return_type);
}

bool type_equal(Context& context, const Lambda& type1, Lambda& type2) {
    if (type1.params.size() != type2.params.size()) return false;

    for (size_t i = 0; i < type1.params.size(); i++) {
        auto param1 = type1.params[i];
        auto param2 = type2.params[i];

        if (!kind_equal(context, param1.type, param2.type)) return false;

        if (param1.name != param2.name) {
            param2.name = param1.name;
            // No need to subst in param types, since they should be kinds.
            subst(type2.body, param2.name, Ident(param1.name));
        }
    }

    return type_equal(context, type1.body, type2.body);
}

bool type_equal(Context& context, const Call& type1, Call& type2) {
    if (type1.args.size() != type2.args.size()) return false;

    for (size_t i = 0; i < type1.args.size(); i++) {
        if (!type_equal(context, type1.args[i], type2.args[i])) return false;
    }

    return type_equal(context, type1.func, type2.func);
}

struct TypeEqualVisitor : public static_visitor<bool> {
    Context& context;
    TypeEqualVisitor(Context& context) : context(context) {}

    template <typename T>
    bool operator()(const T& type1, T& type2) const {
        return type_equal(context, type1, type2);
    }

    template <typename T, typename S>
    bool operator()(const T& type1, S& type2) const {
        return false;
    }
};


bool type_equal(Context& context, const Expr& type1, Expr& type2) {
    return apply_visitor(TypeEqualVisitor(context), type1, type2);
}

bool type_equal(Context& context, const Expr& type1, const Expr& type2) {
    const Expr reduced_type1 = type_compute(context, type1);
    Expr reduced_type2 = type_compute(context, type2);

    return type_equal(context, reduced_type1, reduced_type2);
}

/***** Kind Equality *********************************************************/
bool kind_equal(Context& context, const Type& kind1, const Type& kind2) {
    return true;
}

bool kind_equal(Context& context, const Forall& kind1, const Forall& kind2) {
    if (kind1.params.size() != kind2.params.size()) {
        return false;
    }

    for (size_t i = 0; i < kind1.params.size(); i++) {
        if (!kind_equal(context, kind1.params[i].type, kind2.params[i].type)) {
            return false;
        }
    }

    return kind_equal(context, kind1.return_type, kind2.return_type);
}

struct KindEqualVisitor : public static_visitor<bool> {
    Context& context;
    KindEqualVisitor(Context& context) : context(context) {}

    // These are not valid since there is no kind-level abstraction.
    bool operator()(const Ident& _1, const Ident& _2) const { return false; }
    bool operator()(const Lambda& _1, const Lambda& _2) const { return false; }
    bool operator()(const Call& _1, const Call& _2) const { return false; }

    template <typename T>
    bool operator()(const T& kind1, const T& kind2) const {
        return kind_equal(context, kind1, kind2);
    }

    template <typename T, typename S>
    bool operator()(const T& _1, const S& _2) const {
        return false;
    }
};

bool kind_equal(Context& context, const Expr& kind1, const Expr& kind2) {
    return apply_visitor(KindEqualVisitor(context), kind1, kind2);
}

/***** Type-level computation ************************************************/

// Types in System-Fω form a simply-typed lambda calculus themselves. As such,
// if we assume that our types are well-kinded we can perform β and η
// reductions without double checking that arguments are of the correct kind.
// This in turn means we don't need to modify the context and can just use it
// for looking up type definitions. Assuming that all identifiers are replaced
// with their definitions already, the following set of rules define small-step
// reductions for type-level computation.
//
//       ((a: k1, b: k2, ...) => T)(A, B, ...) ~> T[A/a, B/b, .../...]
// β-red ---------------------------------------------------------
//
//
//       (a: k1, b: k2, ...) => T(a, b, ...) ~> T
// η-red ----------------------------------------
//
//
//        (a: k1, b: k2, ...) => T ~> (a: k1, b: k2, ...) => S
// λ-cong ----------------------------------------------------
//                               T ~> S
//
//        (A: k1, ..., a: T1, ...) -> T ~> (A: k1, ..., a: S1, ...) -> S
// ∀-cong --------------------------------------------------------------
//                       T1 ~> S1  ... ~> ...  T ~> S
//
//           T(T1, T2, ...) ~> S(S1, S2, ...)
// β-cong --------------------------------------
//        T ~> S  T1 ~> S1  T2 ~> S2  ... ~> ...
//
// The β and η reduction rules are the usual ones for any lambda calculus. The
// congruence rules on lambdas, forall types, and application simply state that
// sub-components of types reduce, ie that we are going for normal form and not
// weak-head normal form.

Expr type_compute(Context& context, const Ident& type) {
    optional<Expr> definition = context.lookup_type_definition(type.ident);

    if (definition) {
        return type_compute(context, definition.get());
    } else {
        return type;
    }
}

Expr type_compute(Context& context, const Type& type) {
    assert(!"'Type' is not a valid type.");
}

// ∀-cong
Expr type_compute(Context& context, const Forall& type) {
    Forall result = type;

    for (size_t i = 0; i < type.params.size(); i++) {
        if (sort_infer(context, type.params[i].type)) {
            // No need to reduce since it is a kind.
        } else {
            result.params[i].type = type_compute(context, type.params[i].type);
        }
    }

    result.return_type = type_compute(context, type.return_type);
    return result;
}

// λ-cong and η-red
Expr type_compute(Context& context, const Lambda& type) {
    Lambda result = type;
    result.body = type_compute(context, type.body);

    if (!is_call(type.body)) return result;
    const Call& inner_call = strict_get<Call>(type.body);
    if (inner_call.args.size() != type.params.size()) return result;

    for (size_t i = 0; i < type.params.size(); i++) {
        if (!is_ident(inner_call.args[i])) return result;
        const Ident& inner_call_arg = strict_get<Ident>(inner_call.args[i]);
        if (inner_call_arg.ident != type.params[i].name) return result;
    }

    return type_compute(context, inner_call.func);
}

// β-cong and β-red
Expr type_compute(Context& context, const Call& type) {
    Expr result_func = type_compute(context, type.func);
    vector<Expr> result_args;

    for (auto arg : type.args) {
        result_args.push_back(type_compute(context, arg));
    }

    if (is_lambda(result_func)) {
        Lambda func = strict_get<Lambda>(result_func);
        assert(func.params.size() == result_args.size());

        for (size_t i = 0; i < func.params.size(); i++) {
            subst(func.body, func.params[i].name, result_args[i]);
        }

        return type_compute(context, func.body);
    } else {
        return Call(result_func, result_args);
    }
}

struct TypeComputeVisitor : public static_visitor<Expr> {
    Context& context;
    TypeComputeVisitor(Context& context) : context(context) {}

    template <typename T>
    Expr operator()(const T& type) const {
        return type_compute(context, type);
    }
};

Expr type_compute(Context& context, const Expr& type) {
    return apply_visitor(TypeComputeVisitor(context), type);
}

} /* namespace ast::expr */

} /* namespace ast */
