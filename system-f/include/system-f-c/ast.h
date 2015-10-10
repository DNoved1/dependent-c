#ifndef SYSTEM_F_C_AST_H
#define SYSTEM_F_C_AST_H

#include <boost/optional/optional.hpp>
#include <boost/variant/variant.hpp>
#include <boost/variant/recursive_wrapper.hpp>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

namespace context {
    class Context;
}

namespace ast {

    namespace expr {

        struct Ident;
        struct Type;
        struct Forall;
        struct Lambda;
        struct Call;

        typedef boost::variant
            < boost::recursive_wrapper<Ident>
            , boost::recursive_wrapper<Type>
            , boost::recursive_wrapper<Forall>
            , boost::recursive_wrapper<Lambda>
            , boost::recursive_wrapper<Call>
            > Expr;

        bool is_ident(const Expr& expr);
        bool is_type(const Expr& expr);
        bool is_forall(const Expr& expr);
        bool is_lambda(const Expr& expr);
        bool is_call(const Expr& expr);

        std::unordered_set<std::string> free_vars(const Expr& expr);
        void subst(Expr& expr, const std::string& name, const Expr& with);
        bool alpha_eq(const Expr& expr1, const Expr& expr2);
        std::ostream& operator<<(std::ostream& os, const Expr& expr);

        /* Check if an expression is a valid kind. */
        bool sort_infer(context::Context& context, const Expr& expr);

        /* Check if an expression is a valid type and, if so, determine its
         * kind.
         */
        boost::optional<Expr> kind_infer(context::Context& context,
            const Expr& expr);

        /* Check if an expression is a valid term and, if so, determine its
         * type.
         */
        boost::optional<Expr> type_infer(context::Context& context,
            const Expr& expr);
    }

    struct MaybeNamedType {
        boost::optional<std::string> name;
        expr::Expr type;

        MaybeNamedType(std::string name, expr::Expr type)
            : name(name), type(type) {}
        MaybeNamedType(expr::Expr type)
            : name(), type(type) {}
    };

    struct NamedType {
        std::string name;
        expr::Expr type;

        NamedType(std::string name, expr::Expr type)
            : name(name), type(type) {}
    };

    namespace expr {

        struct Ident {
            std::string ident;

            Ident(std::string ident) : ident(ident) {}
        };

        struct Type {
            Type() {}
        };

        struct Forall {
            std::vector<MaybeNamedType> params;
            Expr return_type;

            Forall(std::vector<MaybeNamedType> params, Expr return_type)
                : params(params), return_type(return_type) {}
        };

        struct Lambda {
            std::vector<NamedType> params;
            Expr body;

            Lambda(std::vector<NamedType> params, Expr body)
                : params(params), body(body) {}
        };

        struct Call {
            Expr func;
            std::vector<Expr> args;

            Call(Expr func, std::vector<Expr> args)
                : func(func), args(args) {}
        };
    }
}

#endif /* SYSTEM_F_C_AST_H */
