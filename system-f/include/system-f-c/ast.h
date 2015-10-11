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

        // TODO: need seperate functions for getting the free variables at the
        // term and type level, since identifiers can be duplicated between them.
        /*********************************************************************\
         * Determine the set of free variables in an expression.             *
        \*********************************************************************/
        std::unordered_set<std::string> free_vars(const Expr& expr);

        // TODO: make this only substitute at the type level, since that's
        // all that is needed.
        /*********************************************************************\
         * Substitute an expression for a identifier. Equivalent to the      *
         * operation `expr[name := with]`.                                   *
        \*********************************************************************/
        void subst(Expr& expr, const std::string& name, const Expr& with);

        /*********************************************************************\
         * Pretty-print an expression.                                       *
        \*********************************************************************/
        std::ostream& operator<<(std::ostream& os, const Expr& expr);

        /*********************************************************************\
         * Determine the sort of a kind. Equivalent to the judgement:        *
         *   Γ ⊢ K : □                                                       *
        \*********************************************************************/
        bool sort_infer(context::Context& context, const Expr& kind);

        /*********************************************************************\
         * Determine the kind of a type. Equivalent to the judgement:        *
         *   Γ ⊢ T : K                                                       *
        \*********************************************************************/
        boost::optional<Expr> kind_infer(context::Context& context,
            const Expr& type);

        /*********************************************************************\
         * Determine the type of a term. Equivalent to the judgement:        *
         *   Γ ⊢ M : T                                                       *
        \*********************************************************************/
        boost::optional<Expr> type_infer(context::Context& context,
            const Expr& term);

        /*********************************************************************\
         * Determine if two types are equal. Equivalent to the judgement:    *
         *                                                                   *
         *       Γ ⊢ T = S : K                                               *
         *   ---------------------                                           *
         *   Γ ⊢ T : K   Γ ⊢ S : K                                           *
         *                                                                   *
         * Checking this may require β, η, and α conversions.                *
        \*********************************************************************/
        bool type_equal(context::Context& context,
            const Expr& type1, const Expr& type2);

        /*********************************************************************\
         * Determine if two kinds are equal. Equivalent to the judgement:    *
         *                                                                   *
         *       Γ ⊢ K1 = K2 : □                                             *
         *   -----------------------                                         *
         *   Γ ⊢ K1 : □   Γ ⊢ K2 : □                                         *
        \*********************************************************************/
        bool kind_equal(context::Context& context,
            const Expr& kind1, const Expr& kind2);

        /*********************************************************************\
         * β and η reduce a type as much as possible. The type must be       *
         * well-kinded.                                                      *
        \*********************************************************************/
        Expr type_compute(context::Context& context, const Expr& type);
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

        struct Type { };

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
