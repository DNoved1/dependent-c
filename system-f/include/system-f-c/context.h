#ifndef SYSTEM_F_C_CONTEXT_H
#define SYSTEM_F_C_CONTEXT_H

#include <boost/optional/optional.hpp>
#include <utility>
#include <vector>

namespace context {

    class ContextMarker {
        size_t term_type_mark;
        size_t type_kind_mark;
        size_t term_definition_mark;
        size_t type_definition_mark;

        friend class Context;

        ContextMarker(
            size_t term_type_mark,
            size_t type_kind_mark,
            size_t term_definition_mark,
            size_t type_definition_mark) :
            term_type_mark(term_type_mark),
            type_kind_mark(type_kind_mark),
            term_definition_mark(term_definition_mark),
            type_definition_mark(type_definition_mark)
            {}
    };

    class Context {
        std::vector<std::pair<std::string, ast::expr::Expr>> term_context;
        std::vector<std::pair<std::string, ast::expr::Expr>> type_context;

        std::vector<std::pair<std::string, ast::expr::Expr>> term_defines;
        std::vector<std::pair<std::string, ast::expr::Expr>> type_defines;

      public:
        Context() : term_context(), type_context(),
            term_defines(), type_defines() {}

        ContextMarker mark();
        void reset(ContextMarker marker);

        void register_term(std::string name, ast::expr::Expr type);
        boost::optional<ast::expr::Expr> lookup_term(std::string name);

        void register_type(std::string name, ast::expr::Expr kind);
        boost::optional<ast::expr::Expr> lookup_type(std::string name);

        void define_term(std::string name, ast::expr::Expr term);
        boost::optional<ast::expr::Expr> lookup_term_definition(std::string name);

        void define_type(std::string name, ast::expr::Expr type);
        boost::optional<ast::expr::Expr> lookup_type_definition(std::string name);
    };
}

#endif /* SYSTEM_F_C_CONTEXT_H */
