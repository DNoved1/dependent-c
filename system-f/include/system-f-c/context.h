#ifndef SYSTEM_F_C_CONTEXT_H
#define SYSTEM_F_C_CONTEXT_H

#include <boost/optional/optional.hpp>
#include <utility>
#include <vector>

namespace context {

    class ContextMarker {
        size_t term_marker;
        size_t type_marker;

        friend class Context;

        ContextMarker(size_t term_marker, size_t type_marker)
            : term_marker(term_marker), type_marker(type_marker) {}
    };

    class Context {
        std::vector<std::pair<std::string, ast::expr::Expr>> term_context;
        std::vector<std::pair<std::string, ast::expr::Expr>> type_context;

      public:
        Context() : term_context(), type_context() {}

        ContextMarker mark();
        void reset(ContextMarker marker);

        void register_term(std::string name, ast::expr::Expr type);
        boost::optional<ast::expr::Expr> lookup_term(std::string name);

        void register_type(std::string name, ast::expr::Expr kind);
        boost::optional<ast::expr::Expr> lookup_type(std::string name);
    };
}

#endif /* SYSTEM_F_C_CONTEXT_H */
