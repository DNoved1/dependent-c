#include "system-f-c/ast.h"
#include "system-f-c/context.h"
#include "system-f-c/util.h"

using std::make_pair;
using std::string;

using boost::optional;
using boost::make_optional;

using ast::expr::Expr;
using util::removing_resize;

namespace context {

ContextMarker Context::mark() {
    return ContextMarker(this->term_context.size(), this->type_context.size());
}

void Context::reset(ContextMarker marker) {
    if (marker.term_marker > this->term_context.size()
            || marker.type_marker > this->type_context.size()) {
        throw "Invalid marker";
    }

    removing_resize(this->term_context, marker.term_marker);
    removing_resize(this->type_context, marker.type_marker);
}

void Context::register_term(string name, Expr type) {
    this->term_context.push_back(make_pair(name, type));
}

optional<Expr> Context::lookup_term(string name) {
    for (auto iter = this->term_context.rbegin();
            iter != this->term_context.rend(); iter++) {
        if (iter->first == name) {
            return make_optional(iter->second);
        }
    }

    return optional<Expr>();
}

void Context::register_type(string name, Expr kind) {
    this->type_context.push_back(make_pair(name, kind));
}

optional<Expr> Context::lookup_type(string name) {
    for (auto iter = this->type_context.rbegin();
            iter != this->type_context.rend(); iter++) {
        if (iter->first == name) {
            return make_optional(iter->second);
        }
    }

    return optional<Expr>();
}


} /* namespace context */
