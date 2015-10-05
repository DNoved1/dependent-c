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
    return ContextMarker(
        this->term_context.size(),
        this->type_context.size(),
        this->term_defines.size(),
        this->type_defines.size());
}

void Context::reset(ContextMarker mark) {
    if (mark.term_type_mark > this->term_context.size()
            || mark.type_kind_mark > this->type_context.size()
            || mark.term_definition_mark > this->term_defines.size()
            || mark.type_definition_mark > this->type_defines.size()) {
        throw "Invalid marker";
    }

    removing_resize(this->term_context, mark.term_type_mark);
    removing_resize(this->type_context, mark.type_kind_mark);
    removing_resize(this->term_defines, mark.term_definition_mark);
    removing_resize(this->type_defines, mark.type_definition_mark);
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

void Context::define_term(string name, Expr term) {
    this->term_defines.push_back(make_pair(name, term));
}

optional<Expr> Context::lookup_term_definition(string name) {
    for (auto iter = this->term_defines.rbegin();
            iter != this->term_defines.rend(); iter++) {
        if (iter->first == name) {
            return make_optional(iter->second);
        }
    }

    return optional<Expr>();
}

void Context::define_type(string name, Expr type) {
    this->type_defines.push_back(make_pair(name, type));
}

optional<Expr> Context::lookup_type_definition(string name) {
    for (auto iter = this->type_defines.rbegin();
            iter != this->type_defines.rend(); iter++) {
        if (iter->first == name) {
            return make_optional(iter->second);
        }
    }

    return optional<Expr>();
}

} /* namespace context */
