#include <cctype>
#include <cassert>
#include <boost/variant/variant.hpp>
#include <boost/variant/get.hpp>

#include "system-f-c/parse.h"
#include "system-f-c/util.h"

using std::istream;
using std::string;
using std::vector;

using boost::none;
using boost::optional;
using boost::strict_get;

using ast::expr::Expr;
using ast::MaybeNamedType;
using ast::NamedType;
using util::reverse;

namespace parse {

/***** Lexing ****************************************************************/
void skip_whitespace(istream& input) {
    while (true) {
        int c = input.get();

        if (c == EOF) {
            break;
        }

        if (!isspace(c)) {
            input.putback(c);
            break;
        }
    }
}

bool is_identifier_start(char c) {
    return isalpha(c);
}

bool is_identifier_inner(char c) {
    return isalpha(c);
}

string lex_identifier(istream& input) {
    string identifier;

    int c = input.get();
    assert(is_identifier_start(c));
    identifier += c;

    while (true) {
        int c = input.get();

        if (c == EOF) {
            break;
        }

        if (is_identifier_inner(c)) {
            identifier += c;
        } else {
            input.putback(c);
            break;
        }
    }

    return identifier;
}

optional<vector<Token>> lex(istream& input) {
    vector<Token> result;

    while (true) {
        skip_whitespace(input);

        int c = input.get();

        if (is_identifier_start(c)) {
            input.putback(c);
            string identifier = lex_identifier(input);
            if (identifier == "Type") {
                result.push_back(Token(TokenType::Type));
            } else {
                result.push_back(Token(identifier));
            }
        } else if (c == '(') {
            result.push_back(Token(TokenType::LeftParen));
        } else if (c == ')') {
            result.push_back(Token(TokenType::RightParen));
        } else if (c == ',') {
            result.push_back(Token(TokenType::Comma));
        } else if (c == '-') {
            int c2 = input.get();
            if (c2 == '>') {
                result.push_back(Token(TokenType::ThinArrow));
            } else {
                return none;
            }
        } else if (c == '=') {
            int c2 = input.get();
            if (c2 == '>') {
                result.push_back(Token(TokenType::ThickArrow));
            } else {
                return none;
            }
        } else if (c == EOF) {
            return result;
        } else {
            return none;
        }
    }
}

/***** Parsing ***************************************************************/
optional<Expr> parse_expr(vector<Token>&);

// Check that the next token is the given type and pop it if it is.
bool checked_token_pop(vector<Token>& input, TokenType token_type) {
    if (input.size() == 0 || input.back().type != token_type) {
        return false;
    } else {
        input.pop_back();
        return true;
    }
}

// Parse an expression which begins with parentheses. This includes:
//   Parenthesized expressions: '(' Expr ')'
//   Forall types:              '(' {(Ident ':')? Expr}(',') ')' '->' Expr
//   Lambda expressions:        '(' {Ident ':' Expr}(',') ')' '->' Expr
optional<Expr> parse_parenthesized(vector<Token>& input) {
    if (!checked_token_pop(input, TokenType::LeftParen)) return none;

    vector<MaybeNamedType> elements;

    bool needs_comma = false;
    while (true) {
        if (needs_comma && !checked_token_pop(input, TokenType::Comma)) {
            return none;
        }
        needs_comma = true;

        if (checked_token_pop(input, TokenType::RightParen)) {
            break;
        }

        optional<Expr> first = parse_expr(input);
        if (!first) return none;

        if (is_ident(first.get()) && checked_token_pop(input, TokenType::Colon)) {
            optional<Expr> second = parse_expr(input);
            if (!second) return none;

            string ident = strict_get<ast::expr::Ident>(first.get()).ident;
            Expr type = second.get();
            elements.push_back(MaybeNamedType(ident, type));
        } else {
            elements.push_back(MaybeNamedType(first.get()));
        }
    }

    // Handle a forall type
    if (checked_token_pop(input, TokenType::ThinArrow)) {
        optional<Expr> return_type = parse_expr(input);
        if (!return_type) return none;

        return Expr(ast::expr::Forall(elements, return_type.get()));
    }

    // Handle a lambda expression
    if (checked_token_pop(input, TokenType::ThickArrow)) {
        optional<Expr> body = parse_expr(input);
        if (!body) return none;

        vector<NamedType> params;
        for (MaybeNamedType param : elements) {
            if (param.name) {
                params.push_back(NamedType(param.name.get(), param.type));
            } else {
                return none;
            }
        }

        return Expr(ast::expr::Lambda(params, body.get()));
    }

    // Handle a parenthesized expression
    {
        if (elements.size() == 1 && !elements[0].name) {
            return elements[0].type;
        } else {
            return none;
        }
    }
}

// Parse a 'basic' expression, aka anything other than a call.
optional<Expr> parse_basic_expr(vector<Token>& input) {
    optional<Expr> result;

    if (input.size() == 0) return none;
    switch (input.back().type) {
      case TokenType::Identifier:
        result = Expr(ast::expr::Ident(input.back().identifier));
        input.pop_back();
        return result;

      case TokenType::LeftParen:
        return parse_parenthesized(input);

      case TokenType::Type:
        result = Expr(ast::expr::Type());
        input.pop_back();
        return result;

      case TokenType::RightParen:
      case TokenType::Comma:
      case TokenType::Colon:
      case TokenType::ThinArrow:
      case TokenType::ThickArrow:
        return none;
    }

    return none;
}

// Parse the arguments to a call expression.
//   Call args: '(' {Expr}(',') ')'
optional<vector<Expr>> parse_call_args(vector<Token>& input) {
    if (!checked_token_pop(input, TokenType::LeftParen)) return none;

    vector<Expr> arguments;

    bool needs_comma = false;
    while (true) {
        if (needs_comma && !checked_token_pop(input, TokenType::Comma)) {
            return none;
        }
        needs_comma = true;

        if (checked_token_pop(input, TokenType::RightParen)) {
            break;
        }

        optional<Expr> argument = parse_expr(input);
        if (!argument) return none;
        arguments.push_back(argument.get());
    }

    return arguments;
}

// Parse any sort of expression.
optional<Expr> parse_expr(vector<Token>& input) {
    optional<Expr> base_ = parse_basic_expr(input);
    if (!base_) return none;
    Expr base = base_.get();

    while (true) {
        if (input.size() > 0 && input.back().type == TokenType::LeftParen) {
            optional<vector<Expr>> arguments = parse_call_args(input);
            if (!arguments) return none;
            base = ast::expr::Call(base, arguments.get());
        } else {
            break;
        }
    }

    return base;
}

optional<Expr> parse(vector<Token>& input) {
    reverse(input); // So that we can efficiently pop tokens from the back
    return parse_expr(input);
}

} /* namespace parse */
