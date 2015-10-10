#ifndef SYSTEM_F_C_PARSE_H
#define SYSTEM_F_C_PARSE_H

#include <istream>
#include <boost/optional/optional.hpp>

#include "system-f-c/ast.h"

namespace parse {

    enum struct TokenType {
        Identifier,
        LeftParen,
        RightParen,
        Comma,
        Colon,
        ThinArrow,
        ThickArrow,
        Type,
    };

    struct Token {
        TokenType type;
        std::string identifier;

        Token(TokenType type) : type(type), identifier() {}
        Token(std::string identifier)
            : type(TokenType::Identifier), identifier(identifier) {}
    };

    boost::optional<std::vector<Token>> lex(std::istream& input);
    boost::optional<ast::expr::Expr> parse(std::vector<Token>& input);

}

#endif /* SYSTEM_F_C_PARSE_H */
