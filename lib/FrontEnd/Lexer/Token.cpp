#include "FrontEnd/Lexer/Token.hpp"
#include "fmt/format.h"
#include <cassert>

std::string Token::ToString() const
{
    return fmt::format("`{}`, Line: {}, Col: {}, Value: {}",
                       StringValue,
                       Line + 1,
                       Column + 1,
                       Value);
}

const std::unordered_map<Token::TokenKind, std::string> Token::Token2Str = {
    {EndOfFile,          "End of file"      },
    {Invalid,            "Invalid"          },
    {Identifier,         "Identifier"       },
    {Integer,            "Integer"          },
    {Real,               "Float"            },
    {CharacterLiteral,   "Character Literal"},
    {StringLiteral,      "StringLiteral"    },

    {LeftParen,          "("                },
    {RightParen,         ")"                },
    {LeftBracket,        "["                },
    {RightBracket,       "]"                },
    {LeftBrace,          "{"                },
    {RightBrace,         "}"                },
    {Colon,              ":"                },
    {Comma,              ","                },
    {SemiColon,          ";"                },
    {BackSlash,          "\\"               },
    {Ellipsis,           "..."              },

    {Plus,               "+"                },
    {Minus,              "-"                },
    {Mul,                "*"                },
    {Div,                "/"                },
    {Assign,             "="                },
    {Mod,                "%"                },
    {Less,               "<"                },
    {Greater,            ">"                },
    {And,                "&"                },
    {Or,                 "|"                },
    {Xor,                "^"                },
    {Tilde,              "~"                },
    {Not,                "!"                },
    {Cond,               "?"                },
    {Dot,                "."                },

    {Arrow,              "->"               },
    {Inc,                "++"               },
    {Dec,                "--"               },
    {LeftShift,          "<<"               },
    {RightShift,         ">>"               },
    {PlusEqual,          "+="               },
    {MinusEuqal,         "-="               },
    {MulEqual,           "*="               },
    {DivEqual,           "/="               },
    {ModEqual,           "%="               },
    {LeftShiftEqual,     "<<="              },
    {RightShiftEqual,    ">>="              },
    {AndEqual,           "&="               },
    {OrEqual,            "|="               },
    {XorEqual,           "^="               },
    {Equal,              "=="               },
    {NotEqual,           "!="               },
    {LogicalAnd,         "&&"               },
    {LogicalOr,          "||"               },

    {GreaterEqual,       ">="               },
    {LessEqual,          "<="               },

    {SingleComment,      "//"               },
    {ForwardSlashAstrix, "/*"               },
    {AstrixForwardSlash, "*/"               },

    {Const,              "const"            },

    {If,                 "if"               },
    {Else,               "else"             },
    {Switch,             "switch"           },
    {Case,               "case"             },
    {Default,            "default"          },
    {Break,              "break"            },
    {Continue,           "continue"         },
    {For,                "for"              },
    {Do,                 "do"               },
    {While,              "while"            },
    {Return,             "return"           },
    {Short,              "short"            },
    {Int,                "int"              },
    {Long,               "long"             },
    {Float,              "float"            },
    {Double,             "double"           },
    {Void,               "void"             },
    {Char,               "char"             },
    {Unsigned,           "unsigned"         },
    {Struct,             "struct"           },
    {Enum,               "enum"             },
    {TypeDef,            "typedef"          },
    {Sizeof,             "sizeof"           },
};

std::string Token::ToString(Token::TokenKind tk)
{
    auto &Token2Str = Token::Token2Str;
    auto search     = Token2Str.find(tk);

    if (search != Token2Str.cend())
        return search->second;
    else
    {
        assert(!"Unhandled token type.");
    }
}

bool Token::IsArithmetic(Token::TokenKind TK)
{
    switch (TK)
    {
        case Token::Plus:
        case Token::Minus:
        case Token::Mul:
        case Token::Div: return true;

        default: return false;
    }
}

bool Token::IsCompositeAssignment(TokenKind TK)
{
    switch (TK)
    {
        case Token::Assign:
        case Token::ModEqual:
        case Token::AndEqual:
        case Token::OrEqual:
        case Token::XorEqual:
        case Token::LeftShiftEqual:
        case Token::RightShiftEqual:
        case Token::PlusEqual:
        case Token::MinusEuqal:
        case Token::MulEqual:
        case Token::DivEqual: return true;

        default: return false;
    }
}

bool Token::operator==(const Token &RHS)
{
    return GetString() == RHS.GetString() && Kind == RHS.Kind;
}
