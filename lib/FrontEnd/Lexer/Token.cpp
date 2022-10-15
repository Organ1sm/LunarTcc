#include "FrontEnd/Lexer/Token.hpp"
#include <cassert>

std::string Token::ToString() const
{
    std::string Result;
    Result += "\"" + std::string(StringValue) + "\", ";
    Result += "Line: " + std::to_string(Line + 1) + ", ";
    Result += "Col: " + std::to_string(Column + 1);

    return Result;
}

const std::unordered_map<Token::TokenKind, std::string> Token::Token2Str = {
    {EndOfFile,     "End of file"},
    {Invalid,       "Invalid"    },
    {Identifier,    "Identifier" },
    {Integer,       "Integer"    },
    {Real,          "Float"      },

    {LeftParen,     "("          },
    {RightParen,    ")"          },
    {LeftBracket,   "["          },
    {RightBracket,  "]"          },
    {LeftBrace,     "{"          },
    {RightBrace,    "}"          },
    {Colon,         ":"          },
    {Comma,         ","          },
    {SemiColon,     ";"          },

    {Plus,          "+"          },
    {Minus,         "-"          },
    {Mul,           "*"          },
    {Div,           "/"          },
    {Assign,        "="          },
    {Mod,           "%"          },
    {Less,          "<"          },
    {Greater,       ">"          },
    {And,           "&"          },
    {Not,           "!"          },

    {Equal,         "=="         },
    {NotEqual,      "!="         },
    {LogicalAnd,    "&&"         },

    {SingleComment, "//"         },

    {If,            "if"         },
    {Else,          "else"       },
    {For,           "for"        },
    {While,         "while"      },
    {Return,        "return"     },
    {Int,           "int"        },
    {Double,        "double"     },
    {Void,          "void"       },
    {Char,          "char"       }
};

std::string Token::ToString(Token::TokenKind tk)
{
    auto &Token2Str = Token::Token2Str;
    auto search     = Token2Str.find(tk);
    if (search != Token2Str.cend())
        return search->second;
    else
    {
        assert(false && "Unhandled token type.");
        return {};
    }
}
