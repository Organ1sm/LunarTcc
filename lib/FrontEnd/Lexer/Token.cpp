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
    {Cond,          "?"          },
    {Dot,           "."          },

    {Arrow,         "->"         },
    {Inc,           "++"         },
    {Dec,           "--"         },
    {LeftShift,     "<<"         },
    {RightShift,    ">>"         },
    {PlusEqual,     "+="         },
    {MinusEuqal,    "-="         },
    {MulEqual,      "*="         },
    {DivEqual,      "/="         },
    {Equal,         "=="         },
    {NotEqual,      "!="         },
    {LogicalAnd,    "&&"         },
    {GreaterEqual,  ">="         },
    {LessEqual,     "<="         },

    {SingleComment, "//"         },

    {Const,         "const"      },

    {If,            "if"         },
    {Else,          "else"       },
    {Switch,        "switch"     },
    {Case,          "case"       },
    {Default,       "default"    },
    {Break,         "break"      },
    {Continue,      "continue"   },
    {For,           "for"        },
    {While,         "while"      },
    {Return,        "return"     },
    {Int,           "int"        },
    {Long,          "long"       },
    {Double,        "double"     },
    {Void,          "void"       },
    {Char,          "char"       },
    {Unsigned,      "unsigned"   },
    {Struct,        "struct"     },
    {Enum,          "enum"       },
    {TypeDef,       "typedef"    },
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
