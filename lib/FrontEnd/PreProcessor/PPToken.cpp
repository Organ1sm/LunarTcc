#include "FrontEnd/PreProcessor/PPToken.hpp"
#include "fmt/format.h"
#include <cassert>

std::string PPToken::ToString() const { return fmt::format("`{}`, ", StringValue); }

std::string PPToken::ToString(PPTokenKind tk)
{
    switch (tk)
    {
        case EndOfFile: return "End of file";
        case Invalid: return "Invalid";
        case Identifier: return "Identifier";

        case Dot: return ".";
        case Colon: return ",";
        case Hashtag: return "#";
        case LeftParen: return "(";
        case RightParen: return ")";
        case DoubleQuote: return "\"";
        case ForwardSlash: return "/";
        case LessThan: return "<";
        case GreaterThan: return ">";

        case Define: return "define";
        case Include: return "include";
        case IfNotDefine: return "ifndef";
        case EndIf: return "endif";

        default: assert(false && "Unhandled token type."); break;
    }
}
