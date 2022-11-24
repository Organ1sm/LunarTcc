#include "FrontEnd/Lexer/Lexer.hpp"
#include <cassert>
#include <cctype>
#include <cstdint>
#include <optional>

std::unordered_map<std::string, Token::TokenKind> Lexer::KeyWords =
    std::unordered_map<std::string, Token::TokenKind> {
        {"short",    Token::Short   },
        {"int",      Token::Int     },
        {"long",     Token::Long    },
        {"double",   Token::Double  },
        {"void",     Token::Void    },
        {"char",     Token::Char    },
        {"unsigned", Token::Unsigned},

        {"const",    Token::Const   },

        {"if",       Token::If      },
        {"else",     Token::Else    },
        {"switch",   Token::Switch  },
        {"case",     Token::Case    },
        {"break",    Token::Break   },
        {"default",  Token::Default },
        {"for",      Token::For     },
        {"while",    Token::While   },
        {"return",   Token::Return  },
        {"struct",   Token::Struct  },
        {"enum",     Token::Enum    },
        {"typedef",  Token::TypeDef },
        {"continue", Token::Continue},
};

Lexer::Lexer(std::vector<std::string> &s)
{
    Source      = std::move(s);
    TokenBuffer = std::vector<Token>();
    LineIndex   = 0;
    ColumnIndex = 0;

    LookAhead(1);
}

void Lexer::ConsumeCurrentToken()
{
    assert(!TokenBuffer.empty() && "TokenBuffer is empty.");
    TokenBuffer.erase(TokenBuffer.begin());
}

int Lexer::GetNextChar()
{
    if (LineIndex < Source.size() && Source[LineIndex].length() == 0)
        EatNextChar();

    if (LineIndex >= Source.size() ||
        (LineIndex == Source.size() - 1 && (ColumnIndex == Source[LineIndex].length())))
        return EOF;

    return Source[LineIndex][ColumnIndex];
}

int Lexer::GetNextNthCharOnSameLine(unsigned int n)
{
    if (LineIndex >= Source.size() || (ColumnIndex + n >= Source[LineIndex].length()))
        return EOF;
    return Source[LineIndex][ColumnIndex + n];
}

void Lexer::EatNextChar()
{
    if (LineIndex < Source.size())
    {
        // skip empty line.
        if (Source[LineIndex].empty() || ColumnIndex >= Source[LineIndex].size() - 1)
        {
            ColumnIndex = 0;
            LineIndex++;
        }
        else
        {
            ColumnIndex++;
        }
    }
}
std::optional<Token> Lexer::LexNumber()
{
    auto StartLineIndex   = LineIndex;
    auto StartColumnIndex = ColumnIndex;
    auto TokenKind        = Token::Integer;
    auto Length           = 0u;
    unsigned TokenValue   = 0;

    while (isdigit(GetNextChar()))
    {
        Length++;
        EatNextChar();
    }

    // if value like 3.1415926
    if (GetNextChar() == '.')
    {
        Length++;
        EatNextChar();
        TokenKind = Token::Real;

        // TODO: it might be better to make Invalid token.
        if (!isdigit(GetNextChar()))
            return std::nullopt;

        while (isdigit(GetNextChar()))
        {
            Length++;
            EatNextChar();
        }
    }
    else if (Length == 1 && (GetNextChar() == 'x' || GetNextChar() == 'X'))
    {
        Length++;

        EatNextChar();    // Eat 'x' or 'X'
        uint64_t value = 0;

        int ch = GetNextChar();

        // TODO: use is-funciton simpilify it.
        while (isdigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
        {
            Length++;
            EatNextChar();

            unsigned CurrentDigit = 0;

            if (isdigit(ch))
                CurrentDigit = ch - '0';
            else if (islower(ch))
                CurrentDigit = ch - 'a' + 10;
            else
                CurrentDigit = ch - 'A' + 10;

            value = (value << 4) + CurrentDigit;

            ch = GetNextChar();
        }

        TokenValue = value;
    }

    if (Length == 0)
        return std::nullopt;

    std::string_view StringValue {&Source[StartLineIndex][StartColumnIndex], Length};
    return Token(TokenKind, StringValue, StartLineIndex, StartColumnIndex, TokenValue);
}

std::optional<Token> Lexer::LexIdentifier()
{
    auto StartLineIndex   = LineIndex;
    auto StartColumnIndex = ColumnIndex;
    auto Length           = 0u;
    auto TokenKind        = Token::Identifier;

    if (isdigit(GetNextChar()))
        return std::nullopt;

    while (isalnum(GetNextChar()) || GetNextChar() == '_')
    {
        Length++;
        EatNextChar();
    }

    if (Length == 0)
        return std::nullopt;

    std::string_view StringValue {&Source[StartLineIndex][StartColumnIndex], Length};
    return Token(TokenKind, StringValue, StartLineIndex, StartColumnIndex);
}

std::optional<Token> Lexer::LexKeyWord()
{
    std::size_t WordEnd =
        Source[LineIndex].substr(ColumnIndex).find_first_of("\t\n\v\f\r;: ");
    auto Word = Source[LineIndex].substr(ColumnIndex, WordEnd);

    if (!KeyWords.count(Word))
        return std::nullopt;

    auto StartLineIndex   = LineIndex;
    auto StartColumnIndex = ColumnIndex;

    for (std::size_t i = Word.length(); i > 0; i--)
        EatNextChar();

    std::string_view StringValue {&Source[StartLineIndex][StartColumnIndex],
                                  Word.length()};
    return Token(KeyWords[Word], StringValue, StartLineIndex, StartColumnIndex);
}

std::optional<Token> Lexer::LexSymbol()
{
    auto TokenKind = Token::Invalid;
    unsigned Size  = 1;

    switch (GetNextChar())
    {
        case '.': TokenKind = Token::Dot; break;
        case ',': TokenKind = Token::Comma; break;
        case '+':
            if (GetNextNthCharOnSameLine(1) == '+')
            {
                TokenKind = Token::Inc;
                Size      = 2;
            }
            else if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::PlusEqual;
                Size      = 2;
            }
            else
                TokenKind = Token::Plus;
            break;
        case '-':
            if (GetNextNthCharOnSameLine(1) == '-')
            {
                TokenKind = Token::Dec;
                Size      = 2;
            }
            else if (GetNextNthCharOnSameLine(1) == '>')
            {
                TokenKind = Token::Arrow;
                Size      = 2;
            }
            else if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::MinusEuqal;
                Size      = 2;
            }
            else
                TokenKind = Token::Minus;
            break;
        case '*':
            if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::MulEqual;
                Size      = 2;
            }
            else
                TokenKind = Token::Mul;
            break;
        case '/':
            if (GetNextNthCharOnSameLine(1) == '/')
            {
                TokenKind = Token::SingleComment;
                Size      = 2;
            }
            else if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::DivEqual;
                Size      = 2;
            }
            else
            {
                TokenKind = Token::Div;
            }
            break;
        case '%': TokenKind = Token::Mod; break;
        case '=':
            if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::Equal;
                Size      = 2;
            }
            else
            {
                TokenKind = Token::Assign;
            }
            break;
        case '<':
            if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::LessEqual;
                Size      = 2;
            }
            else if (GetNextNthCharOnSameLine(1) == '<')
            {
                TokenKind = Token::LeftShift;
                Size      = 2;
            }
            else
            {
                TokenKind = Token::Less;
            }
            break;
        case '>':
            if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::GreaterEqual;
                Size      = 2;
            }
            else if (GetNextNthCharOnSameLine(1) == '>')
            {
                TokenKind = Token::RightShift;
                Size      = 2;
            }
            else
            {
                TokenKind = Token::Greater;
            }
            break;
        case '!':
            if (GetNextNthCharOnSameLine(1) == '=')
            {
                TokenKind = Token::NotEqual;
                Size      = 2;
            }
            else
            {
                TokenKind = Token::Not;
            }
            break;
        case '?': TokenKind = Token::Cond; break;
        case '&':
            if (GetNextNthCharOnSameLine(1) == '&')
            {
                TokenKind = Token::LogicalAnd;
                Size      = 2;
            }
            else
            {
                TokenKind = Token::And;
            }
            break;
        case '^': TokenKind = Token::Xor; break;

        case '\\': TokenKind = Token::BackSlash; break;
        case ':': TokenKind = Token::Colon; break;
        case ';': TokenKind = Token::SemiColon; break;
        case '(': TokenKind = Token::LeftParen; break;
        case ')': TokenKind = Token::RightParen; break;
        case '[': TokenKind = Token::LeftBracket; break;
        case ']': TokenKind = Token::RightBracket; break;
        case '{': TokenKind = Token::LeftBrace; break;
        case '}': TokenKind = Token::RightBrace; break;
        default: return std::nullopt;
    }

    std::string_view StringValue {&Source[LineIndex][ColumnIndex], Size};
    auto Result = Token(TokenKind, StringValue, LineIndex, ColumnIndex);

    EatNextChar();
    if (Size == 2)
        EatNextChar();

    return Result;
}

Token Lexer::LookAhead(unsigned n)
{
    // fill in the TokenBuffer to have at least n element
    for (std::size_t i = TokenBuffer.size(); i < n; i++)
        TokenBuffer.push_back(Lex(true));

    return TokenBuffer[n - 1];
}

bool Lexer::Is(Token::TokenKind tk)
{
    // fill in the buffer with one token if it is empty
    if (TokenBuffer.size() == 0)
        LookAhead(1);

    return GetCurrentToken().GetKind() == tk;
}

bool Lexer::IsNot(Token::TokenKind tk) { return !Is(tk); }

Token Lexer::Lex(bool LookAhead)
{
    // if the TokenBuffer not empty then return the Token from there
    // and remove it from the stack
    if (TokenBuffer.size() > 0 && LookAhead == false)
    {
        auto CurrentToken = GetCurrentToken();
        ConsumeCurrentToken();
        return CurrentToken;
    }

    int CurrentCharacter = GetNextChar();
    std::string WhiteSpaceChars("\t\n\v\f\r ");

    // consume white space characters
    while (WhiteSpaceChars.find(CurrentCharacter) != std::string::npos ||
           CurrentCharacter == '\0')
    {
        EatNextChar();
        CurrentCharacter = GetNextChar();
    }

    if (CurrentCharacter == EOF)
    {
        return {Token::EndOfFile};
    }

    auto Result = LexKeyWord();

    if (!Result)
        Result = LexSymbol();

    if (!Result)
        Result = LexNumber();

    if (!Result)
        Result = LexCharLiteral();

    if (!Result)
        Result = LexStringLiteral();

    if (!Result)
        Result = LexIdentifier();

    if (Result.has_value() && Result.value().GetKind() == Token::SingleComment)
    {
        LineIndex++;
        ColumnIndex = 0;
        return Lex();
    }

    if (Result)
        return Result.value();

    return {Token::Invalid};
}

std::optional<Token> Lexer::LexCharLiteral()
{
    unsigned StartLineIndex   = LineIndex;
    unsigned StartColumnIndex = ColumnIndex;

    // It must start with a `'` character
    if (GetNextChar() != '\'')
        return std::nullopt;

    EatNextChar();    // eat `'` character

    bool IsEscaped = false;
    if (GetNextChar() == '\\')
    {
        IsEscaped = true;
        EatNextChar();
    }

    auto CurrentChar = GetNextChar();
    EatNextChar();
    unsigned value = -1;

    // TODO: Add support for other cases like multiple octal digits, hex etc...
    if (IsEscaped)
    {
        if (isdigit(CurrentChar))
        {
            assert(CurrentChar < '8' && "Expecting octal digits");
            value = CurrentChar - '0';
        }
        else if (CurrentChar == 'a')
            value = 0x07;
        else if (CurrentChar == 'b')
            value = 0x08;
        else if (CurrentChar == 'e')
            value = 0x1B;
        else if (CurrentChar == 'f')
            value = 0x0C;
        else if (CurrentChar == 'n')
            value = 0x0A;
        else if (CurrentChar == 'r')
            value = 0x0D;
        else if (CurrentChar == 't')
            value = 0x0B;
        else
            assert("TODO: add the other ones");
    }
    else
    {
        value = CurrentChar;
    }
    if (GetNextChar() != '\'')
        return Token(Token::Invalid);

    EatNextChar();    // eat `'` character

    std::string_view StringValue {&Source[StartLineIndex][StartColumnIndex],
                                  ColumnIndex - StartColumnIndex};
    return Token(Token::CharacterLiteral,
                 StringValue,
                 StartLineIndex,
                 StartColumnIndex,
                 value);
}

std::optional<Token> Lexer::LexStringLiteral()
{
    unsigned StartLineIndex   = LineIndex;
    unsigned StartColumnIndex = ColumnIndex;
    unsigned Length           = 0;

    if (GetNextChar() != '"')
        return std::nullopt;

    EatNextChar();    // eat '"'
    Length++;

    bool LastCharIsEscape = false;

    while ((GetNextChar() != '"' || (GetNextChar() == '"' && LastCharIsEscape)) &&
           GetNextChar() != EOF)
    {
        char ch          = GetNextChar();
        LastCharIsEscape = (ch == '\\');

        EatNextChar();
        Length++;
    }

    if (GetNextChar() != '"')
        return Token(Token::Invalid);

    EatNextChar();
    Length++;

    std::string_view StringValue {&Source[StartLineIndex][StartColumnIndex], Length};
    return Token(Token::StringLiteral, StringValue, StartLineIndex, StartColumnIndex);
}
