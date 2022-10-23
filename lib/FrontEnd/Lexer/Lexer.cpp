#include "FrontEnd/Lexer/Lexer.hpp"
#include <cassert>

std::unordered_map<std::string, Token::TokenKind> Lexer::KeyWords =
    std::unordered_map<std::string, Token::TokenKind> {
        {"int",    Token::Int   },
        {"double", Token::Double},
        {"void",   Token::Void  },
        {"char",   Token::Char  },
        {"if",     Token::If    },
        {"else",   Token::Else  },
        {"for",    Token::For   },
        {"while",  Token::While },
        {"return", Token::Return},
        {"struct", Token::Struct},
        {"enum",   Token::Enum  },
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

    if (LineIndex >= Source.size()
        || (LineIndex == Source.size() - 1
            && (ColumnIndex == Source[LineIndex].length())))
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

    if (Length == 0)
        return std::nullopt;

    std::string_view StringValue {&Source[StartLineIndex][StartColumnIndex], Length};
    return Token(TokenKind, StringValue, StartLineIndex, StartColumnIndex);
}

std::optional<Token> Lexer::LexIdentifier()
{
    auto StartLineIndex   = LineIndex;
    auto StartColumnIndex = ColumnIndex;
    auto Length           = 0u;
    auto TokenKind        = Token::Identifier;

    while (isalpha(GetNextChar()) || GetNextChar() == '_')
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
        Source[LineIndex].substr(ColumnIndex).find_first_of("\t\n\v\f\r ");
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
        case '+': TokenKind = Token::Plus; break;
        case '-': TokenKind = Token::Minus; break;
        case '*': TokenKind = Token::Mul; break;
        case '/':
            if (GetNextNthCharOnSameLine(1) == '/')
            {
                TokenKind = Token::SingleComment;
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
        case '<': TokenKind = Token::Less; break;
        case '>': TokenKind = Token::Greater; break;
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
    while (WhiteSpaceChars.find(CurrentCharacter) != std::string::npos
           || CurrentCharacter == '\0')
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
