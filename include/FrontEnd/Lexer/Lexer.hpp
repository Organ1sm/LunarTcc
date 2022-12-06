#pragma once

#include "FrontEnd/Lexer/Token.hpp"
#include <cassert>
#include <vector>
#include <string>
#include <unordered_map>
#include <optional>

class Lexer
{
  public:
    void ConsumeCurrentToken();
    int GetNextChar();
    int GetNextNthCharOnSameLine(unsigned n);
    void EatNextChar();

    std::optional<Token> LexNumber();
    std::optional<Token> LexIdentifier();
    std::optional<Token> LexKeyWord();
    std::optional<Token> LexCharLiteral();
    std::optional<Token> LexStringLiteral();
    std::optional<Token> LexSymbol();

    Token LookAhead(unsigned n);

    Token GetCurrentToken() { return LookAhead(1); }

    bool Is(Token::TokenKind tk);
    bool IsNot(Token::TokenKind tk);

    Token Lex(bool LookAhead = false);

    explicit Lexer(std::vector<std::string> &s);

  private:
    static std::unordered_map<std::string, Token::TokenKind> KeyWords;
    std::vector<std::string> Source;
    std::vector<Token> TokenBuffer;
    unsigned LineIndex;
    unsigned ColumnIndex;
};
