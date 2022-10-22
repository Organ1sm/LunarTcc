#ifndef LUNARTCC_LEXER_H
#define LUNARTCC_LEXER_H

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
    std::optional<Token> LexSymbol();

    Token LookAhead(unsigned n);

    Token GetCurrentToken() { return LookAhead(1); }

    bool Is(Token::TokenKind tk);
    bool IsNot(Token::TokenKind tk);

    std::vector<std::string> &GetSource() { return Source; }

    unsigned GetLine() { return LineIndex + 1; }

    Token Lex(bool LookAhead = false);

    Lexer(std::vector<std::string> &s);

  private:
    static std::unordered_map<std::string, Token::TokenKind> KeyWords;
    std::vector<std::string> Source;
    std::vector<Token> TokenBuffer;
    unsigned LineIndex;
    unsigned ColumnIndex;
};
#endif
