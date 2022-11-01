#pragma once

#include "FrontEnd/PreProcessor/PPToken.hpp"
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

class PPLexer
{
  public:
    PPLexer() = delete;
    explicit PPLexer(std::string &s);

    void ConsumeCurrentPPToken();
    int GetNextChar();
    int GetNextNthCharOnSameLine(unsigned n);

    /// Update LineIndex to make them pointing to the next input character
    void EatNextChar();

    std::optional<PPToken> LexIdentifier();
    std::optional<PPToken> LexKeyword();
    std::optional<PPToken> LexSymbol();

    PPToken LookAhead(unsigned n);
    PPToken GetCurrentPPToken() { return LookAhead(1); }
    bool Is(PPToken::PPTokenKind tk);
    bool IsNot(PPToken::PPTokenKind tk);

    std::string GetRemainingText();
    std::string &GetSource() { return Source; }
    unsigned GetLineNum() const { return LineIndex + 1; }

    PPToken Lex(bool LookAhead = false);

  private:
    unsigned LineIndex = 0;
    std::string Source;
    std::vector<PPToken> PPTokenBuffer;
    static std::unordered_map<std::string, PPToken::PPTokenKind> Keywords;
};
