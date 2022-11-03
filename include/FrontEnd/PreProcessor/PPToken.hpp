#pragma once

#include <string_view>
#include <string>

class PPToken
{
  public:
    enum PPTokenKind {
        EndOfFile,
        Invalid,

        Identifier,

        // Symbols
        Colon,
        Hashtag,
        LeftParen,
        RightParen,

        // Keywords
        Define,
    };

    PPToken() : Kind(Invalid) {}
    PPToken(PPTokenKind TK) : Kind(TK) {}
    PPToken(PPTokenKind TK, std::string_view sv) : Kind(TK), StringValue(sv) {}

    std::string GetString() const { return std::string(StringValue); }
    PPTokenKind GetKind() const { return Kind; }

    std::string ToString() const;
    static std::string ToString(PPTokenKind TK);

    bool IsKeyword() const { return Kind >= Define; }

  private:
    PPTokenKind Kind;
    std::string_view StringValue;
};
