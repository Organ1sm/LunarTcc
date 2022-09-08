#ifndef LUNARTCC_TOKEN_H
#define LUNARTCC_TOKEN_H

#include <cstdint>
#include <string>
#include <unordered_map>


class Token
{
  public:
    enum TokenKind {
        EndOfFile,
        Invalid,

        Identifier,

        // Numbers
        Integer,
        Real,

        // Punctuators
        LeftParen,       // '('
        RightParen,      // ')'
        LeftBracket,     // '['
        RightBracket,    // ']'
        LeftBrace,       // '{'
        RightBrace,      // '}'
        Colon,           // ':'
        Comma,           // ','
        SemiColon,       // ';'

        // Opeartors
        Plus,       // '+'
        Minus,      // '-'
        Mul,        // '*'
        Div,        // '/'
        Or,         // '|'
        And,        // '&'
        Xor,        // '^'
        Less,       // '<'
        Greater,    // '>'
        Assign,     // '='
        Dot,        // '.'
        Mod,        // '%'
        Tilde,      // '~'
        Not,        // '!'
        Cond,       // '?'

        // Multichar Operators
        Ptr,             // ->
        Inc,             // ++
        Dec,             // --
        Left,            // <<
        Right,           // >>
        LessEqual,       // <=
        GreaterEqual,    // >=
        Equal,           // ==
        NotEqual,        // !=
        LogicalAnd,      // &&
        LogicalOr,       // ||
        MulAssign,       // *=
        DivAssign,       // /=
        ModAssign,       // %=
        AddAssign,       // +=
        SubAssign,       // -=
        LeftAssign,      // <<=
        RightAssign,     // >>=
        XorAssign,       // ^=
        OrAssign,        // |=
        AndAssign,       // &=
        Ellipsis,        // ...

        // Keyword
        // Type qualifier
        Const,
        Restrict,
        Volatile,
        Atomic,

        // Type specifier
        For,
        While,
        Do,
        If,
        Switch,
        Case,
        Break,
        Default,
        Continue,
        Else,
        Goto,
        Return,
        Void,
        Char,
        Short,
        Int,
        Long,
        Float,
        Double,
        Signed,
        Unsigned,
        Struct,
        Union,
        Enum,
        Inline,
        Sizeof,

        // C11, Not Support
        // todo
        Bool,            // _Bool
        Complex,         // _Complex
        Alignas,         // _Alignas
        Alignof,         // _Alignof
        Generic,         // _Generic
        Imaginary,       // _Imaginary
        Attribute,       // GNU extension __attribute__
        Noreturn,        // _Noreturn
        StaticAssert,    // _Static_assert
        ThreadLocal,     // _Thread_local
    };

    Token() : Kind(Invalid) {}

    Token(TokenKind tk) : Kind {tk} {}

    Token(TokenKind tk, std::string_view sv, std::size_t line, std::size_t col)
        : Kind(tk), StringValue(sv), Line(line), Column(col)
    {}

    std::string GetString() { return std::string(StringValue); }
    TokenKind GetKind() { return Kind; }

    std::size_t GetLine() { return Line; }
    std::size_t GetColumn() { return Column; }

    std::string ToString();

    static std::string ToString(TokenKind tk);

  private:
    TokenKind Kind;
    std::string_view StringValue;
    std::size_t Line;
    std::size_t Column;

  public:
    static const std::unordered_map<TokenKind, std::string> Token2Str;
};
#endif