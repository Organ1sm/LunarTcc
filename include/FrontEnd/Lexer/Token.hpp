#pragma once

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
        CharacterLiteral,
        StringLiteral,

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
        BackSlash,       // "\\"

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
        Arrow,              // ->
        Inc,                // ++
        Dec,                // --
        LeftShift,          // <<
        RightShift,         // >>
        LessEqual,          // <=
        GreaterEqual,       // >=
        Equal,              // ==
        NotEqual,           // !=
        LogicalAnd,         // &&
        LogicalOr,          // ||
        MulEqual,           // *=
        DivEqual,           // /=
        ModEqual,           // %=
        PlusEqual,          // +=
        MinusEuqal,         // -=
        LeftShiftEqual,     // <<=
        RightShiftEqual,    // >>=
        XorEqual,           // ^=
        OrEqual,            // |=
        AndEqual,           // &=
        Ellipsis,           // ...

        // Comment
        SingleComment,

        // Keyword
        // FuncType qualifier
        Const,
        Restrict,
        Volatile,
        Atomic,

        // FuncType specifier
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
        TypeDef,
        Inline,
        Sizeof,

        // C11, Not Support
        // TODO: Support it.
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

    Token(TokenKind tk, std::string_view sv, std::size_t line, size_t col, unsigned v)
        : Kind(tk), StringValue(sv), Line(line), Column(col), Value(v)
    {}

    std::string GetString() const { return std::string(StringValue); }

    TokenKind GetKind() const { return Kind; }

    std::size_t GetLine() const { return Line; }
    std::size_t GetColumn() const { return Column; }
    unsigned GetValue() const { return this->Value; }

    std::string ToString() const;

    static std::string ToString(TokenKind tk);
    static bool IsArithmetic(TokenKind TK);

  private:
    TokenKind Kind;
    std::string_view StringValue;
    std::size_t Line;
    std::size_t Column;
    unsigned Value = 0;

  public:
    static const std::unordered_map<TokenKind, std::string> Token2Str;
};
