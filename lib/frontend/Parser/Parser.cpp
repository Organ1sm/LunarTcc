#include "frontend/Parser/Parser.hpp"

static bool IsTypeSpecifier(Token::TokenKind tk)
{
    return tk == Token::Int || tk == Token::Double;
}

static bool IsReturnTypeSpecifier(Token::TokenKind tk)
{
    return tk == Token::Void || IsTypeSpecifier(tk);
}

static Type ParseType(Token::TokenKind tk)
{
    Type Result;
    switch (tk)
    {
        case Token::Void:
            Result.SetTypeVariant(Type::Void);
        case Token::Int:
            Result.SetTypeVariant(Type::Int);
        case Token::Double:
            Result.SetTypeVariant(Type::Double);

        default:
            // TODO
            break;
    }

    return Result;
}

Token Parser::Expect(Token::TokenKind Token)
{
    if (lexer.IsNot(Token))
    {
        std::cout << "Error: Expected other thing here, not this '"
                  << lexer.GetCurrentToken().GetString() << "'." << std::endl;
        return {};
    }
    else
    {
        return Lex();
    }
}

std::unique_ptr<Node> Parser::Parse() { return ParseExternalDeclaration(); }

unsigned Parser::ParseIntegerConstant()
{
    Token T       = Expect(Token::Integer);
    auto TokenStr = T.GetString();
    unsigned Result = 0;
    for(auto c : TokenStr)
    {
        Result *= 10;
        Result += c - '0';
    }

    return Result;
}

double Parser::ParseRealConstant() { return 0; }

std::unique_ptr<Node> Parser::ParseTranslationUnit() { return std::unique_ptr<Node>(); }


// <ExternalDeclaration> ::= <FunctionDeclaration>
//                         | <VaraibleDeclaration>
std::unique_ptr<Node> Parser::ParseExternalDeclaration()
{
    std::unique_ptr<TranslationUnit> TU = std::make_unique<TranslationUnit>();
    auto TokenKind                      = GetCurrentTokenKind();

    while (IsReturnTypeSpecifier(TokenKind))
    {
        Type Type = ParseType(TokenKind);
        Lex();

        auto Name    = Expect(Token::Identifier);
        auto NameStr = Name.GetString();

        // typeSpecifier funcname (T1 a, ...);
        if (lexer.Is(Token::LeftParen))
        {
            Lex();    // consume '('
            auto PL = ParseParameterList();

            Expect(Token::RightParen);

            // for now assume that a function is defined
            // TODO: planed to have function Declaration.

            auto Body = ParseCompoundStatement();

            auto Function =
                std::make_unique<FunctionDeclaration>(Type, NameStr, PL, Body);
            TU->AddDeclaration(std::move(Function));
        }
        else
        {
            // Variable Declaration;
            // int a = 0;

            std::vector<unsigned> Dimensions;

            // Array Declaration.
            while (lexer.Is(Token::LeftBracket))
            {
                Lex();    // consume '['
                Dimensions.push_back(ParseIntegerConstant());
                Expect(Token::RightBracket);
                if (lexer.IsNot(Token::Colon))
                    break;
                Lex();    // consume ','
            }

            Expect(Token::SemiColon);

            TU->AddDeclaration(
                std::make_unique<VariableDeclaration>(NameStr, Type, Dimensions));
        }

        TokenKind = GetCurrentTokenKind();
    }

    return TU;
}

std::unique_ptr<Node> Parser::ParseFunctionDeclaration()
{
    return std::unique_ptr<Node>();
}

// <ParameterDeclaration> ::= <TypeSpecifier> <Identifier>?
std::unique_ptr<FunctionParameterDeclaration> Parser::ParseParameterDeclaration()
{
    std::unique_ptr<FunctionParameterDeclaration> FPD =
        std::make_unique<FunctionParameterDeclaration>();

    if (IsTypeSpecifier(GetCurrentTokenKind()))
    {
        Type type = ParseTypeSpecifier();
        Lex();
        FPD->SetType(type);

        if (lexer.Is(Token::Identifier))
        {
            auto IdName = Lex().GetString();
            FPD->SetName(IdName);
        }
    }

    return FPD;
}

// <ParameterList> ::= <ParameterDeclaration> {',' <ParameterDeclaration>}*
std::vector<std::unique_ptr<FunctionParameterDeclaration>> Parser::ParseParameterList()
{
    std::vector<std::unique_ptr<FunctionParameterDeclaration>> Params;

    if (!IsTypeSpecifier(GetCurrentTokenKind()))
        return Params;

    Params.push_back(ParseParameterDeclaration());

    while (lexer.Is(Token::Comma))
    {
        Lex();
        Params.push_back(ParseParameterDeclaration());
    }

    return Params;
}


Type Parser::ParseTypeSpecifier()
{
    auto TokenKind = GetCurrentTokenKind();

    // TODO emit Error;
    if (!IsTypeSpecifier(TokenKind))
        ;

    return ParseType(TokenKind);
}

Node Parser::ParseReturnTypeSpecifier() { return Node(); }


// <VaraibleDeclaration> ::= <TypeSpecifier> <Identifier>
//                           {'[' <IntegerConstant> ]'}* ';'
std::unique_ptr<VariableDeclaration> Parser::ParseVariableDeclaration()
{
    auto Type = ParseTypeSpecifier();
    Lex();

    std::string Name = Expect(Token::Identifier).GetString();
    std::vector<unsigned> Dimensions;

    while (lexer.Is(Token::LeftBracket))
    {
        Lex();
        Dimensions.push_back(ParseIntegerConstant());
        Expect(Token::RightBracket);
    }

    Expect(Token::SemiColon);

    return std::make_unique<VariableDeclaration>(Name, Type, Dimensions);
}


// <Statement> ::= <ExpressionStatement>
//               | <WhileStatement>
//               | <IfStatement>
//               | <CompoundStatement>
//               | <ReturnStatement>
std::unique_ptr<Statement> Parser::ParseStatement()
{
    if (lexer.Is(Token::If))
        return ParseStatement();
    if (lexer.Is(Token::While))
        return ParseWhileStatement();
    if (lexer.Is(Token::LeftBrace))
        return ParseCompoundStatement();
    if (lexer.Is(Token::Return))
        return ParseReturnStatement();

    return ParseExpressionStatement();
}
// <IfStatement> ::= if '(' <Expression> ')' <Statement> {else <Statement>}?
std::unique_ptr<IfStatement> Parser::ParseIfStatement()
{
    auto IS = std::make_unique<IfStatement>();

    Expect(Token::If);
    Expect(Token::LeftParen);
    IS->SetCondition(std::move(ParseExpression()));
    Expect(Token::RightParen);
    IS->SetIfBody(std::move(ParseStatement()));

    if (lexer.Is(Token::Else))
    {
        Lex();
        IS->SetElseBody(ParseStatement());
    }

    return IS;
}

// <WhileStatement> ::= while '(' <Expression> ')' <Statement>
std::unique_ptr<WhileStatement> Parser::ParseWhileStatement()
{
    auto WS = std::make_unique<WhileStatement>();
    Expect(Token::While);
    Expect(Token::LeftParen);
    WS->SetCondition(std::move(ParseExpression()));
    Expect(Token::RightParen);
    WS->SetBody(std::move(ParseStatement()));

    return WS;
}

// <ReturnStatement> ::= return <Expression>? ';'
std::unique_ptr<ReturnStatement> Parser::ParseReturnStatement()
{
    Expect(Token::Return);
    auto RS = std::make_unique<ReturnStatement>(ParseExpression());
    Expect(Token::SemiColon);

    return RS;
}

// <CompoundStatement> ::= '{' <VaraibleDeclaration>* <Statement>* '}'
std::unique_ptr<CompoundStatement> Parser::ParseCompoundStatement()
{
    Expect(Token::LeftBrace);

    std::vector<std::unique_ptr<VariableDeclaration>> Declarations;
    std::vector<std::unique_ptr<Statement>> Statements;

    while (IsTypeSpecifier(GetCurrentTokenKind()))
        Declarations.push_back(std::move(ParseVariableDeclaration()));

    while (lexer.IsNot(Token::RightBrace))
        Statements.push_back(std::move(ParseStatement()));

    Expect(Token::RightBrace);

    return std::make_unique<CompoundStatement>(Declarations, Statements);
}

// <ExpressionStatement> ::= <Expression>? ';'
std::unique_ptr<ExpressionStatement> Parser::ParseExpressionStatement()
{
    auto ES = std::make_unique<ExpressionStatement>();

    if (lexer.IsNot(Token::SemiColon))
        ES->SetExpression(std::move(ParseExpression()));
    Expect(Token::SemiColon);

    return ES;
}

std::unique_ptr<Expression> Parser::ParseExpression() { return ParseBinaryExpression(); }

static int GetBinOpPrecedence(Token::TokenKind TK)
{
    switch (TK)
    {
        case Token::Assign:
            return 10;
        case Token::LogicalAnd:
            return 20;
        case Token::And:
            return 30;
        case Token::Equal:
        case Token::NotEqual:
            return 40;
        case Token::Less:
        case Token::Greater:
            return 50;
        case Token::Plus:
        case Token::Minus:
            return 60;
        case Token::Mul:
        case Token::Div:
        case Token::Mod:
            return 70;
        case Token::Not:
            return 80;
        default:
            return -1;
    }
}

std::unique_ptr<Expression> Parser::ParseBinaryExpression()
{
    auto LeftExpression = ParsePrimaryExpression();
    return ParseBinaryExpressionRHS(0, std::move(LeftExpression));
}

std::unique_ptr<Expression> Parser::ParsePrimaryExpression()
{
    return std::unique_ptr<Expression>();
}

std::unique_ptr<Expression> Parser::ParseIndentifierExpression()
{
    return std::unique_ptr<Expression>();
}

std::unique_ptr<Expression> Parser::ParseConstantExpression()
{
    return std::unique_ptr<Expression>();
}

std::unique_ptr<Expression>
    Parser::ParseBinaryExpressionRHS(int Precedence, std::unique_ptr<Expression> LHS)
{
    while (true)
    {
        int TokenPrec = GetBinOpPrecedence(GetCurrentTokenKind());

        if (TokenPrec < Precedence)
            return LHS;

        Token BinaryOperator = Lex();
        auto RightExpression = ParsePrimaryExpression();

        int NextTokenPrec = GetBinOpPrecedence(GetCurrentTokenKind());
        if (TokenPrec < NextTokenPrec)
            RightExpression =
                ParseBinaryExpressionRHS(TokenPrec + 1, std::move(RightExpression));

        LHS = std::make_unique<BinaryExpression>(std::move(LHS), BinaryOperator,
                                                 std::move(RightExpression));
    }
}
