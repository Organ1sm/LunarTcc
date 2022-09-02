#ifndef LUNARTCC_PARSER_H
#define LUNARTCC_PARSER_H

#include <cassert>
#include <stack>
#include <memory>
#include "frontend/AST/AST.hpp"
#include "frontend/Lexer/Lexer.hpp"
#include "frontend/Lexer/Type.hpp"


class Parser
{
  public:
    std::unique_ptr<Node> Parse();

    Parser() = delete;
    Parser(std::vector<std::string> &s) : lexer(s) {}

    Token Lex() { return lexer.Lex(); }

    Token::TokenKind GetCurrentTokenKind() { return lexer.GetCurrentToken().GetKind(); }

    Token Expect(Token::TokenKind Token);

    std::unique_ptr<Node> ParseTranslationUnit();
    std::unique_ptr<Node> ParseExternalDeclaration();
    Node ParseFunctionDeclaration();
    std::unique_ptr<VariableDeclaration> ParseVariableDeclaration();
    Node ParseReturnTypeSpecifier();
    std::vector<std::unique_ptr<FunctionParameterDeclaration>> ParseParameterList();
    std::unique_ptr<FunctionParameterDeclaration> ParseParameterDeclaration();
    Type ParseTypeSpecifier();
    std::unique_ptr<CompoundStatement> ParseCompoundStatement();
    std::unique_ptr<ReturnStatement> ParseReturnStatement();
    std::unique_ptr<Statement> ParseStatement();
    std::unique_ptr<ExpressionStatement> ParseExpressionStatement();
    std::unique_ptr<Expression> ParseExpression();
    std::unique_ptr<Expression> ParseBinaryExpression();
    std::unique_ptr<Expression> ParseBinaryExpressionRHS(int Precedence,
                                                         std::unique_ptr<Expression> LHS);
    std::unique_ptr<Expression> ParseIndentifierExpression();
    std::unique_ptr<Expression> ParsePrimaryExpression();
    std::unique_ptr<WhileStatement> ParseWhileStatement();
    std::unique_ptr<IfStatement> ParseIfStatement();
    std::unique_ptr<Expression> ParseConstantExpression();
    unsigned ParseIntegerConstant();
    double ParseRealConstant();

  private:
    Lexer lexer;
};



#endif