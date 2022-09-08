#ifndef LUNARTCC_PARSER_H
#define LUNARTCC_PARSER_H

#include <cassert>
#include <stack>
#include <memory>
#include "frontend/AST/AST.hpp"
#include "frontend/Lexer/Lexer.hpp"
#include "frontend/AST/Type.hpp"
#include "frontend/Parser/SymbolTable.hpp"


class Parser
{
  public:
    std::unique_ptr<Node> Parse();

    Parser() = delete;
    Parser(std::vector<std::string> &s) : lexer(s) {}

    Token Lex() { return lexer.Lex(); }
    Token Expect(Token::TokenKind Token);

    Token::TokenKind GetCurrentTokenKind() { return lexer.GetCurrentToken().GetKind(); }

    unsigned ParseIntegerConstant();
    double ParseRealConstant();

    std::unique_ptr<Node> ParseTranslationUnit();
    std::unique_ptr<Node> ParseExternalDeclaration();
    std::unique_ptr<Node> ParseFunctionDeclaration();
    std::unique_ptr<VariableDeclaration> ParseVariableDeclaration();
    std::unique_ptr<FunctionParameterDeclaration> ParseParameterDeclaration();
    std::vector<std::unique_ptr<FunctionParameterDeclaration>> ParseParameterList();

    Type ParseTypeSpecifier();
    Node ParseReturnTypeSpecifier();

    std::unique_ptr<Statement> ParseStatement();
    std::unique_ptr<IfStatement> ParseIfStatement();
    std::unique_ptr<WhileStatement> ParseWhileStatement();
    std::unique_ptr<ReturnStatement> ParseReturnStatement();
    std::unique_ptr<CompoundStatement> ParseCompoundStatement();
    std::unique_ptr<ExpressionStatement> ParseExpressionStatement();

    std::unique_ptr<Expression> ParseExpression();
    std::unique_ptr<Expression> ParseBinaryExpression();
    std::unique_ptr<Expression> ParsePrimaryExpression();
    std::unique_ptr<Expression> ParseIdentifierExpression();
    std::unique_ptr<Expression> ParseConstantExpression();
    std::unique_ptr<Expression> ParseBinaryExpressionRHS(int Precedence,
                                                         std::unique_ptr<Expression> LeftExpression);

    void InsertToSymbolTable(const std::string &SymbolName,
                             ComplexType SymType,
                             ValueType SymValue = ValueType());


  private:
    Lexer lexer;
    SymbolTableStack SymTabStack;
};



#endif