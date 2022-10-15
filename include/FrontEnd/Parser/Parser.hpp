#ifndef LUNARTCC_PARSER_H
#define LUNARTCC_PARSER_H

#include <cassert>
#include <stack>
#include <memory>
#include "FrontEnd/Lexer/Lexer.hpp"
#include "FrontEnd/AST/Type.hpp"
#include "FrontEnd/Parser/SymbolTable.hpp"

class Node;
class Expression;

class Declaration;
class VariableDeclaration;
class FunctionDeclaration;
class FunctionParameterDeclaration;

class Statement;
class IfStatement;
class WhileStatement;
class ForStatement;
class ReturnStatement;
class CompoundStatement;
class ExpressionStatement;

class SymbolTableStack;
class IRFactory;

class Parser
{
  public:
    std::unique_ptr<Node> Parse();

    Parser() = delete;
    Parser(std::vector<std::string> &s, IRFactory *irf) : lexer(s), IRF(irf) {}

    Token Lex() { return lexer.Lex(); }
    Token Expect(Token::TokenKind Token);

    Token::TokenKind GetCurrentTokenKind() { return lexer.GetCurrentToken().GetKind(); }

    unsigned ParseIntegerConstant();
    double ParseRealConstant();

    std::unique_ptr<Node> ParseTranslationUnit();
    std::unique_ptr<Node> ParseExternalDeclaration();
    std::unique_ptr<FunctionDeclaration> ParseFunctionDeclaration(const Type &ReturnType,
                                                                  const Token &Name);
    std::unique_ptr<VariableDeclaration> ParseVariableDeclaration();
    std::unique_ptr<FunctionParameterDeclaration> ParseParameterDeclaration();
    std::vector<std::unique_ptr<FunctionParameterDeclaration>> ParseParameterList();

    Type ParseTypeSpecifier();
    Node ParseReturnTypeSpecifier();

    std::unique_ptr<Statement> ParseStatement();
    std::unique_ptr<IfStatement> ParseIfStatement();
    std::unique_ptr<WhileStatement> ParseWhileStatement();
    std::unique_ptr<ForStatement> ParseForStatement();
    std::unique_ptr<ReturnStatement> ParseReturnStatement();
    std::unique_ptr<CompoundStatement> ParseCompoundStatement();
    std::unique_ptr<ExpressionStatement> ParseExpressionStatement();

    std::unique_ptr<Expression> ParseExpression();
    std::unique_ptr<Expression> ParseBinaryExpression();
    std::unique_ptr<Expression> ParsePrimaryExpression();
    std::unique_ptr<Expression> ParseIdentifierExpression();
    std::unique_ptr<Expression> ParseConstantExpression();
    std::unique_ptr<Expression>
        ParseBinaryExpressionRHS(int Precedence,
                                 std::unique_ptr<Expression> LeftExpression);

    void InsertToSymbolTable(const std::string &SymbolName,
                             ComplexType SymType,
                             const bool ToGlobal = false,
                             ValueType SymValue  = ValueType());

  private:
    Lexer lexer;
    SymbolTableStack SymTabStack;
    IRFactory *IRF;

    /// Used for determining if implicit cast need or not in return statements
    Type CurrentFuncRetType {Type::Invalid};
};


#endif
