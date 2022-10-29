#pragma once

#include <cassert>
#include <stack>
#include <memory>
#include <map>
#include "FrontEnd/Lexer/Lexer.hpp"
#include "FrontEnd/AST/Type.hpp"
#include "FrontEnd/Parser/SymbolTable.hpp"

class Node;
class Expression;

class Declaration;
class VariableDeclaration;
class MemberDeclaration;
class StructDeclaration;
class EnumDeclaration;
class FunctionDeclaration;
class FunctionParameterDeclaration;

class Statement;
class IfStatement;
class SwitchStatement;
class BreakStatement;
class WhileStatement;
class ForStatement;
class ReturnStatement;
class CompoundStatement;
class ExpressionStatement;

class SymbolTableStack;
class IRFactory;

class Parser
{
    using ExprPtr = std::unique_ptr<Expression>;

  public:
    std::unique_ptr<Node> Parse();

    Parser() = delete;
    Parser(std::vector<std::string> &s, IRFactory *irf) : lexer(s), IRF(irf) {}

    Token Lex() { return lexer.Lex(); }
    Token Expect(Token::TokenKind Token);

    Token GetCurrentToken() { return lexer.GetCurrentToken(); }
    Token::TokenKind GetCurrentTokenKind() { return lexer.GetCurrentToken().GetKind(); }

    unsigned ParseIntegerConstant();
    double ParseRealConstant();

    unsigned ParseQualifiers();
    Type ParseType(Token::TokenKind tk);
    bool IsTypeSpecifier(Token T);
    bool IsReturnTypeSpecifier(Token T);
    bool IsQualifer(Token::TokenKind tk);

    bool IsUserDefinedType(std::string Name);
    Type GetUserDefinedType(std::string Name);

    std::unique_ptr<Node> ParseTranslationUnit();
    std::unique_ptr<Node> ParseExternalDeclaration();
    std::unique_ptr<FunctionDeclaration> ParseFunctionDeclaration(const Type &ReturnType,
                                                                  const Token &Name);
    std::unique_ptr<VariableDeclaration> ParseVariableDeclaration();
    std::unique_ptr<FunctionParameterDeclaration> ParseParameterDeclaration();
    std::vector<std::unique_ptr<FunctionParameterDeclaration>> ParseParameterList();
    std::unique_ptr<MemberDeclaration> ParseMemberDeclaration();
    std::unique_ptr<StructDeclaration> ParseStructDeclaration(unsigned Qualifiers = 0);
    std::unique_ptr<EnumDeclaration> ParseEnumDeclaration();

    Type ParseTypeSpecifier();
    Node ParseReturnTypeSpecifier();

    std::unique_ptr<Statement> ParseStatement();
    std::unique_ptr<IfStatement> ParseIfStatement();
    std::unique_ptr<SwitchStatement> ParseSwitchStatement();
    std::unique_ptr<BreakStatement> ParseBreakStatement();
    std::unique_ptr<WhileStatement> ParseWhileStatement();
    std::unique_ptr<ForStatement> ParseForStatement();
    std::unique_ptr<ReturnStatement> ParseReturnStatement();
    std::unique_ptr<CompoundStatement> ParseCompoundStatement();
    std::unique_ptr<ExpressionStatement> ParseExpressionStatement();

    ExprPtr ParseExpression();
    ExprPtr ParseCallExpression(Token ID);
    ExprPtr ParseArrayExpression(ExprPtr Base);
    ExprPtr ParsePostFixExpression();
    ExprPtr ParseUnaryExpression();
    ExprPtr ParseBinaryExpression();
    ExprPtr ParsePrimaryExpression();
    ExprPtr ParseInitializerListExpression();
    ExprPtr ParseIdentifierExpression();
    ExprPtr ParseConstantExpression();
    ExprPtr ParseBinaryExpressionRHS(int Precedence,
                                     std::unique_ptr<Expression> LeftExpression);

    void InsertToSymbolTable(const std::string &SymbolName,
                             Type SymType,
                             const bool ToGlobal = false,
                             ValueType SymValue  = ValueType());

  private:
    Lexer lexer;
    SymbolTableStack SymTabStack;
    IRFactory *IRF;

    // Type name to type, and the list of names for the struct field
    std::map<std::string, std::tuple<Type, std::vector<std::string>>> UserDefinedTypes;

    /// Mapping identifiers to types. Eg: "typedef int i32" -> {"i32", Type::Int}
    std::map<std::string, Type> TypeDefinitions;

    /// Used for determining if implicit cast need or not in return statements
    Type CurrentFuncRetType {Type::Invalid};
};
