#pragma once

#include <cassert>
#include <stack>
#include <memory>
#include <map>
#include "FrontEnd/Lexer/Lexer.hpp"
#include "FrontEnd/AST/Type.hpp"
#include "FrontEnd/Parser/SymbolTable.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "Utils/DiagnosticPrinter.hpp"

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
class ContinueStatement;
class WhileStatement;
class DoWhileStatement;
class ForStatement;
class ReturnStatement;
class CompoundStatement;
class ExpressionStatement;

class SymbolTableStack;
class IRFactory;

class Parser
{
  public:
    using ExprPtr          = std::unique_ptr<Expression>;
    using StmtPtr          = std::unique_ptr<Statement>;
    using FuncParamDeclPtr = std::unique_ptr<FunctionParameterDeclaration>;

    static constexpr const unsigned EmptyDimension = ~0;

  public:
    std::unique_ptr<Node> Parse();

    Parser() = delete;
    Parser(std::vector<std::string> &s, IRFactory *irf, DiagnosticPrinter &DP)
        : lexer(s), IRF(irf), DiagPrinter(DP)
    {}

    Token Lex() { return lexer.Lex(); }
    Token Expect(Token::TokenKind Token);

    Token GetCurrentToken() { return lexer.GetCurrentToken(); }
    Token::TokenKind GetCurrentTokenKind() { return lexer.GetCurrentToken().GetKind(); }

    unsigned ParseIntegerConstant();
    double ParseRealConstant();

    unsigned ParseQualifiers();
    Type ParseType(Token::TokenKind tk);
    void ParseArrayDimensions(Type &type);

    bool IsTypeSpecifier(Token T);
    bool IsReturnTypeSpecifier(Token T);
    bool IsQualifer(Token::TokenKind tk);
    bool IsQualifedType(Token T);

    bool IsUserDefinedType(const std::string &Name);
    Type GetUserDefinedType(const std::string &Name);
    std::vector<Token> GetUserDefinedTypeMembers(std::string Name);

    std::unique_ptr<Node> ParseTranslationUnit();
    std::unique_ptr<Node> ParseExternalDeclaration();
    std::unique_ptr<FunctionDeclaration> ParseFunctionDeclaration(const Type &ReturnType,
                                                                  const Token &Name);
    std::unique_ptr<VariableDeclaration> ParseVariableDeclaration(Type type);
    std::vector<std::unique_ptr<VariableDeclaration>> ParseVariableDeclarationList();

    FuncParamDeclPtr ParseParameterDeclaration();
    std::vector<FuncParamDeclPtr> ParseParameterList(bool &HasVarArg);
    std::unique_ptr<MemberDeclaration> ParseMemberDeclaration();
    std::unique_ptr<StructDeclaration> ParseStructDeclaration(unsigned Qualifiers = 0);
    std::unique_ptr<EnumDeclaration> ParseEnumDeclaration(unsigned Qualifiers);

    Type ParseTypeSpecifier();
    Node ParseReturnTypeSpecifier();

    std::unique_ptr<Statement> ParseStatement();
    std::unique_ptr<IfStatement> ParseIfStatement();
    std::unique_ptr<SwitchStatement> ParseSwitchStatement();
    std::unique_ptr<BreakStatement> ParseBreakStatement();
    std::unique_ptr<ContinueStatement> ParseContinueStatement();
    std::unique_ptr<WhileStatement> ParseWhileStatement();
    std::unique_ptr<DoWhileStatement> ParseDoWhileStatement();
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
    ExprPtr ParseInitializerListExpression(const Type &ExpectedType);
    ExprPtr ParseIdentifierExpression();
    ExprPtr ParseConstantExpression();
    ExprPtr ParseTernaryExpression(ExprPtr Condition);
    ExprPtr ParseBinaryExpressionRHS(int Precedence,
                                     std::unique_ptr<Expression> LeftExpression);

    void InsertToSymbolTable(const Token &SymbolName,
                             const Type &SymType,
                             const bool ToGlobal = false,
                             ValueType SymValue  = ValueType());

    DiagnosticPrinter &GetDiagPrinter() { return DiagPrinter; }

  private:
    Lexer lexer;
    SymbolTableStack SymTabStack;
    IRFactory *IRF;

    // Type name to type, and the list of names for the struct field
    // TODO: write example to explain it.
    std::map<std::string, std::tuple<Type, std::vector<Token>>> UserDefinedTypes;

    /// Mapping identifiers to types. Eg: "typedef int i32" -> {"i32", Type::Int}
    std::map<std::string, Type> TypeDefinitions;

    /// Used for determining if implicit cast need or not in return statements
    Type CurrentFuncRetType {Type::Invalid};

    /// The amount of return seen in the current function.
    unsigned ReturnNumber = 0;

    DiagnosticPrinter &DiagPrinter;
};
