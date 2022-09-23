#ifndef LUNARTCC_AST_H
#define LUNARTCC_AST_H

#include <cassert>
#include <iostream>
#include <cstdint>
#include <list>
#include <variant>
#include <optional>
#include <memory>

#include "FrontEnd/Lexer/Token.hpp"
#include "FrontEnd/AST/Type.hpp"
#include "Utils/ErrorLogger.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/Value.hpp"

class Node
{
  public:
    virtual void ASTDump(unsigned tab = 0) { PrintLn("Node"); }
    virtual ~Node() {}

    virtual Value *IRCodegen(IRFactory *IRF);
};

class Statement : public Node
{
  public:
    void ASTDump(unsigned int tab = 0) override { PrintLn("Statement", tab); }
};

class Expression : public Node
{
  public:
    Expression() = default;
    Expression(ComplexType t) : ResultType(std::move(t)) {}
    Expression(Type::VariantKind vk) : ResultType(vk) {}

    ComplexType GetResultType() { return ResultType; }
    void SetResultType(ComplexType t) { ResultType = t; }

    void ASTDump(unsigned int tab = 0) override { PrintLn("Expression", tab); }

  protected:
    ComplexType ResultType;
};

class VariableDeclaration : public Statement
{
  public:
    std::string &GetName() { return Name; }
    void SetName(std::string &name) { Name = name; }

    ArrayType GetType() { return AType; }
    void SetType(ArrayType t) { AType = t; }

    VariableDeclaration(std::string &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(Ty, std::move(Dim))
    {}

    VariableDeclaration(std::string &Name, ArrayType Ty) : Name(Name), AType(Ty) {}

    void ASTDump(unsigned int tab = 0) override;
    
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    ArrayType AType;
};

class CompoundStatement : public Statement
{
    using DeclVec = std::vector<std::unique_ptr<VariableDeclaration>>;
    using StmtVec = std::vector<std::unique_ptr<Statement>>;

  public:
    DeclVec &GetDeclarations() { return Declarations; }
    void SetDeclarations(DeclVec &d) { Declarations = std::move(d); }
    void AddDeclarations(std::unique_ptr<VariableDeclaration> &d)
    {
        Declarations.push_back(std::move(d));
    }

    StmtVec &GetStatement() { return Statements; }
    void SetStatements(StmtVec &s) { Statements = std::move(s); }
    void AddStatements(std::unique_ptr<Statement> &s)
    {
        Statements.push_back(std::move(s));
    }

    CompoundStatement()                                     = delete;
    CompoundStatement(const CompoundStatement &)            = delete;
    CompoundStatement &operator=(const CompoundStatement &) = delete;
    CompoundStatement(CompoundStatement &&)                 = default;

    CompoundStatement(DeclVec &Decls, StmtVec &Stats)
        : Declarations(std::move(Decls)), Statements(std::move(Stats))
    {}

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    DeclVec Declarations;
    StmtVec Statements;
};

class ExpressionStatement : public Statement
{
  public:
    std::unique_ptr<Expression> &GetExpression() { return Expr; }
    void SetExpression(std::unique_ptr<Expression> e) { Expr = std::move(e); }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::unique_ptr<Expression> Expr;
};

class IfStatement : public Statement
{
  public:
    std::unique_ptr<Expression> &GetCondition() { return Condition; }
    void SetCondition(std::unique_ptr<Expression> c) { Condition = std::move(c); }

    std::unique_ptr<Statement> &GetIfBody() { return IfBody; }
    void SetIfBody(std::unique_ptr<Statement> iB) { IfBody = std::move(iB); }

    std::unique_ptr<Statement> &GetElseBody() { return ElseBody; }
    void SetElseBody(std::unique_ptr<Statement> eB) { ElseBody = std::move(eB); }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::unique_ptr<Expression> Condition;
    std::unique_ptr<Statement> IfBody;
    std::unique_ptr<Statement> ElseBody;
};

class WhileStatement : public Statement
{
  public:
    std::unique_ptr<Expression> &GetCondition() { return Condition; }
    void SetCondition(std::unique_ptr<Expression> c) { Condition = std::move(c); }

    std::unique_ptr<Statement> &GetBody() { return Body; }
    void SetBody(std::unique_ptr<Statement> b) { Body = std::move(b); }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::unique_ptr<Expression> Condition;
    std::unique_ptr<Statement> Body;
};

class ReturnStatement : public Statement
{
  public:
    std::unique_ptr<Expression> &GetCondition()
    {
        assert(HasValue() && "Must have a value to return it.");
        return ReturnValue.value();
    }
    void SetCondition(std::unique_ptr<Expression> v) { ReturnValue = std::move(v); }
    bool HasValue() { return ReturnValue.has_value(); }

    ReturnStatement() = default;
    ReturnStatement(std::unique_ptr<Expression> e) : ReturnValue(std::move(e)) {}

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;


  private:
    std::optional<std::unique_ptr<Expression>> ReturnValue;
};

class FunctionParameterDeclaration : public Statement
{
  public:
    std::string &GetName() { return Name; }
    void SetName(std::string &s) { Name = s; }

    Type GetType() { return Ty; }
    void SetType(Type t) { Ty = t; }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    Type Ty;
};

class FunctionDeclaration : public Statement
{
    using ParamVec = std::vector<std::unique_ptr<FunctionParameterDeclaration>>;

  public:
    FunctionType GetType() { return FuncType; }
    void SetType(FunctionType ft) { FuncType = ft; }

    std::string &GetName() { return Name; }
    void SetName(std::string &s) { Name = s; }

    ParamVec &GetArguments() { return Arguments; }
    void SetArguments(ParamVec &a) { Arguments = std::move(a); }
    void SetArguments(ParamVec &&a) { Arguments = std::move(a); }

    std::unique_ptr<CompoundStatement> &GetBody() { return Body; }
    void SetBody(std::unique_ptr<CompoundStatement> &cs) { Body = std::move(cs); }

    static FunctionType CreateType(const Type &t, const ParamVec &params);

    FunctionDeclaration() = delete;
    FunctionDeclaration(FunctionType FT,
                        std::string Name,
                        ParamVec &Args,
                        std::unique_ptr<CompoundStatement> &Body)
        : FuncType(FT), Name(Name), Arguments(std::move(Args)), Body(std::move(Body))
    {}

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    FunctionType FuncType;
    std::string Name;
    ParamVec Arguments;
    std::unique_ptr<CompoundStatement> Body;
};

class BinaryExpression : public Expression
{
    using ExprPtr = std::unique_ptr<Expression>;

  public:
    enum BinaryOperation {
        Assign,    // a = b
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        And,
        Not,
        Equal,
        Less,
        Greater,
        NotEqual,
        LogicalAnd
    };

    BinaryOperation GetOperationKind();

    Token GetOperation() { return Operation; }
    void SetOperation(Token op) { Operation = op; }

    ExprPtr &GetLeftExpr() { return Lhs; }
    void SetLeftExpr(ExprPtr &e) { Lhs = std::move(e); }

    ExprPtr &GetRightExpr() { return Rhs; }
    void SetRightExpr(ExprPtr &e) { Rhs = std::move(e); }

    bool IsCondition() { return GetOperationKind() >= Not; }

    BinaryExpression() = default;
    BinaryExpression(ExprPtr Left, Token Op, ExprPtr Right);

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Operation;
    std::unique_ptr<Expression> Lhs;
    std::unique_ptr<Expression> Rhs;
};

class CallExpression : public Expression
{
    using ExprVec = std::vector<std::unique_ptr<Expression>>;

  public:
    std::string &GetName() { return Name; }
    void SetName(std::string &n) { Name = n; }

    ExprVec &GetArguments() { return Arguments; }
    void SetArguments(ExprVec &A) { Arguments = std::move(A); }

    CallExpression(const std::string &Name, ExprVec &Args, ComplexType T)
        : Name(Name), Arguments(std::move(Args)), Expression(std::move(T))
    {}

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    ExprVec Arguments;
};

class ReferenceExpression : public Expression
{
  public:
    std::string &GetIdentifier() { return Identifier; }
    void SetIdentifier(std::string &id) { Identifier = id; }

    void SetLValueness(bool p) { IsLValue = p; }
    bool GetLValueness() const { return IsLValue; }

    ReferenceExpression(Token t) { Identifier = t.GetString(); }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    bool IsLValue {false};
    std::string Identifier;
};

class IntegerLiteralExpression : public Expression
{
  public:
    unsigned GetValue() { return IntValue; }
    void SetValue(unsigned v) { IntValue = v; }

    IntegerLiteralExpression() = delete;
    IntegerLiteralExpression(unsigned v) : IntValue(v)
    {
        SetResultType(ComplexType(Type::Int));
    }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    uint64_t IntValue;
};

class FloatLiteralExpression : public Expression
{
  public:
    double GetValue() { return FPValue; }
    void SetValue(double v) { FPValue = v; }

    FloatLiteralExpression() = delete;
    FloatLiteralExpression(double v) : FPValue(v)
    {
        SetResultType(ComplexType(Type::Double));
    }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    double FPValue;
};

class ArrayExpression : public Expression
{
    using ExprVec = std::vector<std::unique_ptr<Expression>>;

  public:
    Token &GetIdentifier() { return Identifier; }
    void SetIdentifier(Token &id) { Identifier = id; };

    ExprVec &GetIndexExpression() { return IndexExpression; }
    void SetIndexExpression(ExprVec &e) { IndexExpression = std::move(e); }

    void SetLValueness(bool p) { IsLValue = p; }
    bool GetLValueness() const { return IsLValue; }

    ArrayExpression(const Token &Id, ExprVec &IEs, ComplexType Ct = ComplexType())
        : Identifier(Id), IndexExpression(std::move(IEs))
    {
        ResultType = Ct;
    }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    bool IsLValue {false};
    Token Identifier;
    ExprVec IndexExpression;
};

class ImplicitCastExpression : public Expression
{
  public:
    ImplicitCastExpression(std::unique_ptr<Expression> e, Type::VariantKind rt)
        : CastableExpression(std::move(e)), Expression(rt)
    {}

    Type GetSourceType() { return CastableExpression->GetResultType(); }
    std::unique_ptr<Expression> &GetCastableExpression() { return CastableExpression; }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::unique_ptr<Expression> CastableExpression;
};

class TranslationUnit : public Statement
{
  public:
    std::vector<std::unique_ptr<Statement>> &GetDeclarations() { return Declarations; }
    void SetDeclarations(std::vector<std::unique_ptr<Statement>> s)
    {
        Declarations = std::move(s);
    }
    void AddDeclaration(std::unique_ptr<Statement> s)
    {
        Declarations.push_back(std::move(s));
    }

    TranslationUnit() = default;
    TranslationUnit(std::vector<std::unique_ptr<Statement>> s)
        : Declarations(std::move(s))
    {}

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::vector<std::unique_ptr<Statement>> Declarations;
};



#endif
