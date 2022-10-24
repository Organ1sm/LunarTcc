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
#include "FrontEnd/Parser/Parser.hpp"
#include "Utils/ErrorLogger.hpp"

class Value;
class IRFactory;

class Node
{
  public:
    virtual ~Node() {}

    virtual void ASTDump(unsigned tab = 0) { PrintLn("Node"); }
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
    Expression(Type t) : ResultType(std::move(t)) {}
    Expression(Type::VariantKind vk) : ResultType(vk) {}

    Type &GetResultType() { return ResultType; }
    void SetResultType(Type t) { ResultType = t; }

    void SetLValueness(bool p) { IsLValue = p; }
    bool GetLValueness() { return IsLValue; }

    void ASTDump(unsigned int tab = 0) override { PrintLn("Expression", tab); }

  protected:
    bool IsLValue {false};
    Type ResultType;
};

class VariableDeclaration : public Statement
{
  public:
    VariableDeclaration(std::string &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(Ty, std::move(Dim))
    {}

    VariableDeclaration(std::string &Name, Type Ty) : Name(Name), AType(Ty) {}

    std::unique_ptr<Expression> &GetInitExpr() { return Init; }
    void SetInitExpr(std::unique_ptr<Expression> e) { Init = std::move(e); }

    std::string &GetName() { return Name; }
    void SetName(std::string &name) { Name = name; }

    Type GetType() { return AType; }
    void SetType(Type t) { AType = t; }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    Type AType;
    std::unique_ptr<Expression> Init {nullptr};
};

class MemberDeclaration : public Statement
{
  public:
    MemberDeclaration() = default;
    MemberDeclaration(std::string &Name, Type Ty) : Name(Name), AType(Ty) {}
    MemberDeclaration(std::string &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(Ty, std::move(Dim))
    {}

    std::string &GetName() { return Name; }
    void SetName(std::string &s) { Name = s; }

    Type GetType() { return AType; }
    void SetType(Type t) { AType = t; }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    Type AType;
};

class EnumDeclaration : public Statement
{
  public:
    using EnumList = std::vector<std::pair<std::string, int>>;

  public:
    EnumDeclaration(Type &BT, EnumList Enumerators)
        : BaseType(BT), Enumerators(std::move(Enumerators))
    {}

    EnumDeclaration(EnumList Enumerators) : Enumerators(std::move(Enumerators)) {}

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Type BaseType {Type::Int};
    EnumList Enumerators;
};

class StructDeclaration : public Statement
{
    using MemberDeclVec = std::vector<std::unique_ptr<MemberDeclaration>>;

  public:
    StructDeclaration() = default;
    StructDeclaration(std::string &Name, MemberDeclVec &M, Type &StructType)
        : Name(Name), Members(std::move(M)), SType(StructType)
    {}

    std::string &GetName() { return Name; }
    void SetName(std::string &s) { Name = s; }

    MemberDeclVec &GetMembers() { return Members; }
    void SetType(MemberDeclVec m) { Members = std::move(m); }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Type SType;
    std::string Name;
    MemberDeclVec Members;
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

class SwitchStatement : public Statement
{
  public:
    using StmtsVec     = std::vector<std::unique_ptr<Statement>>;
    using CasesDataVec = std::vector<std::pair<int, StmtsVec>>;

    std::unique_ptr<Expression> &GetCondition() { return Condition; }
    void SetCondition(std::unique_ptr<Expression> c) { Condition = std::move(c); }

    CasesDataVec &GetCaseBodies() { return Cases; }
    void SetCasesBodies(CasesDataVec c) { Cases = std::move(c); }

    StmtsVec &GetDefaultBody() { return DefaultBody; }
    void SetDefaultBody(StmtsVec db) { DefaultBody = std::move(db); }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::unique_ptr<Expression> Condition;
    CasesDataVec Cases;
    StmtsVec DefaultBody;
};

class BreakStatement : public Statement
{
  public:
    BreakStatement() = default;

    void ASTDump(unsigned tab = 0) override { PrintLn("BreakStatement", tab); }
    Value *IRCodegen(IRFactory *IRF) override { return nullptr; }
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

class ForStatement : public Statement
{
  public:
    std::unique_ptr<Expression> &GetInit() { return Init; }
    void SetInit(std::unique_ptr<Expression> i) { Init = std::move(i); }

    std::unique_ptr<Expression> &GetCondition() { return Condition; }
    void SetCondition(std::unique_ptr<Expression> c) { Condition = std::move(c); }

    std::unique_ptr<Expression> &GetIncrement() { return Increment; }
    void SetIncrement(std::unique_ptr<Expression> i) { Increment = std::move(i); }

    std::unique_ptr<Statement> &GetBody() { return Body; }
    void SetBody(std::unique_ptr<Statement> b) { Body = std::move(b); }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::unique_ptr<Expression> Init;
    std::unique_ptr<Expression> Condition;
    std::unique_ptr<Expression> Increment;
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
    Type GetType() { return FuncType; }
    void SetType(Type ft) { FuncType = ft; }

    std::string &GetName() { return Name; }
    void SetName(std::string &s) { Name = s; }

    ParamVec &GetArguments() { return Arguments; }
    void SetArguments(ParamVec &a) { Arguments = std::move(a); }
    void SetArguments(ParamVec &&a) { Arguments = std::move(a); }

    std::unique_ptr<CompoundStatement> &GetBody() { return Body; }
    void SetBody(std::unique_ptr<CompoundStatement> &cs) { Body = std::move(cs); }

    static Type CreateType(const Type &t, const ParamVec &params);

    FunctionDeclaration() = delete;
    FunctionDeclaration(Type FT,
                        std::string Name,
                        ParamVec &Args,
                        std::unique_ptr<CompoundStatement> &Body)
        : FuncType(FT), Name(Name), Arguments(std::move(Args)), Body(std::move(Body))
    {}

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Type FuncType;
    std::string Name;
    ParamVec Arguments;
    std::unique_ptr<CompoundStatement> Body;
};

class StructMemberReference : public Expression
{
    using ExprPtr = std::unique_ptr<Expression>;

  public:
    StructMemberReference() {}
    StructMemberReference(ExprPtr Expr, std::string Id, std::size_t Idx);

    std::string GetMemberId() { return MemberIdentifier; }
    void SetMemberId(std::string Id) { MemberIdentifier = Id; }

    ExprPtr &GetExpr() { return StructTypedExpression; }
    void SetExpr(ExprPtr &e) { StructTypedExpression = std::move(e); }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr StructTypedExpression;
    std::string MemberIdentifier;
    std::size_t MemberIndex;
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
    ExprPtr Lhs;
    ExprPtr Rhs;
};

class UnaryExpression : public Expression
{
    using ExprPtr = std::unique_ptr<Expression>;

  public:
    enum UnaryOperation { DeRef, PostIncrement, PostDecrement };

    UnaryExpression() = default;
    UnaryExpression(Token Op, ExprPtr E);

    UnaryOperation GetOperationKind();

    Token GetOperation() { return Operation; }
    void SetOperation(Token op) { Operation = op; }

    ExprPtr &GetPtr() { return Expr; }
    void SetExpr(ExprPtr &e) { Expr = std::move(e); }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Operation;
    ExprPtr Expr;
};

class CallExpression : public Expression
{
    using ExprVec = std::vector<std::unique_ptr<Expression>>;

  public:
    std::string &GetName() { return Name; }
    void SetName(std::string &n) { Name = n; }

    ExprVec &GetArguments() { return Arguments; }
    void SetArguments(ExprVec &A) { Arguments = std::move(A); }

    CallExpression(const std::string &Name, ExprVec &Args, Type T)
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

    ReferenceExpression(Token t) { Identifier = t.GetString(); }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Identifier;
};

class IntegerLiteralExpression : public Expression
{
  public:
    unsigned GetValue() { return IntValue; }
    int64_t GetSIntValue() const { return IntValue; }
    void SetValue(unsigned v) { IntValue = v; }

    IntegerLiteralExpression() = delete;
    IntegerLiteralExpression(unsigned v) : IntValue(v) { SetResultType(Type(Type::Int)); }

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
    FloatLiteralExpression(double v) : FPValue(v) { SetResultType(Type(Type::Double)); }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    double FPValue;
};

class ArrayExpression : public Expression
{
    using ExprPtr = std::unique_ptr<Expression>;

  public:
    ExprPtr &GetIndexExpression() { return IndexExpression; }
    void SetIndexExpression(ExprPtr &e) { IndexExpression = std::move(e); }

    ArrayExpression(ExprPtr &Base, ExprPtr &IEs, Type Ct = Type())
        : BaseExpression(std::move(Base)), IndexExpression(std::move(IEs))
    {
        ResultType = Ct;
    }

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr BaseExpression;
    ExprPtr IndexExpression;
};

class ImplicitCastExpression : public Expression
{
  public:
    ImplicitCastExpression(std::unique_ptr<Expression> e, Type t)
        : CastableExpression(std::move(e)), Expression(t.GetTypeVariant())
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
