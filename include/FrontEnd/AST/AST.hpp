#pragma once

#include <cassert>
#include <iostream>
#include <cstdint>
#include <list>
#include <variant>
#include <optional>
#include <memory>
#include <vector>

#include "FrontEnd/Lexer/Token.hpp"
#include "FrontEnd/AST/Type.hpp"
#include "FrontEnd/Parser/Parser.hpp"
#include "Utils/ErrorLogger.hpp"

class Value;
class IRFactory;
class ASTVisitor;

using ExprPtr = std::unique_ptr<Expression>;
using StmtPtr = std::unique_ptr<Statement>;

using ExprPtrVec = std::vector<ExprPtr>;
using StmtPtrVec = std::vector<StmtPtr>;

class Node
{
  public:
    virtual ~Node() {}

    virtual void Accept(ASTVisitor *Visitor) const { return; }
    virtual Value *IRCodegen(IRFactory *IRF);
};

class Statement : public Node
{
  public:
    enum StmtInfo { None = 0, Return = 1 };

    void AddInfo(unsigned Bit) { InfoBits |= Bit; }
    bool IsRet() { return !!(InfoBits & Return); }

  private:
    unsigned InfoBits = 0;
};

class Expression : public Node
{
  public:
    Expression() = default;
    Expression(Type t) : ResultType(std::move(t)) {}
    Expression(Type::VariantKind vk) : ResultType(vk) {}

    Type &GetResultType() { return ResultType; }
    const Type &GetResultType() const { return ResultType; }
    void SetResultType(Type t) { ResultType = t; }

    void SetLValueness(bool p) { IsLValue = p; }
    bool GetLValueness() { return IsLValue; }

  protected:
    bool IsLValue {false};
    Type ResultType;
};

class VariableDeclaration : public Statement
{
  public:
    VariableDeclaration(std::string &Name, Type Ty) : Name(Name), AType(Ty) {}
    VariableDeclaration(std::string &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(Ty, std::move(Dim))
    {}

    VariableDeclaration(std::string &Name, Type Ty, ExprPtr E)
        : Name(Name), AType(Ty), Init(std::move(E))
    {}

    ExprPtr &GetInitExpr() { return Init; }
    const ExprPtr &GetInitExpr() const { return Init; }
    void SetInitExpr(ExprPtr e) { Init = std::move(e); }

    const std::string &GetName() const { return Name; }
    void SetName(std::string &name) { Name = name; }

    Type GetType() const { return AType; }
    void SetType(Type t) { AType = t; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    Type AType;
    ExprPtr Init {nullptr};
};

class MemberDeclaration : public Statement
{
  public:
    MemberDeclaration() = default;
    MemberDeclaration(std::string &Name, Type Ty) : Name(Name), AType(Ty) {}
    MemberDeclaration(std::string &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(Ty, std::move(Dim))
    {}

    const std::string &GetName() const { return Name; }
    void SetName(std::string &s) { Name = s; }

    Type GetType() const { return AType; }
    void SetType(Type t) { AType = t; }

    void Accept(ASTVisitor *Visitor) const override;
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

    Type GetBaseType() const { return BaseType; }
    const EnumList &GetEnumerators() const { return Enumerators; }

    void Accept(ASTVisitor *Visitor) const override;
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

    const std::string &GetName() const { return Name; }
    void SetName(std::string &s) { Name = s; }

    const MemberDeclVec &GetMembers() const { return Members; }
    void SetType(MemberDeclVec m) { Members = std::move(m); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Type SType;
    std::string Name;
    MemberDeclVec Members;
};

class CompoundStatement : public Statement
{
  public:
    CompoundStatement()                                     = delete;
    CompoundStatement(const CompoundStatement &)            = delete;
    CompoundStatement &operator=(const CompoundStatement &) = delete;
    CompoundStatement(CompoundStatement &&)                 = default;
    CompoundStatement(StmtPtrVec &Stats) : Statements(std::move(Stats)) {}

    StmtPtrVec &GetStatements() { return Statements; }
    const StmtPtrVec &GetStatements() const { return Statements; }
    void SetStatements(StmtPtrVec &s) { Statements = std::move(s); }
    void AddStatements(StmtPtr &s) { Statements.push_back(std::move(s)); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    StmtPtrVec Statements;
};

class ExpressionStatement : public Statement
{
  public:
    const ExprPtr &GetExpression() const { return Expr; }
    void SetExpression(ExprPtr e) { Expr = std::move(e); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Expr;
};

class IfStatement : public Statement
{
  public:
    const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr c) { Condition = std::move(c); }

    const StmtPtr &GetIfBody() const { return IfBody; }
    void SetIfBody(StmtPtr iB) { IfBody = std::move(iB); }

    const StmtPtr &GetElseBody() const { return ElseBody; }
    void SetElseBody(StmtPtr eB) { ElseBody = std::move(eB); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Condition;
    StmtPtr IfBody;
    StmtPtr ElseBody;
};

class SwitchStatement : public Statement
{
  public:
    using CasesDataVec = std::vector<std::pair<int, StmtPtrVec>>;

    const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr c) { Condition = std::move(c); }

    const CasesDataVec &GetCaseBodies() const { return Cases; }
    void SetCasesBodies(CasesDataVec c) { Cases = std::move(c); }

    const StmtPtrVec &GetDefaultBody() const { return DefaultBody; }
    void SetDefaultBody(StmtPtrVec db) { DefaultBody = std::move(db); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Condition;
    CasesDataVec Cases;
    StmtPtrVec DefaultBody;
};

class BreakStatement : public Statement
{
  public:
    BreakStatement() = default;

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;
};

class ContinueStatement : public Statement
{
  public:
    ContinueStatement() = default;

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;
};

class WhileStatement : public Statement
{
  public:
    const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr c) { Condition = std::move(c); }

    const StmtPtr &GetBody() const { return Body; }
    void SetBody(StmtPtr b) { Body = std::move(b); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Condition;
    StmtPtr Body;
};

class ForStatement : public Statement
{
  public:
    const StmtPtr &GetVarDecl() const { return VarDecl; }
    void SetVarDecl(StmtPtr VD) { VarDecl = std::move(VD); }

    const ExprPtr &GetInit() const { return Init; }
    void SetInit(ExprPtr i) { Init = std::move(i); }

    const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr c) { Condition = std::move(c); }

    const ExprPtr &GetIncrement() const { return Increment; }
    void SetIncrement(ExprPtr i) { Increment = std::move(i); }

    const StmtPtr &GetBody() const { return Body; }
    void SetBody(StmtPtr b) { Body = std::move(b); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    StmtPtr VarDecl {nullptr};
    ExprPtr Init {nullptr};
    ExprPtr Condition;
    ExprPtr Increment;
    StmtPtr Body;
};

class ReturnStatement : public Statement
{
  public:
    ExprPtr &GetReturnVal()
    {
        assert(HasValue() && "Must have a value to return it.");
        return ReturnValue.value();
    }

    const ExprPtr &GetReturnVal() const
    {
        assert(HasValue() && "Must have a value to return it.");
        return ReturnValue.value();
    }

    void SetReturnVal(ExprPtr v) { ReturnValue = std::move(v); }
    bool HasValue() const { return ReturnValue.has_value(); }

    ReturnStatement() { AddInfo(Statement::Return); };
    ReturnStatement(ExprPtr e) : ReturnValue(std::move(e)) { AddInfo(Statement::Return); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::optional<ExprPtr> ReturnValue;
};

class FunctionParameterDeclaration : public Statement
{
  public:
    const std::string &GetName() const { return Name; }
    void SetName(std::string &s) { Name = s; }

    Type GetType() const { return Ty; }
    void SetType(Type t) { Ty = t; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    Type Ty;
};

class FunctionDeclaration : public Statement
{
    using ParamVec = std::vector<std::unique_ptr<FunctionParameterDeclaration>>;

  public:
    FunctionDeclaration() = delete;
    FunctionDeclaration(Type FT,
                        std::string Name,
                        ParamVec &Args,
                        std::unique_ptr<CompoundStatement> &Body,
                        unsigned RetNum)
        : FuncType(FT), Name(Name), Arguments(std::move(Args)), Body(std::move(Body)),
          ReturnNumber(RetNum)
    {}

    Type GetType() const { return FuncType; }
    void SetType(Type ft) { FuncType = ft; }

    const std::string &GetName() const { return Name; }
    void SetName(std::string &s) { Name = s; }

    const ParamVec &GetArguments() const { return Arguments; }
    void SetArguments(ParamVec &a) { Arguments = std::move(a); }
    void SetArguments(ParamVec &&a) { Arguments = std::move(a); }

    const std::unique_ptr<CompoundStatement> &GetBody() const { return Body; }
    void SetBody(std::unique_ptr<CompoundStatement> &cs) { Body = std::move(cs); }

    static Type CreateType(const Type &t, const ParamVec &params);

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    unsigned ReturnNumber;
    Type FuncType;
    std::string Name;
    ParamVec Arguments;
    std::unique_ptr<CompoundStatement> Body;
};

class StructMemberReference : public Expression
{
  public:
    StructMemberReference() {}
    StructMemberReference(ExprPtr Expr, std::string Id, std::size_t Idx);

    std::string GetMemberId() const { return MemberIdentifier; }
    void SetMemberId(std::string Id) { MemberIdentifier = Id; }

    const ExprPtr &GetExpr() const { return StructTypedExpression; }
    void SetExpr(ExprPtr &e) { StructTypedExpression = std::move(e); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr StructTypedExpression;
    std::string MemberIdentifier;
    std::size_t MemberIndex;
};

class StructInitExpression : public Expression
{
  public:
    using UintList = std::vector<unsigned>;

  public:
    StructInitExpression() {}
    StructInitExpression(Type ResultType, ExprPtrVec InitValues, UintList InitOrder)
        : MemberOrdering(std::move(InitOrder)), InitValues(std::move(InitValues))
    {
        this->ResultType = ResultType;
    }

    const UintList &GetMemberOrdering() const { return MemberOrdering; }
    void SetMemberOrdering(UintList UL) { MemberOrdering = UL; }

    const ExprPtrVec &GetInitList() const { return InitValues; }
    void SetInitList(ExprPtrVec &IVS) { InitValues = std::move(IVS); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    UintList MemberOrdering;
    ExprPtrVec InitValues;
};


class BinaryExpression : public Expression
{
  public:
    enum BinaryOperation {
        Assign,       // a = b
        AddAssign,    // a += b
        SubAssign,    // a -= b
        MulAssign,    // a *= b
        DivAssign,    // a /= b
        LSL,          // a << 1
        LSR,          // a >> 2
        Add,
        Sub,
        Mul,
        Div,
        DivU,
        Mod,
        ModU,
        And,
        Xor,
        Not,
        Equal,
        Less,
        Greater,
        LessEqual,
        GreaterEqual,
        NotEqual,
        LogicalAnd
    };

    BinaryOperation GetOperationKind();

    Token GetOperation() const { return Operation; }
    void SetOperation(Token op) { Operation = op; }

    const ExprPtr &GetLeftExpr() const { return Lhs; }
    void SetLeftExpr(ExprPtr &e) { Lhs = std::move(e); }

    const ExprPtr &GetRightExpr() const { return Rhs; }
    void SetRightExpr(ExprPtr &e) { Rhs = std::move(e); }

    bool IsCondition() { return GetOperationKind() >= Not; }

    BinaryExpression() = default;
    BinaryExpression(ExprPtr Left, Token Op, ExprPtr Right);

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Operation;
    ExprPtr Lhs;
    ExprPtr Rhs;
};

class UnaryExpression : public Expression
{
  public:
    enum UnaryOperation {
        Address,
        DeRef,
        Minus,
        Not,
        PostIncrement,
        PostDecrement,
        PreIncrement,
        PreDecrement
    };

    UnaryExpression() = default;
    UnaryExpression(Token Op, ExprPtr E, bool PostFix = false);

    UnaryOperation GetOperationKind();

    Token GetOperation() const { return Operation; }
    void SetOperation(Token op) { Operation = op; }

    const ExprPtr &GetExpr() const { return Expr; }
    void SetExpr(ExprPtr &e) { Expr = std::move(e); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    bool IsPostFix {false};
    Token Operation;
    ExprPtr Expr;
};

class TernaryExpression : public Expression
{
  public:
    TernaryExpression() = default;
    TernaryExpression(ExprPtr &Cond, ExprPtr &True, ExprPtr &False)
        : Condition(std::move(Cond)), ExprIfTrue(std::move(True)),
          ExprIfFalse(std::move(False))
    {
        ResultType = ExprIfTrue->GetResultType();
    }

    const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr &e) { Condition = std::move(e); }

    const ExprPtr &GetExprIfTrue() const { return ExprIfTrue; }
    void SetExprIfTrue(ExprPtr &e) { ExprIfTrue = std::move(e); }

    const ExprPtr &GetExprIfFalse() const { return ExprIfFalse; }
    void SetExprIfFalse(ExprPtr &e) { ExprIfFalse = std::move(e); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Condition;
    ExprPtr ExprIfTrue;
    ExprPtr ExprIfFalse;
};

class CallExpression : public Expression
{
  public:
    CallExpression(const std::string &Name, ExprPtrVec &Args, Type T)
        : Name(Name), Arguments(std::move(Args)), Expression(std::move(T))
    {}

    const std::string &GetName() const { return Name; }
    void SetName(std::string &n) { Name = n; }

    const ExprPtrVec &GetArguments() const { return Arguments; }
    void SetArguments(ExprPtrVec &A) { Arguments = std::move(A); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Name;
    ExprPtrVec Arguments;
};

class ReferenceExpression : public Expression
{
  public:
    ReferenceExpression(Token t) { Identifier = t.GetString(); }

    std::string &GetIdentifier() { return Identifier; }
    const std::string &GetIdentifier() const { return Identifier; }
    void SetIdentifier(std::string &id) { Identifier = id; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string Identifier;
};

class IntegerLiteralExpression : public Expression
{
  public:
    IntegerLiteralExpression() = delete;
    IntegerLiteralExpression(uint64_t v) : IntValue(v) { SetResultType(Type(Type::Int)); }

    int64_t GetValue() { return IntValue; }
    void SetValue(uint64_t v) { IntValue = v; }

    int64_t GetSIntValue() const { return IntValue; }
    uint64_t GetUIntValue() const { return IntValue; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    uint64_t IntValue;
};

class FloatLiteralExpression : public Expression
{
  public:
    FloatLiteralExpression() = delete;
    FloatLiteralExpression(double v) : FPValue(v) { SetResultType(Type(Type::Double)); }

    double GetValue() const { return FPValue; }
    void SetValue(double v) { FPValue = v; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    double FPValue;
};

class ArrayExpression : public Expression
{
  public:
    ArrayExpression(ExprPtr &Base, ExprPtr &IEs, Type Ct = Type())
        : BaseExpression(std::move(Base)), IndexExpression(std::move(IEs))
    {
        ResultType = Ct;
    }

    const ExprPtr &GetIndexExpression() const { return IndexExpression; }
    void SetIndexExpression(ExprPtr &e) { IndexExpression = std::move(e); }

    const ExprPtr &GetBase() const { return BaseExpression; }
    void SetBaseExpression(ExprPtr &e) { BaseExpression = std::move(e); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr BaseExpression;
    ExprPtr IndexExpression;
};

class ImplicitCastExpression : public Expression
{
  public:
    ImplicitCastExpression(ExprPtr e, Type t)
        : CastableExpression(std::move(e)), Expression(t)
    {}

    Type GetSourceType() { return CastableExpression->GetResultType(); }
    ExprPtr &GetCastableExpression() { return CastableExpression; }
    ExprPtr const &GetCastableExpression() const { return CastableExpression; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr CastableExpression;
};

class InitializerListExpression : public Expression
{
  public:
    InitializerListExpression(ExprPtrVec EV) : Expressions(std::move(EV)) {}

    ExprPtrVec &GetExprList() { return Expressions; }
    const ExprPtrVec &GetExprList() const { return Expressions; }
    void SetExprList(ExprPtrVec &e) { Expressions = std::move(e); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override { return nullptr; }

  private:
    ExprPtrVec Expressions;
};

class TranslationUnit : public Statement
{
  public:
    TranslationUnit() = default;
    TranslationUnit(StmtPtrVec s) : Declarations(std::move(s)) {}

    const StmtPtrVec &GetDeclarations() const { return Declarations; }
    void SetDeclarations(StmtPtrVec s) { Declarations = std::move(s); }
    void AddDeclaration(StmtPtr s) { Declarations.push_back(std::move(s)); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    StmtPtrVec Declarations;
};
