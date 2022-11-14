#pragma once

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
    enum StmtInfo { None = 0, Return = 1 };

    void AddInfo(unsigned Bit) { InfoBits |= Bit; }
    bool IsRet() { return !!(InfoBits & Return); }

    void ASTDump(unsigned int tab = 0) override { PrintLn("Statement", tab); }

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
    VariableDeclaration(std::string &Name, Type Ty) : Name(Name), AType(Ty) {}
    VariableDeclaration(std::string &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(Ty, std::move(Dim))
    {}

    VariableDeclaration(std::string &Name, Type Ty, std::unique_ptr<Expression> E)
        : Name(Name), AType(Ty), Init(std::move(E))
    {}

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
    using StmtVec = std::vector<std::unique_ptr<Statement>>;

  public:
    StmtVec &GetStatements() { return Statements; }
    void SetStatements(StmtVec &s) { Statements = std::move(s); }
    void AddStatements(std::unique_ptr<Statement> &s)
    {
        Statements.push_back(std::move(s));
    }

    CompoundStatement()                                     = delete;
    CompoundStatement(const CompoundStatement &)            = delete;
    CompoundStatement &operator=(const CompoundStatement &) = delete;
    CompoundStatement(CompoundStatement &&)                 = default;

    CompoundStatement(StmtVec &Stats) : Statements(std::move(Stats)) {}

    void ASTDump(unsigned int tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
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
    Value *IRCodegen(IRFactory *IRF) override;
};

class ContinueStatement : public Statement
{
  public:
    ContinueStatement() = default;

    void ASTDump(unsigned tab = 0) override { PrintLn("ContinueStatement", tab); }
    Value *IRCodegen(IRFactory *IRF) override;
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
    std::unique_ptr<Statement> &GetVarDecl() { return VarDecl; }
    void SetVarDecl(std::unique_ptr<Statement> VD) { VarDecl = std::move(VD); }

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
    std::unique_ptr<Statement> VarDecl {nullptr};
    std::unique_ptr<Expression> Init {nullptr};
    std::unique_ptr<Expression> Condition;
    std::unique_ptr<Expression> Increment;
    std::unique_ptr<Statement> Body;
};

class ReturnStatement : public Statement
{
  public:
    std::unique_ptr<Expression> &GetReturnVal()
    {
        assert(HasValue() && "Must have a value to return it.");
        return ReturnValue.value();
    }
    void SetReturnVal(std::unique_ptr<Expression> v) { ReturnValue = std::move(v); }
    bool HasValue() { return ReturnValue.has_value(); }

    ReturnStatement() { AddInfo(Statement::Return); };
    ReturnStatement(std::unique_ptr<Expression> e) : ReturnValue(std::move(e))
    {
        AddInfo(Statement::Return);
    }

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
    FunctionDeclaration() = delete;
    FunctionDeclaration(Type FT,
                        std::string Name,
                        ParamVec &Args,
                        std::unique_ptr<CompoundStatement> &Body,
                        unsigned RetNum)
        : FuncType(FT), Name(Name), Arguments(std::move(Args)), Body(std::move(Body)),
          ReturnNumber(RetNum)
    {}

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

    void ASTDump(unsigned int tab = 0) override;
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

class StructInitExpression : public Expression
{
  public:
    using StrList     = std::vector<std::string>;
    using ExprPtrList = std::vector<std::unique_ptr<Expression>>;

  public:
    StructInitExpression() {}
    StructInitExpression(Type ResultType,
                         StrList MemberIdentifiers,
                         ExprPtrList InitValues)
        : MemberIdentifiers(std::move(MemberIdentifiers)),
          InitValues(std::move(InitValues))
    {
        this->ResultType = ResultType;
    }

    StrList &GetMemberId() { return MemberIdentifiers; }
    void SetMemberId(StrList SL) { MemberIdentifiers = std::move(SL); }

    ExprPtrList &GetInitList() { return InitValues; }
    void SetInitList(ExprPtrList &IVS) { InitValues = std::move(IVS); }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    StrList MemberIdentifiers;
    ExprPtrList InitValues;
};


class BinaryExpression : public Expression
{
    using ExprPtr = std::unique_ptr<Expression>;

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
    enum UnaryOperation { Address, DeRef, Minus, PostIncrement, PostDecrement };

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

class TernaryExpression : public Expression
{
    using ExprPtr = std::unique_ptr<Expression>;

  public:
    TernaryExpression() = default;
    TernaryExpression(ExprPtr &Cond, ExprPtr &True, ExprPtr &False)
        : Condition(std::move(Cond)), ExprIfTrue(std::move(True)),
          ExprIfFalse(std::move(False))
    {
        ResultType = ExprIfTrue->GetResultType();
    }

    ExprPtr &GetCondition() { return Condition; }
    void SetCondition(ExprPtr &e) { Condition = std::move(e); }

    ExprPtr &GetExprIfTrue() { return ExprIfTrue; }
    void SetExprIfTrue(ExprPtr &e) { ExprIfTrue = std::move(e); }

    ExprPtr &GetExprIfFalse() { return ExprIfFalse; }
    void SetExprIfFalse(ExprPtr &e) { ExprIfFalse = std::move(e); }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Condition;
    ExprPtr ExprIfTrue;
    ExprPtr ExprIfFalse;
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
    int64_t GetValue() { return IntValue; }
    void SetValue(uint64_t v) { IntValue = v; }

    int64_t GetSIntValue() const { return IntValue; }
    uint64_t GetUIntValue() const { return IntValue; }

    IntegerLiteralExpression() = delete;
    IntegerLiteralExpression(uint64_t v) : IntValue(v) { SetResultType(Type(Type::Int)); }

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

class InitializerListExpression : public Expression
{
    using ExprVec = std::vector<std::unique_ptr<Expression>>;

  public:
    InitializerListExpression(ExprVec EV) : Expressions(std::move(EV)) {}

    ExprVec &GetExprList() { return Expressions; }
    void SetExprList(ExprVec &e) { Expressions = std::move(e); }

    void ASTDump(unsigned tab = 0) override;
    Value *IRCodegen(IRFactory *IRF) override { return nullptr; }

  private:
    ExprVec Expressions;
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
