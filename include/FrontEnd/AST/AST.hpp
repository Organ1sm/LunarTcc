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
    virtual ~Node() = default;

    virtual void Accept(ASTVisitor *Visitor) const {}
    virtual Value *IRCodegen(IRFactory *IRF);
};

class Statement : public Node
{
  public:
    enum StmtInfo { Return = 1 };

    void AddInfo(unsigned Bit) { InfoBits |= Bit; }
    [[nodiscard]] bool IsRet() { return !!(InfoBits & Return); }

  private:
    unsigned InfoBits = 0;
};

class Expression : public Node
{
  public:
    Expression() = default;
    explicit Expression(Type t) : ResultType(std::move(t)) {}
    explicit Expression(Type::VariantKind vk) : ResultType(vk) {}

    Type &GetResultType() { return ResultType; }
    const Type &GetResultType() const { return ResultType; }
    void SetType(const Type &t) { ResultType = t; }

    void SetLValueness(bool p) { IsLValue = p; }
    bool GetLValueness() const { return IsLValue; }

  protected:
    bool IsLValue {false};
    Type ResultType;
};

class VariableDeclaration : public Statement
{
  public:
    VariableDeclaration(const Token &Name, Type Ty) : Name(Name), AType(std::move(Ty)) {}
    VariableDeclaration(const Token &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(std::move(Ty), std::move(Dim))
    {}

    VariableDeclaration(const Token &Name, Type Ty, ExprPtr E)
        : Name(Name), AType(std::move(Ty)), Init(std::move(E))
    {}

    const ExprPtr &GetInitExpr() const { return Init; }
    void SetInitExpr(ExprPtr e) { Init = std::move(e); }

    std::string GetName() const { return Name.GetString(); }
    const Token &GetNameToken() const { return Name; }

    Type GetType() const { return AType; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Name;
    Type AType;
    ExprPtr Init {nullptr};
};

class MemberDeclaration : public Statement
{
  public:
    MemberDeclaration() = default;
    MemberDeclaration(const Token &Name, Type Ty) : Name(Name), AType(std::move(Ty)) {}
    MemberDeclaration(const Token &Name, Type Ty, std::vector<unsigned> Dim)
        : Name(Name), AType(std::move(Ty), std::move(Dim))
    {}

    std::string GetName() const { return Name.GetString(); }
    Token GetNameToken() const { return Name; }

    Type GetType() const { return AType; }
    void SetType(Type t) { AType = t; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Name;
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

    explicit EnumDeclaration(EnumList Enumerators) : Enumerators(std::move(Enumerators))
    {}

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
    StructDeclaration(const Token &Name, MemberDeclVec &M, Type &StructType)
        : Name(Name), Members(std::move(M)), SType(StructType)
    {}

    std::string GetName() const { return Name.GetString(); }
    const MemberDeclVec &GetMembers() const { return Members; }
    const Type &GetType() const { return SType; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Type SType;
    Token Name;
    MemberDeclVec Members;
};

class CompoundStatement : public Statement
{
  public:
    CompoundStatement()                                     = delete;
    CompoundStatement(const CompoundStatement &)            = delete;
    CompoundStatement &operator=(const CompoundStatement &) = delete;
    CompoundStatement(CompoundStatement &&)                 = default;

    explicit CompoundStatement(StmtPtrVec &Stats) : Statements(std::move(Stats)) {}

    StmtPtrVec &GetStatements() { return Statements; }
    const StmtPtrVec &GetStatements() const { return Statements; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    StmtPtrVec Statements;
};

class ExpressionStatement : public Statement
{
  public:
    [[nodiscard]] const ExprPtr &GetExpression() const { return Expr; }
    void SetExpression(ExprPtr e) { Expr = std::move(e); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Expr;
};

class IfStatement : public Statement
{
  public:
    [[nodiscard]] const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr c) { Condition = std::move(c); }

    [[nodiscard]] const StmtPtr &GetIfBody() const { return IfBody; }
    void SetIfBody(StmtPtr iB) { IfBody = std::move(iB); }

    [[nodiscard]] const StmtPtr &GetElseBody() const { return ElseBody; }
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
    using CasesDataVec = std::vector<std::pair<ExprPtr, StmtPtrVec>>;

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
    [[nodiscard]] const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr c) { Condition = std::move(c); }

    [[nodiscard]] const StmtPtr &GetBody() const { return Body; }
    void SetBody(StmtPtr b) { Body = std::move(b); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Condition;
    StmtPtr Body;
};

class DoWhileStatement : public Statement
{
  public:
    [[nodiscard]] const ExprPtr &GetCondition() const { return Condition; }
    void SetCondition(ExprPtr c) { Condition = std::move(c); }

    [[nodiscard]] const StmtPtr &GetBody() const { return Body; }
    void SetBody(StmtPtr b) { Body = std::move(b); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr Condition;
    StmtPtr Body;
};

class ForStatement : public Statement
{
    using StmtPtrVec = std::vector<std::unique_ptr<VariableDeclaration>>;

  public:
    const StmtPtrVec &GetVarDecls() const { return VarDecls; }
    void SetVarDecls(StmtPtrVec VD) { VarDecls = std::move(VD); }

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
    StmtPtrVec VarDecls;
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

    [[nodiscard]] const ExprPtr &GetReturnVal() const
    {
        assert(HasValue() && "Must have a value to return it.");
        return ReturnValue.value();
    }

    [[nodiscard]] bool HasValue() const { return ReturnValue.has_value(); }

    ReturnStatement() { AddInfo(Statement::Return); };
    explicit ReturnStatement(ExprPtr e) : ReturnValue(std::move(e))
    {
        AddInfo(Statement::Return);
    }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::optional<ExprPtr> ReturnValue;
};

class FunctionParameterDeclaration : public Statement
{
  public:
    std::string GetName() const { return Name.GetString(); }
    const Token &GetNameToken() const { return Name; }
    void SetName(Token &name) { Name = name; }

    Type GetType() const { return Ty; }
    void SetType(const Type &t) { Ty = t; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Name;
    Type Ty;
};

class FunctionDeclaration : public Statement
{
    using ParamVec = std::vector<std::unique_ptr<FunctionParameterDeclaration>>;

  public:
    FunctionDeclaration() = delete;
    FunctionDeclaration(Type FT,
                        Token Name,
                        ParamVec &Args,
                        std::unique_ptr<CompoundStatement> &Body,
                        unsigned RetNum)
        : FuncType(std::move(FT)), Name(Name), Arguments(std::move(Args)),
          Body(std::move(Body)), ReturnNumber(RetNum)
    {}

    Type GetType() const { return FuncType; }
    void SetType(Type ft) { FuncType = ft; }

    const std::string GetName() const { return Name.GetString(); }
    const Token &GetNameToken() const { return Name; }
    void SetName(Token &name) { Name = name; }

    const ParamVec &GetArguments() const { return Arguments; }

    const std::unique_ptr<CompoundStatement> &GetBody() const { return Body; }

    static Type CreateType(const Type &t, const ParamVec &params);

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    unsigned ReturnNumber;
    Type FuncType;
    Token Name;
    ParamVec Arguments;
    std::unique_ptr<CompoundStatement> Body;
};

class StructMemberReference : public Expression
{
  public:
    StructMemberReference() = default;
    StructMemberReference(ExprPtr Expr, Token &Id, std::size_t Idx, bool Arrow);

    std::string GetMemberId() const { return MemberIdentifier.GetString(); }
    const Token &GetMemberIdToken() const { return MemberIdentifier; }

    const ExprPtr &GetExpr() const { return StructTypedExpression; }
    void SetExpr(ExprPtr &e) { StructTypedExpression = std::move(e); }

    bool IsArrow() const { return Arrow; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    bool Arrow {};
    ExprPtr StructTypedExpression;
    Token MemberIdentifier;
    std::size_t MemberIndex {};
};

class StructInitExpression : public Expression
{
  public:
    using UintList = std::vector<unsigned>;

  public:
    StructInitExpression() {}
    StructInitExpression(const Type &ResultType,
                         ExprPtrVec InitValues,
                         UintList InitOrder)
        : MemberOrdering(std::move(InitOrder)), InitValues(std::move(InitValues))
    {
        this->ResultType = ResultType;
    }

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
        ModAssign,    // a %= b
        AndAssign,    // a &= b
        OrAssign,     // a |= b
        XorAssign,    // a ^= b
        LSLAssign,    // a <<= b
        LSRAssign,    // a >>= b
        LSL,          // a << 1
        LSR,          // a >> 2
        Add,
        Sub,
        Mul,
        Div,
        DivU,
        AddF,
        SubF,
        MulF,
        DivF,
        Mod,
        ModU,
        And,
        Or,
        Xor,
        Not,
        Equal,
        Less,
        Greater,
        LessEqual,
        GreaterEqual,
        NotEqual,
        LogicalAnd,
        LogicalOr,
    };

    BinaryOperation GetOperationKind() const;

    Token GetOperation() const { return Operation; }

    const ExprPtr &GetLeftExpr() const { return Lhs; }
    const ExprPtr &GetRightExpr() const { return Rhs; }

    bool IsCondition() const { return GetOperationKind() >= Not; }
    bool IsAssignment() const { return GetOperationKind() <= LSRAssign; }
    bool IsCompositeAssignmentOperator() const;
    bool IsModulo() const;
    bool IsShift() const;

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
        Not,           // '!'
        BitWiseNot,    // '~'
        PostIncrement,
        PostDecrement,
        PreIncrement,
        PreDecrement,
        Sizeof,
    };

    UnaryExpression() = default;
    UnaryExpression(Token Op, ExprPtr E, bool PostFix = false);

    UnaryOperation GetOperationKind() const;

    Token GetOperation() const { return Operation; }

    const ExprPtr &GetExpr() const { return Expr; }

    Type GetSizeOfType() const
    {
        assert(SizeOfType.has_value());
        return SizeOfType.value();
    }

    void SetSizeOfType(const Type &t) { SizeOfType = t; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Operation;
    ExprPtr Expr {nullptr};
    bool IsPostFix {false};
    std::optional<Type> SizeOfType {std::nullopt};
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
    const ExprPtr &GetExprIfTrue() const { return ExprIfTrue; }
    const ExprPtr &GetExprIfFalse() const { return ExprIfFalse; }

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
    CallExpression(const Token &Name, ExprPtrVec &Args, Type T)
        : Name(Name), Arguments(std::move(Args)), FuncType(std::move(T))
    {
        Type ResultType = FuncType;
        ResultType.GetParamList().clear();

        GetResultType() = ResultType;
    }

    std::string GetName() const { return Name.GetString(); }
    const Token &GetNameToken() const { return Name; }
    const ExprPtrVec &GetArguments() const { return Arguments; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Name;
    Type FuncType;
    ExprPtrVec Arguments;
};

class ReferenceExpression : public Expression
{
  public:
    explicit ReferenceExpression(Token t) : Identifier(t) {}

    std::string GetIdentifier() { return Identifier.GetString(); }
    std::string GetIdentifier() const { return Identifier.GetString(); }
    const Token &GetIdentifierToken() const { return Identifier; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    Token Identifier;
};

class IntegerLiteralExpression : public Expression
{
  public:
    IntegerLiteralExpression() = delete;
    explicit IntegerLiteralExpression(uint64_t v) : IntValue(v)
    {
        SetType(Type(Type::Int));
    }

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
    explicit FloatLiteralExpression(double v) : FPValue(v)
    {
        SetType(Type(Type::Double));
    }

    double GetValue() const { return FPValue; }
    void SetValue(double v) { FPValue = v; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    double FPValue;
};

class StringLiteralExpression : public Expression
{
  public:
    explicit StringLiteralExpression(const std::string &s) : StringValue(s)
    {
        std::vector<unsigned> d = {static_cast<unsigned>(s.length()) + 1};

        /// string literal is a char array.
        SetType(Type(Type(Type::Char), std::move(d)));
    }

    std::string GetValue() const { return StringValue; }
    void SetValue(std::string v) { StringValue = v; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    std::string StringValue;
};

class ArrayExpression : public Expression
{
  public:
    ArrayExpression(ExprPtr &Base, ExprPtr &IEs, const Type &Ct = Type())
        : BaseExpression(std::move(Base)), IndexExpression(std::move(IEs))
    {
        ResultType = Ct;
    }

    const ExprPtr &GetIndexExpression() const { return IndexExpression; }
    const ExprPtr &GetBase() const { return BaseExpression; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr BaseExpression;
    ExprPtr IndexExpression;
};

class ImplicitCastExpression : public Expression
{
  public:
    ImplicitCastExpression(ExprPtr e, Type t, bool c = false)
        : CastableExpression(std::move(e)), Expression(std::move(t)), IsExplicitCast(c)
    {}

    ExprPtr &GetCastableExpression() { return CastableExpression; }
    ExprPtr const &GetCastableExpression() const { return CastableExpression; }

    bool IsExplicit() const { return IsExplicitCast; }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    ExprPtr CastableExpression;
    bool IsExplicitCast {false};
};

class InitializerListExpression : public Expression
{
  public:
    explicit InitializerListExpression(ExprPtrVec EV) : Expressions(std::move(EV)) {}

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
    explicit TranslationUnit(StmtPtrVec s) : Declarations(std::move(s)) {}

    const StmtPtrVec &GetDeclarations() const { return Declarations; }
    void AddDeclaration(StmtPtr s) { Declarations.push_back(std::move(s)); }

    void Accept(ASTVisitor *Visitor) const override;
    Value *IRCodegen(IRFactory *IRF) override;

  private:
    StmtPtrVec Declarations;
};
