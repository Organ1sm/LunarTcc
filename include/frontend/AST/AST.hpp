#ifndef LUNARTCC_AST_H
#define LUNARTCC_AST_H

#include <cassert>
#include <corecrt.h>
#include <iostream>
#include <cstdint>
#include <list>
#include <memory>

#include "frontend/Lexer/Token.hpp"
#include "frontend/Lexer/Type.hpp"
static void PrintImpl(const char *str, unsigned tab = 0, bool newline = false)
{
    for (int i = 0; i < tab; i++)
        std::cout << " ";
    std::cout << str;

    if (newline)
        std::cout << std::endl;
}

static void Print(const char *str, unsigned tab = 0) { PrintImpl(str, tab); }

static void PrintLn(const char *str, unsigned tab = 0) { PrintImpl(str, tab, true); }

class Node
{
  public:
    virtual void ASTDump(unsigned tab = 0) { PrintLn("Node"); }
};

class Statement : public Node
{
  public:
    void ASTDump(unsigned int tab = 0) override { PrintLn("Statement", tab); }
};

class Expression : public Node
{
  public:
    Type GetType() { return Ty; }
    void SetType(Type t) { Ty = t; }
    void ASTDump(unsigned int tab = 0) override { PrintLn("Expression", tab); }

  protected:
    Type Ty;
};

class VariableDeclaration : public Statement
{
  public:
    std::string &GetName() { return Name; }
    void SetName(std::string &name) { Name = name; }

  private:
    std::string Name;
    ArrayType Ty;
};

class CompoundStatment : public Statement
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
    void AddStatements(std::unique_ptr<Statement> &s) { Statements.push_back(std::move(s)); }

    CompoundStatment()                                    = delete;
    CompoundStatment(const CompoundStatment &)            = delete;
    CompoundStatment &operator=(const CompoundStatment &) = delete;
    CompoundStatment(CompoundStatment &&)                 = default;

    CompoundStatment(DeclVec &Decls, StmtVec &Stats)
        : Declarations(std::move(Decls)), Statements(std::move(Stats))
    {}

    void ASTDump(unsigned int tab = 0) override
    {
        PrintLn("CompoundStatement", tab);
        for (auto &d : Declarations)
            d->ASTDump(tab + 2);
        for (auto &s : Statements)
            s->ASTDump(tab + 2);
    }

  private:
    DeclVec Declarations;
    StmtVec Statements;
};

class ExpressionStatement : public Statement
{
  public:
    std::unique_ptr<Expression> &GetExpression() { return Expr; }
    void SetExpression(std::unique_ptr<Expression> e) { Expr = std::move(e); }
    void ASTDump(unsigned int tab = 0) override
    {
        PrintLn("ExpressionStatement", tab);
        Expr->ASTDump(tab + 2);
    }

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

    void ASTDump(unsigned int tab = 0) override
    {
        PrintLn("IfStatement", tab);
        Condition->ASTDump(tab + 2);
        IfBody->ASTDump(tab + 2);
        ElseBody->ASTDump(tab + 2);
    }

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

    void ASTDump(unsigned int tab = 0) override
    {
        PrintLn("WhileStatement", tab);
        Condition->ASTDump(tab + 2);
        Body->ASTDump(tab + 2);
    }

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
        return Value.value();
    }
    void SetCondition(std::unique_ptr<Expression> v) { Value = std::move(v); }
    bool HasValue() { return Value.has_value(); }

    ReturnStatement() = default;
    ReturnStatement(std::unique_ptr<Expression> e) : Value(std::move(e)) {}

    void ASTDump(unsigned int tab = 0) override
    {
        PrintLn("ReturnStatement", tab);
        if (Value)
            Value.value()->ASTDump(tab + 2);
    }

  private:
    std::optional<std::unique_ptr<Expression>> Value;
};


class FunctionParameterDeclaration : public Statement
{
  public:
    std::string &GetName() { return Name; }
    void SetName(std::string &s) { Name = s; }

    Type GetType() { return Ty; }
    void SetType(Type t) { Ty = t; }

    void ASTDump(unsigned int tab = 0) override
    {
        auto TypeStr = "'" + Ty.ToString() + "' ";
        auto NameStr = "'" + Name + "'";

        Print("FunctionParameterDeclaration", tab);
        Print(TypeStr.c_str());
        PrintLn(NameStr.c_str());
    }

  private:
    std::string Name;
    Type Ty;
};

class FunctionDeclaration : public Statement
{
    using ParamVec = std::vector<std::unique_ptr<FunctionParameterDeclaration>>;

  public:
    FunctionType GetType() { return Type; }
    void SetType(FunctionType ft) { Type = ft; }

    std::string &GetName() { return Name; }
    void SetName(std::string &s) { Name = s; }

    ParamVec &GetArguments() { return Arguments; }
    void SetArguments(ParamVec &a) { Arguments = std::move(a); }
    void SetArguments(ParamVec &&a) { Arguments = std::move(a); }

    std::unique_ptr<CompoundStatment> &GetBody() { return Body; }
    void SetBody(std::unique_ptr<CompoundStatment> &cs) { Body = std::move(cs); }

    void CaclArgumentTypes()
    {
        for (auto &Argument : Arguments)
        {
            auto t = Argument->GetType().GetTypeVariant();
            Type.GetArgumentTypes().push_back(t);
        }

        // if there are no arguments then set it to void
        if (Arguments.empty())
            Type.GetArgumentTypes().push_back(Type::Void);
    }

    FunctionDeclaration() = delete;
    FunctionDeclaration(FunctionType FT,
                        std::string Name,
                        ParamVec &Args,
                        std::unique_ptr<CompoundStatment> &Body)
        : Type(FT), Name(Name), Arguments(std::move(Args)), Body(std::move(Body))
    {
        CaclArgumentTypes();
    }

    void ASTDump(unsigned int tab = 0) override
    {
        auto TypeStr = "'" + Type.ToString() + "' ";
        auto NameStr = "'" + Name + "'";
        Print("FunctionDeclaration", tab);
        Print(TypeStr.c_str());
        PrintLn(NameStr.c_str());

        for (auto &Argument : Arguments)
            Argument->ASTDump(tab + 2);

        Body->ASTDump(tab + 2);
    }

  private:
    FunctionType Type;
    std::string Name;
    ParamVec Arguments;
    std::unique_ptr<CompoundStatment> Body;
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
        Equal,
        Less,
        Greater,
        NotEqual,
        LogicalAnd
    };

    BinaryOperation GetOperationKind()
    {
        switch (Operation.GetKind())
        {
            case Token::Assign:
                return Assign;
            case Token::Plus:
                return Add;
            case Token::Minus:
                return Sub;
            case Token::Mul:
                return Mul;
            case Token::Div:
                return Div;
            case Token::Equal:
                return Equal;
            case Token::Less:
                return Less;
            case Token::Greater:
                return Greater;
            case Token::NotEqual:
                return NotEqual;
            case Token::LogicalAnd:
                return LogicalAnd;
            default:
                assert(false && "Invalid binary Operator kind.");
                break;
        }
    }

    Token GetOperation() { return Operation; }
    Token SetOperation(Token op) { Operation = op; }

    ExprPtr &GettLeftExpr() { return Lhs; }
    void SetLeftExpr(ExprPtr &e) { Lhs = std::move(e); }

    ExprPtr &GetRightExpr() { return Rhs; }
    void SetRightExpr(ExprPtr &e) { Rhs = std::move(e); }

    bool IsCondition() { return GetOperationKind() >= Equal; }

    BinaryExpression() = default;
    BinaryExpression(ExprPtr Left, Token Op, ExprPtr Right)
        : Lhs(std::move(Left)), Operation(Op), Rhs(std::move(Right))
    {}

    void ASTDump(unsigned int tab = 0) override
    {
        Print("BinaryExpression ", tab);

        auto OpStr = "'" + Operation.GetString() + "'";
        PrintLn(OpStr.c_str());
        Lhs->ASTDump(tab + 2);
        Rhs->ASTDump(tab + 2);
    }

  private:
    Token Operation;
    std::unique_ptr<Expression> Lhs;
    std::unique_ptr<Expression> Rhs;
};




#endif