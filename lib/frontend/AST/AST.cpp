#include "frontend/AST/AST.hpp"

void CompoundStatement::ASTDump(unsigned int tab)
{
    PrintLn("CompoundStatement", tab);
    for (auto &d : Declarations)
        d->ASTDump(tab + 2);
    for (auto &s : Statements)
        s->ASTDump(tab + 2);
}
void ExpressionStatement::ASTDump(unsigned int tab)
{
    PrintLn("ExpressionStatement", tab);
    Expr->ASTDump(tab + 2);
}

void IfStatement::ASTDump(unsigned int tab)
{
    PrintLn("IfStatement", tab);
    Condition->ASTDump(tab + 2);
    IfBody->ASTDump(tab + 2);
    ElseBody->ASTDump(tab + 2);
}

void WhileStatement::ASTDump(unsigned int tab)
{
    PrintLn("WhileStatement", tab);
    Condition->ASTDump(tab + 2);
    Body->ASTDump(tab + 2);
}

void ReturnStatement::ASTDump(unsigned int tab)
{
    PrintLn("ReturnStatement", tab);
    if (Value)
        Value.value()->ASTDump(tab + 2);
}

void FunctionParameterDeclaration::ASTDump(unsigned int tab)
{
    auto TypeStr = "'" + Ty.ToString() + "' ";
    auto NameStr = "'" + Name + "'";

    Print("FunctionParameterDeclaration", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());
}

void FunctionDeclaration::ASTDump(unsigned int tab)
{
    auto TypeStr = "'" + FuncType.ToString() + "' ";
    auto NameStr = "'" + Name + "'";
    Print("FunctionDeclaration", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());

    for (auto &Argument : Arguments)
        Argument->ASTDump(tab + 2);

    Body->ASTDump(tab + 2);
}
FunctionType FunctionDeclaration::CreateType(const Type &t,
                                             const FunctionDeclaration::ParamVec &params)
{
    FunctionType funcType(t);
    for (auto &Argument : params)
    {
        auto type = Argument->GetType().GetTypeVariant();
        funcType.GetArgumentTypes().push_back(type);
    }

    // if there are no arguments then set it to void
    if (params.empty())
        funcType.GetArgumentTypes().push_back(Type::Void);

    return funcType;
}

BinaryExpression::BinaryOperation BinaryExpression::GetOperationKind()
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
        case Token::Mod:
            return Mod;
        case Token::And:
            return And;
        case Token::Not:
            return Not;
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

void BinaryExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'" + Operation.GetString() + "'";

    Print("BinaryExpression ", tab);
    PrintLn(Str.c_str());

    Lhs->ASTDump(tab + 2);
    Rhs->ASTDump(tab + 2);
}
BinaryExpression::BinaryExpression(BinaryExpression::ExprPtr L,
                                   Token Op,
                                   BinaryExpression::ExprPtr R)
{
    Lhs       = std::move(L);
    Operation = Op;
    Rhs       = std::move(R);

    if (IsCondition())
        ResultType = ComplexType(Type::Int);
    else
        ResultType =
            ComplexType(Type::GetStrongestType(Lhs->GetResultType().GetTypeVariant(),
                                               Rhs->GetResultType().GetTypeVariant()));
}

void CallExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'" + Name + "'";

    Print("CallExpression", tab);
    PrintLn(Str.c_str());

    for (auto &Argument : Arguments)
        Argument->ASTDump(tab + 2);
}

void ReferenceExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'" + Identifier + "' ";

    Print("ReferenceExpression", tab);
    PrintLn(Str.c_str());
}

void IntegerLiteralExpression::ASTDump(unsigned int tab)
{
    Print("IntegerLiteralExpression ", tab);

    auto TyStr = "'" + ResultType.ToString() + "' ";
    Print(TyStr.c_str());

    auto ValStr = "'" + std::to_string(Value) + "'";
    PrintLn(ValStr.c_str());
}

void FloatLiteralExpression::ASTDump(unsigned int tab)
{
    Print("FloatLiteralExpression", tab);

    auto TyStr = "'" + ResultType.ToString() + "' ";
    Print(TyStr.c_str());

    auto ValueStr = "'" + std::to_string(Value) + "'";
    PrintLn(ValueStr.c_str());
}

void ArrayExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'" + Identifier.GetString() + "'";

    Print("ArrayExpression", tab);
    PrintLn(Str.c_str());

    for (auto &i : IndexExpression)
        i->ASTDump(tab + 2);
}

void TranslationUnit::ASTDump(unsigned int tab)
{
    PrintLn("TranslationUnit", tab);
    for (auto &Declaration : Declarations)
        Declaration->ASTDump(tab + 2);
}

void ImplicitCastExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "'";

    Print("ImplicitCastExpression ", tab);
    PrintLn(Str.c_str());

    CastableExpression->ASTDump(tab + 2);
}
