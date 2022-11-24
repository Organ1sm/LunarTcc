#include "FrontEnd/AST/AST.hpp"
#include "FrontEnd/AST/ASTPrint.hpp"
#include "Utils/ErrorLogger.hpp"
#include "fmt/core.h"

void ASTPrint::VisitVariableDeclaration(const VariableDeclaration *node)
{
    auto TypeStr = fmt::format("`{}` ", node->GetType().ToString());
    auto NameStr = fmt::format("`{}`", node->GetName());

    Print("VariableDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());

    tab += 2;
    if (node->GetInitExpr())
        node->GetInitExpr()->Accept(this);
    tab -= 2;
}

void ASTPrint::VisitMemberDeclaration(const MemberDeclaration *node)
{
    auto TypeStr = fmt::format("`{}` ", node->GetType().ToString());
    auto NameStr = fmt::format("`{}`", node->GetName());

    Print("MemberDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());
}

void ASTPrint::VisitStructDeclaration(const StructDeclaration *node)
{
    Print("StructDeclaration `", tab);
    Print(node->GetName().c_str());
    PrintLn("` ");

    for (auto &M : node->GetMembers())
        M->Accept(this);
}

void ASTPrint::VisitEnumDeclaration(const EnumDeclaration *node)
{
    auto HeaderStr = fmt::format("EnumDeclaration `{}`", node->GetBaseType().ToString());
    std::string BodyStr = "Enumerators ";

    std::size_t LoopCounter = 0;
    for (auto &[Enum, Val] : node->GetEnumerators())
    {
        BodyStr += fmt::format("`{}` = {}", Enum, Val);

        if (++LoopCounter < node->GetEnumerators().size())
            BodyStr += ", ";
    }

    PrintLn(HeaderStr.c_str(), tab);
    PrintLn(BodyStr.c_str(), tab + 2);
}

void ASTPrint::VisitFunctionDeclaration(const FunctionDeclaration *node)
{
    auto TypeStr = fmt::format("`{}` ", node->GetType().ToString());
    auto NameStr = fmt::format("`{}`", node->GetName());

    Print("FunctionDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());

    tab += 2;

    for (auto &Argument : node->GetArguments())
        Argument->Accept(this);

    if (node->GetBody())
        node->GetBody()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitFunctionParameterDeclaration(const FunctionParameterDeclaration *node)
{
    auto TypeStr = fmt::format("`{}` ", node->GetType().ToString());
    auto NameStr = fmt::format("`{}`", node->GetName());

    Print("FunctionParameterDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());
}

void ASTPrint::VisitCompoundStatement(const CompoundStatement *node)
{
    PrintLn("CompoundStatement ", tab);

    tab += 2;

    for (auto &s : node->GetStatements())
        s->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitExpressionStatement(const ExpressionStatement *node)
{
    PrintLn("ExpressionStatement ", tab);

    tab += 2;

    node->GetExpression()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitIfStatement(const IfStatement *node)
{
    PrintLn("IfStatement ", tab);

    tab += 2;

    node->GetCondition()->Accept(this);
    node->GetIfBody()->Accept(this);

    if (node->GetElseBody())
        node->GetElseBody()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitSwitchStatement(const SwitchStatement *node)
{
    PrintLn("SwitchStatement", tab);

    tab += 2;
    node->GetCondition()->Accept(this);

    for (auto &[CaseConst, CaseBody] : node->GetCaseBodies())
    {
        auto Str = fmt::format("Case `{}`", CaseConst);
        PrintLn(Str.c_str(), tab + 2);

        tab += 2;

        for (auto &CaseStmt : CaseBody)
            CaseStmt->Accept(this);

        tab -= 2;
    }

    if (!node->GetDefaultBody().empty())
        PrintLn("DefaultCase", tab + 2);

    tab += 2;

    for (auto &DefaultStmt : node->GetDefaultBody())
        DefaultStmt->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitWhileStatement(const WhileStatement *node)
{
    PrintLn("WhileStatement ", tab);

    tab += 2;

    node->GetCondition()->Accept(this);
    node->GetBody()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitForStatement(const ForStatement *node)
{
    PrintLn("ForStatement", tab);

    tab += 2;

    if (node->GetInit())
        node->GetInit()->Accept(this);
    else
        node->GetVarDecl()->Accept(this);


    node->GetCondition()->Accept(this);
    node->GetIncrement()->Accept(this);
    node->GetBody()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitReturnStatement(const ReturnStatement *node)
{
    PrintLn("ReturnStatement ", tab);

    tab += 2;

    if (node->GetReturnVal())
        node->GetReturnVal()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitBreakStatement(const BreakStatement *node)
{
    PrintLn("BreakStatement", tab);
}

void ASTPrint::VisitContinueStatement(const ContinueStatement *node)
{
    PrintLn("ContinueStatement", tab);
}

void ASTPrint::VisitBinaryExpression(const BinaryExpression *node)
{
    auto Str = fmt::format("`{}` `{}`",
                           node->GetResultType().ToString(),
                           node->GetOperation().GetString());

    Print("BinaryExpression ", tab);
    PrintLn(Str.c_str());

    tab += 2;

    node->GetLeftExpr()->Accept(this);
    node->GetRightExpr()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitTernaryExpression(const TernaryExpression *node)
{
    auto Str = fmt::format("`{}` ", node->GetResultType().ToString());

    Print("TernaryExpression ", tab);
    PrintLn(Str.c_str());

    tab += 2;

    node->GetCondition()->Accept(this);
    node->GetExprIfTrue()->Accept(this);
    node->GetExprIfFalse()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitStructMemberReference(const StructMemberReference *node)
{
    // Todo: if it's struct pointer, should output `-> member`
    auto Str =
        fmt::format("`{}` `.{}`", node->GetResultType().ToString(), node->GetMemberId());

    Print("StructMemberReference ", tab);
    PrintLn(Str.c_str());

    tab += 2;

    node->GetExpr()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitStructInitExpression(const StructInitExpression *node)
{
    auto Str = fmt::format("`{}` ", node->GetResultType().ToString());

    Print("StructInitExpression ", tab);
    PrintLn(Str.c_str());

    tab += 2;

    for (auto &InitValue : node->GetInitList())
        InitValue->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitUnaryExpression(const UnaryExpression *node)
{
    auto Str = fmt::format("`{}` {}",
                           node->GetResultType().ToString(),
                           node->GetOperation().GetString());
    Print("UnaryExpression ", tab);
    PrintLn(Str.c_str());

    tab += 2;

    node->GetExpr()->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitCallExpression(const CallExpression *node)
{
    auto Str =
        fmt::format("`{}` `{}`", node->GetResultType().ToString(), node->GetName());

    Print("CallExpression ", tab);
    PrintLn(Str.c_str());

    tab += 2;

    for (auto &Argument : node->GetArguments())
        Argument->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitReferenceExpression(const ReferenceExpression *node)
{
    auto Str =
        fmt::format("`{}` `{}`", node->GetResultType().ToString(), node->GetIdentifier());

    Print("ReferenceExpression ", tab);
    PrintLn(Str.c_str());
}

void ASTPrint::VisitIntegerLiteralExpression(const IntegerLiteralExpression *node)
{
    auto TypeStr = fmt::format("`{}` ", node->GetResultType().ToString());
    auto ValStr  = fmt::format("`{}`", node->GetSIntValue());

    Print("IntegerLiteralExpression ", tab);
    Print(TypeStr.c_str());
    PrintLn(ValStr.c_str());
}

void ASTPrint::VisitFloatLiteralExpression(const FloatLiteralExpression *node)
{
    auto TypeStr = fmt::format("`{}` ", node->GetResultType().ToString());
    auto ValStr  = fmt::format("`{}`", node->GetValue());

    Print("FloatLiteralExpression ", tab);
    Print(TypeStr.c_str());
    PrintLn(ValStr.c_str());
}

void ASTPrint::VisitStringLiteralExpression(const StringLiteralExpression *node)
{
    auto TypeStr = fmt::format("`{}` ", node->GetResultType().ToString());
    auto ValStr  = fmt::format("`{}`", node->GetValue());

    Print("StringLiteralExpression ", tab);
    Print(TypeStr.c_str());
    PrintLn(ValStr.c_str());
}

void ASTPrint::VisitArrayExpression(const ArrayExpression *node)
{
    std::string TypeStr = node->GetResultType().ToString();

    if (node->GetBase()->GetResultType().IsArray())
    {
        for (auto Dim : node->GetBase()->GetResultType().GetDimensions())
        {
            TypeStr += fmt::format("[{}]", Dim);
        }
    }

    auto Str = fmt::format("`{}`", TypeStr);

    Print("ArrayExpression ", tab);
    PrintLn(Str.c_str());

    tab += 2;
    node->GetIndexExpression()->Accept(this);
    tab -= 2;
}

void ASTPrint::VisitImplicitCastExpression(const ImplicitCastExpression *node)
{
    auto Str = fmt::format("`{}`", node->GetResultType().ToString());

    Print("ImplicitCastExpression ", tab);
    PrintLn(Str.c_str());

    tab += 2;
    node->GetCastableExpression()->Accept(this);
    tab -= 2;
}

void ASTPrint::VisitInitializerListExpression(const InitializerListExpression *node)
{
    PrintLn("InitializerListExpression", tab);

    tab += 2;

    for (auto &E : node->GetExprList())
        E->Accept(this);

    tab -= 2;
}

void ASTPrint::VisitTranslationUnit(const TranslationUnit *node)
{
    PrintLn("TranslationUnit", tab);

    tab += 2;

    for (auto &Declaration : node->GetDeclarations())
        Declaration->Accept(this);

    tab -= 2;

    PrintLn("");
}
