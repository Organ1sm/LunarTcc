#include "FrontEnd/AST/Semantics.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "FrontEnd/Lexer/Token.hpp"
#include "fmt/core.h"
#include <optional>

//=--------------------------------------------------------------------------=//
//=--------------------------- Helper functions -----------------------------=//
//=--------------------------------------------------------------------------=//

void Semantics::InsertToSymTable(const Token &SymName,
                                 Type SymType,
                                 const bool ToGlobal,
                                 ValueType SymValue)
{
    SymbolTableStack::Entry SymEntry(SymName, SymType, SymValue);

    auto SymNameStr = SymName.GetString();

    std::optional<SymbolTableStack::Entry> ExistingEntry =
        ToGlobal ? SymbolTables.ContainsInGlobalScope(SymNameStr) :
                   SymbolTables.ContainsInCurrentScope(SymNameStr);


    bool IsRedefinition = ExistingEntry.has_value();

    // If the existing definition just a function prototype
    // then it is not an error.
    if (auto FuncDecl = GetFuncDecl(SymNameStr);
        FuncDecl != nullptr && FuncDecl->GetBody() == nullptr)
        IsRedefinition = false;

    if (IsRedefinition)
    {
        std::string Msg = fmt::format("redefinition of '{}'", SymNameStr);
        DiagPrinter.AddError(Msg, SymName);

        Msg = "previous definition was here";
        DiagPrinter.AddNote(Msg, std::get<0>(ExistingEntry.value()));
    }
    else if (ToGlobal)
        SymbolTables.InsertGlobalEntry(SymEntry);
    else
        SymbolTables.InsertEntry(SymEntry);
}

const FunctionDeclaration *Semantics::GetFuncDecl(const std::string &FuncName)
{
    for (auto FuncDecl : FuncDeclList)
    {
        if (FuncDecl->GetName() == FuncName)
            return FuncDecl;
    }

    return nullptr;
}

//=--------------------------------------------------------------------------=//
//=----------------------------- Sema functions -----------------------------=//
//=--------------------------------------------------------------------------=//

void Semantics::VisitVariableDeclaration(const VariableDeclaration *node)
{
    auto VarName = node->GetName();

    InsertToSymTable(node->GetNameToken(), node->GetType());

    if (node->GetInitExpr())
        node->GetInitExpr()->Accept(this);
}

void Semantics::VisitMemberDeclaration(const MemberDeclaration *node) {}

void Semantics::VisitEnumDeclaration(const EnumDeclaration *node) {}

void Semantics::VisitStructDeclaration(const StructDeclaration *node) {}

void Semantics::VisitCompoundStatement(const CompoundStatement *node)
{
    SymbolTables.PushSymbolTable();

    for (const auto &Stmt : node->GetStatements())
        Stmt->Accept(this);

    SymbolTables.PopSymbolTable();
}

void Semantics::VisitExpressionStatement(const ExpressionStatement *node)
{
    if (node->GetExpression() != nullptr)
        node->GetExpression()->Accept(this);
}

void Semantics::VisitIfStatement(const IfStatement *node) {}

void Semantics::VisitSwitchStatement(const SwitchStatement *node) {}

void Semantics::VisitWhileStatement(const WhileStatement *node) {}

void Semantics::VisitDoWhileStatement(const DoWhileStatement *node) {}

void Semantics::VisitForStatement(const ForStatement *node) {}

void Semantics::VisitReturnStatement(const ReturnStatement *node)
{
    if (node->GetReturnVal())
        node->GetReturnVal()->Accept(this);
}

void Semantics::VisitBreakStatement(const BreakStatement *node) {}

void Semantics::VisitContinueStatement(const ContinueStatement *node) {}

// clang-format off
void Semantics::VisitFunctionParameterDeclaration(const FunctionParameterDeclaration *node)
// clang-format on
{}

void Semantics::VisitFunctionDeclaration(const FunctionDeclaration *node)
{
    auto FuncName = node->GetName();

    InsertToSymTable(node->GetNameToken(), node->GetType(), true);

    // Opening a new scope for the function
    SymbolTables.PushSymbolTable();

    for (const auto &Arg : node->GetArguments())
        Arg->Accept(this);

    if (node->GetBody())
    {
        auto &ParamList = node->GetArguments();

        for (auto &Param : ParamList)
            if (Param->GetType().IsVoid())
            {
                std::string Msg =
                    fmt::format("('{}') has incomplete type", Param->GetName());

                DiagPrinter.AddError(Msg, Param->GetNameToken());
            }
        node->GetBody()->Accept(this);
    }

    SymbolTables.PopSymbolTable();

    // Adding this function declaration to the already processed ones
    FuncDeclList.push_back(node);
}

void Semantics::VisitBinaryExpression(const BinaryExpression *node) {}

void Semantics::VisitTernaryExpression(const TernaryExpression *node) {}

void Semantics::VisitStructMemberReference(const StructMemberReference *node) {}

void Semantics::VisitStructInitExpression(const StructInitExpression *node) {}

void Semantics::VisitUnaryExpression(const UnaryExpression *node) {}

void Semantics::VisitCallExpression(const CallExpression *node) {}

void Semantics::VisitReferenceExpression(const ReferenceExpression *node)
{
    if (!SymbolTables.Contains(node->GetIdentifier()))
    {
        std::string Msg =
            fmt::format("use of undeclared identifier '{}'", node->GetIdentifier());

        DiagPrinter.AddError(Msg, node->GetIdentifierToken());
    }
}

void Semantics::VisitIntegerLiteralExpression(const IntegerLiteralExpression *node) {}

void Semantics::VisitFloatLiteralExpression(const FloatLiteralExpression *node) {}

void Semantics::VisitStringLiteralExpression(const StringLiteralExpression *node) {}

void Semantics::VisitArrayExpression(const ArrayExpression *node) {}

void Semantics::VisitImplicitCastExpression(const ImplicitCastExpression *node)
{
    node->GetCastableExpression()->Accept(this);
}

void Semantics::VisitInitializerListExpression(const InitializerListExpression *node) {}

void Semantics::VisitTranslationUnit(const TranslationUnit *node)
{
    for (const auto &Decl : node->GetDeclarations())
        Decl->Accept(this);
}
