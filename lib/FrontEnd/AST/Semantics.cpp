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

void Semantics::VisitVariableDeclaration(const VariableDeclaration *node) {}

void Semantics::VisitMemberDeclaration(const MemberDeclaration *node) {}

void Semantics::VisitEnumDeclaration(const EnumDeclaration *node) {}

void Semantics::VisitStructDeclaration(const StructDeclaration *node) {}

void Semantics::VisitCompoundStatement(const CompoundStatement *node) {}

void Semantics::VisitExpressionStatement(const ExpressionStatement *node) {}

void Semantics::VisitIfStatement(const IfStatement *node) {}

void Semantics::VisitSwitchStatement(const SwitchStatement *node) {}

void Semantics::VisitWhileStatement(const WhileStatement *node) {}

void Semantics::VisitDoWhileStatement(const DoWhileStatement *node) {}

void Semantics::VisitForStatement(const ForStatement *node) {}

void Semantics::VisitReturnStatement(const ReturnStatement *node) {}

void Semantics::VisitBreakStatement(const BreakStatement *node) {}

void Semantics::VisitContinueStatement(const ContinueStatement *node) {}

// clang-format off
void Semantics::VisitFunctionParameterDeclaration(const FunctionParameterDeclaration *node)
// clang-format on
{}

void Semantics::VisitFunctionDeclaration(const FunctionDeclaration *node) {}

void Semantics::VisitBinaryExpression(const BinaryExpression *node) {}

void Semantics::VisitTernaryExpression(const TernaryExpression *node) {}

void Semantics::VisitStructMemberReference(const StructMemberReference *node) {}

void Semantics::VisitStructInitExpression(const StructInitExpression *node) {}

void Semantics::VisitUnaryExpression(const UnaryExpression *node) {}

void Semantics::VisitCallExpression(const CallExpression *node) {}

void Semantics::VisitReferenceExpression(const ReferenceExpression *node) {}

void Semantics::VisitIntegerLiteralExpression(const IntegerLiteralExpression *node) {}

void Semantics::VisitFloatLiteralExpression(const FloatLiteralExpression *node) {}

void Semantics::VisitStringLiteralExpression(const StringLiteralExpression *node) {}

void Semantics::VisitArrayExpression(const ArrayExpression *node) {}

void Semantics::VisitImplicitCastExpression(const ImplicitCastExpression *node) {}

void Semantics::VisitInitializerListExpression(const InitializerListExpression *node) {}

void Semantics::VisitTranslationUnit(const TranslationUnit *node) {}
