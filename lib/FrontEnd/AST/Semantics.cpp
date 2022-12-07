#include "FrontEnd/AST/Semantics.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "FrontEnd/Lexer/Token.hpp"
#include "FrontEnd/Support.hpp"
#include "fmt/core.h"
#include <optional>

//=--------------------------------------------------------------------------=//
//=--------------------------- Helper functions -----------------------------=//
//=--------------------------------------------------------------------------=//

static Expression *GetExprIgnoreImplicitCast(Expression *Expr)
{
    Expression *CastedExpr = Expr;

    while (auto ICE = dynamic_cast<ImplicitCastExpression *>(CastedExpr))
        CastedExpr = ICE->GetCastableExpression().get();

    return CastedExpr;
}

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

void Semantics::VisitStructDeclaration(const StructDeclaration *node)
{
    // Register the incomplete type
    UserDefinedTypes[node->GetName()] = {node->GetType(), {}};

    for (const auto &M : node->GetMembers())
        M->Accept(this);

    // Register the full defined type
    std::vector<Token> StructMemberIdentifiers;
    for (const auto &M : node->GetMembers())
        StructMemberIdentifiers.push_back(M->GetNameToken());

    UserDefinedTypes[node->GetName()] = {node->GetType(), {StructMemberIdentifiers}};
}

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

void Semantics::VisitIfStatement(const IfStatement *node)
{
    node->GetCondition()->Accept(this);
    node->GetIfBody()->Accept(this);

    if (node->GetElseBody())
        node->GetElseBody()->Accept(this);
}

void Semantics::VisitSwitchStatement(const SwitchStatement *node)
{
    node->GetCondition()->Accept(this);

    for (const auto &[CaseConst, CaseBody] : node->GetCaseBodies())
    {
        // if non constant value used as label
        if (! instanceof <IntegerLiteralExpression>(CaseConst.get()))
        {
            std::string Msg =
                "case lable expression is not an integer constant expression\n";
            DiagPrinter.AddError(Msg);
        }

        for (const auto &CaseStmt : CaseBody)
            CaseStmt->Accept(this);
    }

    if (!node->GetDefaultBody().empty())
    {
        for (const auto &DefaultStmt : node->GetDefaultBody())
            DefaultStmt->Accept(this);
    }
}

void Semantics::VisitWhileStatement(const WhileStatement *node)
{
    node->GetCondition()->Accept(this);
    node->GetBody()->Accept(this);
}

void Semantics::VisitDoWhileStatement(const DoWhileStatement *node)
{
    node->GetBody()->Accept(this);
    node->GetCondition()->Accept(this);
}

void Semantics::VisitForStatement(const ForStatement *node)
{
    SymbolTables.PushSymbolTable();

    if (node->GetInit())
        node->GetInit()->Accept(this);
    else
    {
        const auto &VarDecls = node->GetVarDecls();
        for (const auto &Decl : VarDecls)
            Decl->Accept(this);
    }

    if (node->GetCondition())
        node->GetCondition()->Accept(this);

    if (node->GetIncrement())
        node->GetIncrement()->Accept(this);

    node->GetBody()->Accept(this);


    SymbolTables.PopSymbolTable();
}

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
{
    InsertToSymTable(node->GetNameToken(), node->GetType());
}

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

void Semantics::VisitBinaryExpression(const BinaryExpression *node)
{
    std::string Msg;
    // Check if the left hand side of the assignment is an L-value
    if (node->IsAssignment() && !node->GetLeftExpr()->GetLValueness())
    {
        Msg =
            "lvalue required of left operand of expression, so expression is not assignable";
    }

    const auto &LHSType = node->GetLeftExpr()->GetResultType();
    const auto &RHSType = node->GetRightExpr()->GetResultType();

    // Ensure that the left hand side of the assignment is not a const
    if (node->IsAssignment() && LHSType.IsConst())
    {
        Msg = fmt::format("cannot assign to variable with const-qualified type '{}'",
                          LHSType.ToString());
    }

    // Modulo operator only applicable to integers
    // Shift right hand operand must be an integer
    if ((node->IsModulo() && (!LHSType.IsIntegerType() || !RHSType.IsIntegerType())) ||
        (node->IsShift() && !RHSType.IsIntegerType()))
    {
        Msg = fmt::format("invalid operands to binary expression ('{}' and '{}')",
                          GetExprIgnoreImplicitCast(node->GetLeftExpr().get())
                              ->GetResultType()
                              .ToString(),
                          RHSType.ToString());
    }

    /// Check if the types are matching in an assignment
    if (node->IsAssignment() && LHSType != RHSType)
    {
        Msg =
            fmt::format("incompatiable types when assigning to type '{}' from type '{}'",
                        LHSType.ToString(),
                        RHSType.ToString());
    }

    if (!Msg.empty())
        DiagPrinter.AddError(Msg, node->GetOperation());

    node->GetLeftExpr()->Accept(this);
    node->GetRightExpr()->Accept(this);
}

void Semantics::VisitTernaryExpression(const TernaryExpression *node)
{
    node->GetCondition()->Accept(this);
    node->GetExprIfTrue()->Accept(this);
    node->GetExprIfFalse()->Accept(this);
}

void Semantics::VisitStructMemberReference(const StructMemberReference *node)
{
    std::string Msg;
    // Validate that indeed a struct is the base type for member reference
    if (!node->IsArrow() && !node->GetExpr()->GetResultType().IsStruct())
    {
        Msg = fmt::format("member reference base type '{}' is not a structure or union",
                          node->GetExpr()->GetResultType().ToString());
    }

    else if (node->IsArrow() && !node->GetExpr()->GetResultType().IsPointerType())
    {
        Msg = fmt::format("invalid type argument of '->'(have '{}')",
                          node->GetExpr()->GetResultType().ToString());
    }
    else
    {
        /// Validate that the accessed member name is exists in the struct
        bool FoundMatch = false;

        const auto StructName = node->GetExpr()->GetResultType().GetName();
        for (auto StructMember : std::get<1>(UserDefinedTypes[StructName]))
        {
            if (StructMember == node->GetMemberIdToken())
            {
                FoundMatch = true;
                break;
            }
        }

        if (!FoundMatch)
        {
            Msg = fmt::format(" no member named '{}' in '{}'",
                              node->GetMemberId(),
                              std::get<0>(UserDefinedTypes[StructName]).ToString());
        }
    }

    if (!Msg.empty())
        DiagPrinter.AddError(Msg, node->GetMemberIdToken());

    node->GetExpr()->Accept(this);
}

void Semantics::VisitStructInitExpression(const StructInitExpression *node)
{
    for (auto &InitValue : node->GetInitList())
        InitValue->Accept(this);
}

void Semantics::VisitUnaryExpression(const UnaryExpression *node)
{
    if (node->GetExpr())
        node->GetExpr()->Accept(this);
}

void Semantics::VisitCallExpression(const CallExpression *node)
{
    auto CalledFunc = SymbolTables.Contains(node->GetName());

    if (CalledFunc)
    {
        // too many argument
        auto &[CalledFuncName, CalledFuncType, _] = CalledFunc.value();

        const auto FuncArgNum = CalledFuncType.GetArgTypes().size();
        const auto CallArgNum = node->GetArguments().size();

        // Exception case if the function has only one void argument, in which
        // calling it without a parameter is permitted.
        bool CallNoParameterFunc = (FuncArgNum == 1 && CallArgNum == 0 &&
                                    CalledFuncType.GetArgTypes()[0] == Type(Type::Void));

        if (!CalledFuncType.HasVarArg() && FuncArgNum != CallArgNum &&
            !CallNoParameterFunc)
        {
            std::string descript = FuncArgNum > CallArgNum ? "few" : "many";

            auto Msg = fmt::format("too {} arguments to function '{}' call",
                                   descript,
                                   node->GetName());

            DiagPrinter.AddError(Msg, node->GetNameToken());

            auto FuncDeclMsg =
                fmt::format("'{}' function declared here", CalledFuncName.GetString());
            DiagPrinter.AddNote(FuncDeclMsg, CalledFuncName);
        }
    }

    else
    {
        auto Msg = fmt::format("implicit declaration of function '{}'", node->GetName());

        DiagPrinter.AddWarning(Msg, node->GetNameToken());
    }

    for (const auto &Arg : node->GetArguments())
        Arg->Accept(this);
}

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

void Semantics::VisitArrayExpression(const ArrayExpression *node)
{
    if (!node->GetBase()->GetResultType().IsArray() &&
        !node->GetBase()->GetResultType().IsPointerType())
    {
        std::string Msg = "subscripted value is not an array, pointer";
        DiagPrinter.AddError(Msg,
                             dynamic_cast<ReferenceExpression *>(node->GetBase().get())
                                 ->GetIdentifierToken());
    }
}

void Semantics::VisitImplicitCastExpression(const ImplicitCastExpression *node)
{
    node->GetCastableExpression()->Accept(this);
}

void Semantics::VisitInitializerListExpression(const InitializerListExpression *node)
{
    for (const auto &E : node->GetExprList())
        E->Accept(this);
}

void Semantics::VisitTranslationUnit(const TranslationUnit *node)
{
    for (const auto &Decl : node->GetDeclarations())
        Decl->Accept(this);
}
