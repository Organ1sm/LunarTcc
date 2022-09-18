#include "FrontEnd/AST/AST.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/IRType.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include <cassert>
#include <memory>

//=--------------------------------------------------------------------------=//
//=------------------------- IR Codegen functions ---------------------------=//
//=--------------------------------------------------------------------------=//

Value *Node::IRCodegen(IRFactory *IRF)
{
    assert(!"Must be a child node type");
    return nullptr;
}

static IRType GetIRTypeFromVK(Type::VariantKind VK)
{
    switch (VK)
    {
        case Type::Int:
            return IRType(IRType::SInt);
            break;
        case Type::Double:
            return IRType(IRType::FP, 64);
            break;
        default:
            assert(!"Invalid type.");
            break;
    }
}

Value *CompoundStatement::IRCodegen(IRFactory *IRF)
{
    for (auto &Declaration : Declarations)
        Declaration->IRCodegen(IRF);

    for (auto &Statement : Statements)
        Statement->IRCodegen(IRF);

    return nullptr;
}

Value *ExpressionStatement::IRCodegen(IRFactory *IRF) { return Expr->IRCodegen(IRF); }

// if there is no else clause, then IR should be something like:
//    # generate code for Condition
//    # if the Condition is a CMP instruction, then revert its
//    # relation otherwise insert another CMP like this:
//    # cmp.eq $c, $Condition, 0 # true if Condition false
//    br $c, <if_end>
// <if_true>
//    # generate code for IfBody
// <if_end>
//
// If there is also an else branch then:
//    # generate code for Condition
//    # if the Condition is a CMP instruction, then revert its
//    # relation otherwise insert another CMP like this:
//    # cmp.eq $c, $Condition, 0 # true if Condition false
//    br $c, <else>
// <if_true>
//    # generate code for IfBody
//    j <if_end>
// <else>
//    # generate code for ElseBody
//    j <if_end>
// <if_end>
Value *IfStatement::IRCodegen(IRFactory *IRF)
{
    const bool HaveElse = (ElseBody != nullptr);
    const auto FuncPtr  = IRF->GetCurrentFunction();

    std::unique_ptr<BasicBlock> Else;
    if (HaveElse)
        Else = std::make_unique<BasicBlock>("if_else", FuncPtr);

    auto IfEnd = std::make_unique<BasicBlock>("if_end", FuncPtr);
    auto Cond  = Condition->IRCodegen(IRF);

    // if Condition was a compare instruction then just revert its relation.
    if (auto CMP = dynamic_cast<CompareInstruction *>(Cond); CMP != nullptr)
    {
        CMP->InvertRelation();
        IRF->CreateBranch(Cond, HaveElse ? Else.get() : IfEnd.get());
    }
    else
    {
        auto cmp =
            IRF->CreateCmp(CompareInstruction::EQ, Cond, IRF->GetConstant((uint64_t)0));

        IRF->CreateBranch(cmp, HaveElse ? Else.get() : IfEnd.get());
    }

    // if True
    auto IfTrue = std::make_unique<BasicBlock>("if_true", FuncPtr);
    IRF->InsertBB(std::move(IfTrue));
    IfBody->IRCodegen(IRF);
    IRF->CreateJump(IfEnd.get());

    if (HaveElse)
    {
        IRF->InsertBB(std::move(Else));
        ElseBody->IRCodegen(IRF);
        IRF->CreateJump(IfEnd.get());
    }

    IRF->InsertBB(std::move(IfEnd));
    return nullptr;
}

//  <loop_header>
//    # generate code for the Condition
//    # if the Condition is a CMP instruction, then revert its
//    # relation otherwise insert another CMP like this:
//    # cmp.eq $c, $Condition, 0    # true if Condition false
//    br $condition, <loop_end>     # goto loop_end if condition false
//  <loop_body>
//    # generate conde for the Body
//    j <loop_header>
//  <loop_end>
Value *WhileStatement::IRCodegen(IRFactory *IRF)
{
    const auto FuncPtr = IRF->GetCurrentFunction();
    auto Header        = std::make_unique<BasicBlock>("loop_header", FuncPtr);
    auto LoopBody      = std::make_unique<BasicBlock>("loop_body", FuncPtr);
    auto LoopEnd       = std::make_unique<BasicBlock>("loop_end", FuncPtr);
    auto HeaderPtr     = Header.get();

    IRF->CreateJump(Header.get());

    IRF->InsertBB(std::move(Header));
    auto Cond = Condition->IRCodegen(IRF);

    if (auto CMP = dynamic_cast<CompareInstruction *>(Cond); CMP != nullptr)
    {
        CMP->InvertRelation();
        IRF->CreateBranch(Cond, LoopEnd.get());
    }
    else
    {
        auto Cmp =
            IRF->CreateCmp(CompareInstruction::EQ, Cond, IRF->GetConstant((uint64_t)0));
        IRF->CreateBranch(Cmp, LoopEnd.get());
    }

    IRF->InsertBB(std::move(LoopBody));
    Body->IRCodegen(IRF);
    IRF->CreateJump(HeaderPtr);

    IRF->InsertBB(std::move(LoopEnd));

    return nullptr;
}

Value *ReturnStatement::IRCodegen(IRFactory *IRF)
{
    if (ReturnValue.has_value() == false)
        return IRF->CreateRet(nullptr);

    auto RetVal = ReturnValue.value()->IRCodegen(IRF);

    if (RetVal == nullptr)
        return nullptr;

    return IRF->CreateRet(RetVal);
}


Value *FunctionDeclaration::IRCodegen(IRFactory *IRF)
{
    IRF->SetGlobalScope(false);

    IRType ReturnType;

    switch (FuncType.GetReturnType())
    {
        case Type::Int:
            ReturnType = IRType(IRType::SInt);
            break;
        case Type::Double:
            ReturnType = IRType(IRType::FP, 64);
            break;
        case Type::Void:
            ReturnType = IRType(IRType::None);
            break;
        default:
            assert(!"Invalid fucntion return type.");
            break;
    }

    IRF->CreateNewFunction(Name, ReturnType);


    for (auto &Argument : Arguments)
        Argument->IRCodegen(IRF);

    Body->IRCodegen(IRF);
    return nullptr;
}

Value *FunctionParameterDeclaration::IRCodegen(IRFactory *IRF)
{
    IRType ParamType = GetIRTypeFromVK(Ty.GetTypeVariant());
    auto SA          = IRF->CreateSA(Name, ParamType);
    IRF->AddToSymbolTable(Name, SA);

    auto Param = std::make_unique<FunctionParameter>(FunctionParameter(Name, ParamType));

    auto ST = IRF->CreateStore(Param.get(), SA);
    IRF->Insert(std::move(Param));

    return nullptr;
}

Value *VariableDeclaration::IRCodegen(IRFactory *IRF)
{
    auto VarType = GetIRTypeFromVK(AType.GetTypeVariant());

    // If an array type, then change type to reflect this.
    if (AType.IsArray())
    {
        unsigned ElementNumber = 1;

        for (auto Dim : AType.GetDimensions())
            ElementNumber *= Dim;

        VarType.SetNumberOfElements(ElementNumber);
    }

    // If we are in global scope, then its a global variable declaration
    if (IRF->IsGlobalScope())
        return IRF->CreateGlobalVar(Name, VarType);

    /// Othewise we are in a localscope of a function.
    /// Allocate space on stack and update the local symbol Table.
    auto SA = IRF->CreateSA(Name, VarType);
    IRF->AddToSymbolTable(Name, SA);


    return SA;
}

Value *CallExpression::IRCodegen(IRFactory *IRF)
{
    std::vector<Value *> Args;

    for (auto &Arg : Arguments)
        Args.push_back(Arg->IRCodegen(IRF));

    auto ReturnType = GetResultType().GetFunctionType().GetReturnType();

    IRType ReturnIRType;

    switch (ReturnType)
    {
        case Type::Int:
            ReturnIRType = IRType(IRType::SInt);
            break;
        case Type::Double:
            ReturnIRType = IRType(IRType::FP, 64);
            break;
        default:
            break;
    }

    return IRF->CreateCall(Name, Args, ReturnIRType);
}

Value *ReferenceExpression::IRCodegen(IRFactory *IRF)
{
    auto Local = IRF->GetSymbolValue(Identifier);

    if (Local)
    {
        if (GetLValueness())
            return Local;
        else
            return IRF->CreateLoad(Local->GetType(), Local);
    }

    auto GV = IRF->GetGlobalVar(Identifier);

    // If Lvalue, then return as a ptr to the global value.
    if (GetLValueness())
        return GV;

    return IRF->CreateLoad(GV->GetType(), GV);
}


//=--------------------------------------------------------------------------=//
//=------------------------- AST ASTDump functions --------------------------=//
//=--------------------------------------------------------------------------=//

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
    if (ReturnValue)
        ReturnValue.value()->ASTDump(tab + 2);
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

    auto ValStr = "'" + std::to_string(IntValue) + "'";
    PrintLn(ValStr.c_str());
}

void FloatLiteralExpression::ASTDump(unsigned int tab)
{
    Print("FloatLiteralExpression", tab);

    auto TyStr = "'" + ResultType.ToString() + "' ";
    Print(TyStr.c_str());

    auto ValueStr = "'" + std::to_string(FPValue) + "'";
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
