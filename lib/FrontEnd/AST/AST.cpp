#include "FrontEnd/AST/AST.hpp"
#include "FrontEnd/AST/ASTVistor.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/IRType.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "Utils/ErrorLogger.hpp"
#include "fmt/core.h"
#include <cassert>
#include <cstdint>
#include <memory>

//=--------------------------------------------------------------------------=//
//=------------------------- Constructor  -----------------------------------=//
//=--------------------------------------------------------------------------=//

StructMemberReference::StructMemberReference(ExprPtr Expr,
                                             std::string Id,
                                             std::size_t Idx)
    : StructTypedExpression(std::move(Expr)), MemberIdentifier(Id), MemberIndex(Idx)
{
    auto STEType = StructTypedExpression->GetResultType();
    assert(MemberIndex < STEType.GetTypeList().size());

    this->ResultType = STEType.GetTypeList()[MemberIndex];
}

BinaryExpression::BinaryExpression(ExprPtr L, Token Op, ExprPtr R)
    : Lhs(std::move(L)), Operation(Op), Rhs(std::move(R))
{
    if (IsCondition())
        ResultType = Type(Type::Int);
    else
    {
        auto Strongest = Type::GetStrongestType(Lhs->GetResultType().GetTypeVariant(),
                                                Rhs->GetResultType().GetTypeVariant());

        ResultType = Type(Type::GetStrongestType(Strongest.GetTypeVariant(), Type::Int));
    }
}

UnaryExpression::UnaryExpression(Token Op, ExprPtr E) : Operation(Op), Expr(std::move(E))
{
    switch (GetOperationKind())
    {
        case UnaryOperation::Address:
            ResultType = Expr->GetResultType();
            ResultType.IncrementPointerLevel();
            break;
        case UnaryOperation::DeRef:
            ResultType = Expr->GetResultType();
            ResultType.DecrementPointerLevel();
            break;
        case UnaryOperation::Not: ResultType = Type(Type::Int); break;
        case UnaryOperation::Minus:
        case UnaryOperation::PostIncrement:
        case UnaryOperation::PostDecrement: ResultType = Expr->GetResultType(); break;

        default: assert(!"Unimplemented!");
    }
}

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
        case Type::Char: return IRType(IRType::SInt, 8);
        case Type::UnsignedChar: return IRType(IRType::UInt, 8);
        case Type::Short: return IRType(IRType::SInt, 16);
        case Type::UnsignedShort: return IRType(IRType::UInt, 16);
        case Type::Int: return IRType(IRType::SInt);
        case Type::UnsignedInt: return IRType(IRType::UInt);
        case Type::Long: return IRType(IRType::SInt, 64);
        case Type::UnsignedLong: return IRType(IRType::UInt, 64);
        case Type::LongLong: return IRType(IRType::SInt, 64);
        case Type::UnsignedLongLong: return IRType(IRType::UInt, 64);
        case Type::Double: return IRType(IRType::FP, 64);
        case Type::Composite: return IRType(IRType::Struct);
        default: assert(!"Invalid type."); break;
    }
}

static IRType GetIRTypeFromASTType(Type CT)
{
    IRType Result = GetIRTypeFromVK(CT.GetTypeVariant());

    if (Result.IsStruct())
    {
        auto StructName = CT.GetName();
        Result.SetStructName(StructName);

        // convert each member AST type to IRType (recursive)
        for (auto &MemberASTType : CT.GetTypeList())
            Result.GetMemberTypes().push_back(GetIRTypeFromASTType(MemberASTType));
    }

    if (CT.IsArray())
    {
        Result.SetDimensions(CT.GetDimensions());
    }

    Result.SetPointerLevel(CT.GetPointerLevel());
    return Result;
}

Value *CompoundStatement::IRCodegen(IRFactory *IRF)
{
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

//   # generate code for Condition
//   cmp.eq $cmp_res1, %Condition, case1_const
//   br $cmp_res1, <case1_body>
//   cmp.eq $cmp_res2, %Condition, case2_const
//   br $cmp_res2, <case2_body>
//   ...
//   cmp.eq $cmp_resN, %Condition, caseN_const
//   br $cmp_resN, <caseN_body>
//   j <default_case>
//
// <case1_body>
//   # generate case1 body
//   # create "j <switch_end>" when break is used
// ...
// <caseN_body>
//   # generate caseN body
//   # create "j <switch_end>" when break is used
// <default_case>
//   # generate default case body
// <switch_end>
Value *SwitchStatement::IRCodegen(IRFactory *IRF)
{
    const auto FuncPtr = IRF->GetCurrentFunction();
    auto SwitchEnd     = std::make_unique<BasicBlock>("switch_end", FuncPtr);
    auto DefaultCase   = std::make_unique<BasicBlock>("switch_default", FuncPtr);

    IRF->GetBreakEndBBsTable().push_back(SwitchEnd.get());

    auto Cond = Condition->IRCodegen(IRF);

    std::vector<std::unique_ptr<BasicBlock>> CaseBodies;

    for (auto &[Const, Statements] : Cases)
        if (!Statements.empty())
            CaseBodies.push_back(std::make_unique<BasicBlock>("switch_case", FuncPtr));

    // because of the fallthrough mechanism multiple cases could use the same name
    // code block, CaseIdx keep track the current target basic block so falling
    // through cases could refer to it
    std::size_t CaseIndex = 0;
    for (auto &[Const, Statements] : Cases)
    {
        auto CmpRes = IRF->CreateCmp(CompareInstruction::EQ,
                                     Cond,
                                     IRF->GetConstant((uint64_t)Const));
        IRF->CreateBranch(CmpRes, CaseBodies[CaseIndex].get());

        if (!Statements.empty())
            ++CaseIndex;
    }

    IRF->CreateJump(DefaultCase.get());

    // Generating the bodies for the cases
    for (auto &[Const, Statements] : Cases)
    {
        if (!Statements.empty())
        {
            IRF->InsertBB(std::move(CaseBodies.front()));

            for (auto &Statement : Statements)
            {
                Statement->IRCodegen(IRF);
            }

            CaseBodies.erase(CaseBodies.begin());
        }
    }

    // Generate default case
    IRF->InsertBB(std::move(DefaultCase));
    for (auto &Statement : DefaultBody)
        Statement->IRCodegen(IRF);

    IRF->InsertBB(std::move(SwitchEnd));
    IRF->GetBreakEndBBsTable().pop_back();

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
    IRF->GetBreakEndBBsTable().push_back(LoopEnd.get());

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
    IRF->GetBreakEndBBsTable().pop_back();

    return nullptr;
}

Value *ForStatement::IRCodegen(IRFactory *IRF)
{
    // Similar solution to WhileStatement. The difference is that here the
    // initialization part has to be generated before the loop_header basicblock
    // and also inserting the increment expression before the backward jump to the
    // loop_header
    const auto FuncPtr = IRF->GetCurrentFunction();
    auto Header        = std::make_unique<BasicBlock>("loop_header", FuncPtr);
    auto LoopIncrement = std::make_unique<BasicBlock>("loop_increment", FuncPtr);
    auto LoopBody      = std::make_unique<BasicBlock>("loop_body", FuncPtr);
    auto LoopEnd       = std::make_unique<BasicBlock>("loop_end", FuncPtr);
    auto HeaderPtr     = Header.get();

    IRF->GetLoopIncrementBBsTable().push_back(LoopIncrement.get());
    IRF->GetBreakEndBBsTable().push_back(LoopEnd.get());
    // Generating code for the initializing expression or the variable initialization and
    // adding and explicit unconditional jump to the loop header basic block
    if (Init)
        Init->IRCodegen(IRF);
    else
        VarDecl->IRCodegen(IRF);

    IRF->CreateJump(Header.get());

    // Inserting the loop header basicblock and generating the code
    // for the loop condition
    IRF->InsertBB(std::move(Header));
    auto Cond = Condition->IRCodegen(IRF);

    if (auto CMP = dynamic_cast<CompareInstruction *>(Cond); CMP != nullptr)
    {
        CMP->InvertRelation();
        IRF->CreateBranch(Cond, LoopEnd.get());
    }
    else
    {
        auto CmpEQ =
            IRF->CreateCmp(CompareInstruction::EQ, Cond, IRF->GetConstant((uint64_t)0));
        IRF->CreateBranch(CmpEQ, LoopEnd.get());
    }

    IRF->InsertBB(std::move(LoopBody));
    Body->IRCodegen(IRF);

    IRF->CreateJump(LoopIncrement.get());
    IRF->InsertBB(std::move(LoopIncrement));
    IRF->GetLoopIncrementBBsTable().pop_back();

    Increment->IRCodegen(IRF);
    IRF->CreateJump(HeaderPtr);

    IRF->GetBreakEndBBsTable().pop_back();
    IRF->InsertBB(std::move(LoopEnd));

    return nullptr;
}

Value *ContinueStatement::IRCodegen(IRFactory *IRF)
{
    return IRF->CreateJump(IRF->GetLoopIncrementBBsTable().back());
}

Value *BreakStatement::IRCodegen(IRFactory *IRF)
{
    assert(IRF->GetBreakEndBBsTable().size() > 0);
    return IRF->CreateJump(IRF->GetBreakEndBBsTable().back());
}

Value *ReturnStatement::IRCodegen(IRFactory *IRF)
{
    auto RetNum = IRF->GetCurrentFunction()->GetReturnNumber();
    IRF->GetCurrentFunction()->SetReturnNumber(RetNum - 1);

    bool HasRetVal =
        ReturnValue.has_value() && !IRF->GetCurrentFunction()->IsReturnTypeVoid();

    Value *RetVal = HasRetVal ? ReturnValue.value()->IRCodegen(IRF) : nullptr;

    if (IRF->GetCurrentFunction()->HasMultipleReturn())
    {
        if (HasRetVal)
            IRF->CreateStore(RetVal, IRF->GetCurrentFunction()->GetReturnValue());

        return IRF->CreateJump(nullptr);
    }

    return IRF->CreateRet(RetVal);
}

Value *FunctionDeclaration::IRCodegen(IRFactory *IRF)
{
    IRF->SetGlobalScope(false);

    IRType ReturnType;
    IRType ParamType;
    std::unique_ptr<FunctionParameter> ImplicitStructPtr {nullptr};
    bool NeedIgnore = false;

    switch (FuncType.GetReturnType())
    {
        case Type::Composite:
            if (FuncType.IsStruct())
            {
                ReturnType = GetIRTypeFromASTType(FuncType);

                // incase the struct is too big to pass by value
                auto ReturnTypeSize = ReturnType.GetByteSize() * 8;
                auto ABIMaxStructSize =
                    IRF->GetTargetMachine()->GetABI()->GetMaxStructSizePassedByValue();
                if (!ReturnType.IsPointer() && ReturnTypeSize > ABIMaxStructSize)
                {
                    NeedIgnore = true;
                    ParamType  = ReturnType;
                    ParamType.IncrementPointerLevel();

                    // then the return type is void and the struct to be returned will be
                    // allocated by the callers and a pointer is passed to the function
                    // as an extra argument
                    ReturnType = IRType(IRType::None);

                    // on the same note create the extra struct pointer operand
                    auto ParamName = "struct." + ParamType.GetStructName();
                    ImplicitStructPtr =
                        std::make_unique<FunctionParameter>(ParamName, ParamType, true);
                }
            }
            else
                assert(!"Unhandled Return Type.");
            break;

        case Type::Char: ReturnType = IRType(IRType::SInt, 8); break;
        case Type::UnsignedChar: ReturnType = IRType(IRType::UInt, 8); break;
        case Type::Short: ReturnType = IRType(IRType::SInt, 16); break;
        case Type::UnsignedShort: ReturnType = IRType(IRType::UInt, 16); break;
        case Type::Int: ReturnType = IRType(IRType::SInt); break;
        case Type::UnsignedInt: ReturnType = IRType(IRType::UInt); break;
        case Type::Long: ReturnType = IRType(IRType::SInt, 64); break;
        case Type::UnsignedLong: ReturnType = IRType(IRType::UInt, 64); break;
        case Type::LongLong: ReturnType = IRType(IRType::SInt, 64); break;
        case Type::UnsignedLongLong: ReturnType = IRType(IRType::UInt, 64); break;
        case Type::Double: ReturnType = IRType(IRType::FP, 64); break;
        case Type::Void: ReturnType = IRType(IRType::None); break;

        default: assert(!"Invalid function return type."); break;
    }

    IRF->CreateNewFunction(Name, ReturnType);
    IRF->GetCurrentFunction()->SetReturnNumber(ReturnNumber);

    if (Body == nullptr)
    {
        IRF->GetCurrentFunction()->SetToDeclarationOnly();
        return nullptr;
    }

    if (ImplicitStructPtr)
    {
        IRF->AddToSymbolTable(ImplicitStructPtr->GetName(), ImplicitStructPtr.get());
        IRF->Insert(std::move(ImplicitStructPtr));
    }

    for (auto &Argument : Arguments)
        Argument->IRCodegen(IRF);

    if (NeedIgnore)
    {
        auto CS = dynamic_cast<CompoundStatement *>(Body.get());
        assert(CS);

        for (auto &Stmt : CS->GetStatements())
        {
            if (Stmt->IsRet())
            {
                auto RetStmt = dynamic_cast<ReturnStatement *>(Stmt.get());
                auto RefExpr =
                    dynamic_cast<ReferenceExpression *>(RetStmt->GetReturnVal().get());

                if (RefExpr)
                {
                    IRF->GetCurrentFunction()->SetIgnorableStructName(
                        RefExpr->GetIdentifier());
                }
            }
        }
    }

    // if there are multiple returns, then create a local variable on the stack
    // which will hold the different return values
    auto HasMultipleReturn = ReturnNumber > 1 && !ReturnType.IsVoid();
    if (HasMultipleReturn)
        IRF->GetCurrentFunction()->SetReturnValue(
            IRF->CreateSA(Name + ".return", ReturnType));

    Body->IRCodegen(IRF);

    // patching JUMP -s with nullptr destination to make them point to the last BB
    if (HasMultipleReturn)
    {
        auto BBName   = Name + "_end";
        auto RetBB    = std::make_unique<BasicBlock>(BBName, IRF->GetCurrentFunction());
        auto RetBBPtr = RetBB.get();
        IRF->InsertBB(std::move(RetBB));
        auto RetVal = IRF->GetCurrentFunction()->GetReturnValue();
        auto LD     = IRF->CreateLoad(RetVal->GetType(), RetVal);
        IRF->CreateRet(LD);

        for (auto &BB : IRF->GetCurrentFunction()->GetBasicBlocks())
            for (auto &Instr : BB->GetInstructions())
                if (auto Jump = dynamic_cast<JumpInstruction *>(Instr.get());
                    Jump && Jump->GetTargetBB() == nullptr)
                {
                    Jump->SetTargetBB(RetBBPtr);
                }
    }

    // if its a void function without return statement, then add one.
    if (ReturnNumber == 0 && ReturnType.IsVoid())
        IRF->CreateRet(nullptr);

    return nullptr;
}

Value *FunctionParameterDeclaration::IRCodegen(IRFactory *IRF)
{
    auto ParamType     = GetIRTypeFromASTType(Ty);
    auto ParamTypeSize = ParamType.GetByteSize() * 8;
    auto ABIMaxStructSize =
        IRF->GetTargetMachine()->GetABI()->GetMaxStructSizePassedByValue();

    // if the param is a struct and too big to passed by value then change it
    // to a struct pointer, because that is how it will be passed by callers
    if (ParamType.IsStruct() && !ParamType.IsPointer() &&
        (ParamTypeSize > ABIMaxStructSize))
    {
        ParamType.IncrementPointerLevel();
    }

    auto Param = std::make_unique<FunctionParameter>(Name, ParamType);
    auto SA    = IRF->CreateSA(Name, ParamType);

    IRF->AddToSymbolTable(Name, SA);
    IRF->CreateStore(Param.get(), SA);
    IRF->Insert(std::move(Param));

    return nullptr;
}

Value *VariableDeclaration::IRCodegen(IRFactory *IRF)
{
    auto VarType = GetIRTypeFromASTType(AType);

    // If an array type, then change type to reflect this.
    if (AType.IsArray())
        VarType.SetDimensions(AType.GetDimensions());

    // If we are in global scope, then its a global variable Declaration
    std::vector<uint64_t> InitList;
    if (IRF->IsGlobalScope())
    {
        // if the initialization is done by an initializer.
        // FIXME: assuming 2 dimensional init list like
        // int a[3][2] =  "{{ 1, 2 }, {3, 4}, {5, 6}}",
        // add support aribitrary dimension.
        // There code should be more simplicity.

        auto HandleConstantExpr = [](auto &&Expr, auto &&InitList) {
            if (auto ConstantExpr = dynamic_cast<IntegerLiteralExpression *>(Expr.get());
                ConstantExpr != nullptr)
            {
                InitList.push_back(ConstantExpr->GetUIntValue());
            }
        };
        auto HandleIntegerLiteralExpr = [&](auto &&Expr, auto &&InitList) {
            HandleConstantExpr(Expr, InitList);
            if (auto InitListExpr = dynamic_cast<InitializerListExpression *>(Expr.get());
                InitListExpr != nullptr)
            {
                for (auto &Expr : InitListExpr->GetExprList())
                {
                    HandleConstantExpr(Expr, InitList);
                    // TODO: To add support aribitrary dimension, must recursive call.
                    // But lambda don't support recursive self.
                    // HandleIntegerLiteralExpr(Expr, InitList);
                }
            }
        };

        if (auto InitListExpr = dynamic_cast<InitializerListExpression *>(Init.get());
            InitListExpr != nullptr)
        {
            for (auto &Expr : InitListExpr->GetExprList())
            {
                HandleIntegerLiteralExpr(Expr, InitList);
            }
        }
        else
        {
            HandleConstantExpr(Init, InitList);
        }

        return IRF->CreateGlobalVar(Name, VarType, std::move(InitList));
    }

    if (IRF->GetCurrentFunction()->GetIgnorableStructVarName() == Name)
    {
        auto &Parameters = IRF->GetCurrentFunction()->GetParameters();
        auto ParamValue  = Parameters[Parameters.size() - 1].get();

        IRF->AddToSymbolTable(Name, ParamValue);

        return ParamValue;
    }

    /// Othewise we are in a local scope of a function.
    /// Allocate space on stack and update the local symbol Table.
    auto SA = IRF->CreateSA(Name, VarType);

    if (Init)
    {
        if (auto InitListExpr = dynamic_cast<InitializerListExpression *>(Init.get());
            InitListExpr != nullptr)
        {
            unsigned LoopCounter = 0;
            for (auto &Expr : InitListExpr->GetExprList())
            {
                if (auto ConstantExpr =
                        dynamic_cast<IntegerLiteralExpression *>(Expr.get());
                    ConstantExpr != nullptr)
                {
                    // basically storing each entry to the right stack area
                    // TODO: problematic for big arrays, Clang and GCC create a global
                    // array to store there the initial values and use memcopy

                    auto ResultType = SA->GetType();
                    ResultType.ReduceDimension();

                    if (ResultType.GetPointerLevel() == 0)
                        ResultType.IncrementPointerLevel();

                    auto GEP = IRF->CreateGEP(ResultType,
                                              SA,
                                              IRF->GetConstant((uint64_t)LoopCounter));

                    IRF->CreateStore(
                        IRF->GetConstant((uint64_t)ConstantExpr->GetUIntValue()),
                        GEP);
                }

                LoopCounter++;
            }
        }
        else
        {
            auto InitExpr = Init->IRCodegen(IRF);
            if (InitExpr->GetType().IsStruct())
                IRF->CreateMemCopy(SA, InitExpr, InitExpr->GetType().GetByteSize());
            else
                IRF->CreateStore(InitExpr, SA);
        }
    }

    IRF->AddToSymbolTable(Name, SA);

    return SA;
}

Value *MemberDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *StructDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *EnumDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *CallExpression::IRCodegen(IRFactory *IRF)
{
    std::vector<Value *> Args;
    auto MaxStructSize =
        IRF->GetTargetMachine()->GetABI()->GetMaxStructSizePassedByValue();

    for (auto &Arg : Arguments)

    {
        auto ArgIR   = Arg->IRCodegen(IRF);
        auto ArgIRTy = ArgIR->GetTypeRef();
        auto ArgTy   = Arg->GetResultType();

        auto IRTySize = ArgIRTy.GetByteSize() * 8;

        // if the generated IR result is a struct pointer, but the actual function
        // expects a struct by value, then issue an extra load
        if (ArgIR->GetTypeRef().IsStruct() && ArgIRTy.IsPointer() && ArgTy.IsStruct() &&
            !ArgTy.IsPointerType())
        {
            if (!(IRTySize > MaxStructSize))
                ArgIR = IRF->CreateLoad(ArgIR->GetType(), ArgIR);
        }

        // if the pointers level does not match then issune loads until it will
        while ((ArgTy.IsPointerType() && ArgIR->GetTypeRef().IsPointer() &&
                (ArgTy.GetPointerLevel() < ArgIR->GetTypeRef().GetPointerLevel())))
        {
            ArgIR = IRF->CreateLoad(ArgIR->GetType(), ArgIR);
        }

        Args.push_back(ArgIR);
    }

    auto ReturnType = GetResultType().GetReturnType();

    IRType ReturnIRType;
    StackAllocationInstruction *StructTemp {nullptr};
    bool IsRetChanged {false};
    int ImplicitStructIndex = -1;

    switch (ReturnType)
    {
        case Type::Int: ReturnIRType = IRType(IRType::SInt); break;
        case Type::UnsignedInt: ReturnIRType = IRType(IRType::UInt); break;
        case Type::Long: ReturnIRType = IRType(IRType::SInt, 64); break;
        case Type::UnsignedLong: ReturnIRType = IRType(IRType::UInt, 64); break;
        case Type::LongLong: ReturnIRType = IRType(IRType::SInt, 64); break;
        case Type::UnsignedLongLong: ReturnIRType = IRType(IRType::UInt, 64); break;
        case Type::Double: ReturnIRType = IRType(IRType::FP, 64); break;
        case Type::Void: ReturnIRType = IRType(IRType::None, 0); break;
        case Type::Composite: {
            ReturnIRType = GetIRTypeFromASTType(GetResultType());

            // If the return type is a struct, then also make a stack allocation
            // to use that as a temporary, where the result would be copied to after
            // the call
            StructTemp = IRF->CreateSA(Name + ".temp", ReturnIRType);

            // check if the call expression is returning a non pointer struct which is
            // to big to be returned back. In this case the called function were
            // already changed to expect an extra struct pointer parameter and use
            // that and also its no longer returning anything, it returns type now
            // void In this case we need to
            //  -allocating space for the struct, which actually do not needed since
            //   at this point its already done above (StructTemp)
            //  -adding extra parameter which is a pointer to this allocated struct
            //  -change the returned value to this newly allocated struct pointer
            //  (even though nothing is returned, doing this so subsequent
            //  instructions can use this struct instead)
            //
            //  FIXME: maybe an extra load will required since its now a struct
            //  pointer
            // but originally the return is a struct (not a pointer)
            auto ReturnIRTypeSize = ReturnIRType.GetByteSize() * 8;

            if (!(!ReturnIRType.IsPointer() && (ReturnIRTypeSize) > MaxStructSize))
                break;    // actually checking the opposite and break if it is true

            IsRetChanged        = true;
            ImplicitStructIndex = Args.size();
            Args.push_back(StructTemp);
            ReturnIRType = IRType::None;

            break;
        }
        default: assert(!"Unreachable"); break;
    }

    assert(!ReturnIRType.IsInvalid() && "Must be a valid type.");

    // in case if the ret type was a struct, so StructTemp not nullptr
    if (StructTemp)
    {
        // make the call
        auto CallRes = IRF->CreateCall(Name, Args, ReturnIRType, ImplicitStructIndex);
        // issue a store using the freshly allocated temporary StructTemp if needed
        if (!IsRetChanged)
            IRF->CreateStore(CallRes, StructTemp);
        return StructTemp;
    }

    return IRF->CreateCall(Name, Args, ReturnIRType);
}

Value *ReferenceExpression::IRCodegen(IRFactory *IRF)
{
    auto Local = IRF->GetSymbolValue(Identifier);

    if (Local && this->GetResultType().IsStruct())
        return Local;

    if (Local)
    {
        if (GetLValueness())
            return Local;
        else
            return IRF->CreateLoad(Local->GetType(), Local);
    }

    auto GV = IRF->GetGlobalVar(Identifier);
    assert(GV && "Cannot be null.");

    // If Lvalue, then return as a ptr to the global value.
    if (GetLValueness() || this->GetResultType().IsStruct())
        return GV;

    return IRF->CreateLoad(GV->GetType(), GV);
}

/// If used as RValue:
/// # calc Index expression.
///  mul  $finalIndex, $Index, sizeof(Array[0])
///  load $rv, [$arr + $finalIndex]

/// If used as LValue:
/// # generate Index:
///  mul $offset, $Index, sizeof(Array[0])
///  store [$arrayBase + $offset], $R
Value *ArrayExpression::IRCodegen(IRFactory *IRF)
{
    assert(BaseExpression && "BaseExpression cannot be Null");
    assert(IndexExpression && "IndexExpression cannot be Null");

    auto BaseValue  = BaseExpression->IRCodegen(IRF);
    auto IndexValue = IndexExpression->IRCodegen(IRF);

    auto ResultType = BaseValue->GetType();

    if (ResultType.IsPointer() && !ResultType.IsArray())
    {
        // if the base value is on the stack
        if (auto SA = dynamic_cast<StackAllocationInstruction *>(BaseValue);
            SA != nullptr)
        {
            BaseValue = IRF->CreateLoad(ResultType, SA);
            ResultType.DecrementPointerLevel();
        }
    }
    else
    {
        ResultType.ReduceDimension();

        if (ResultType.GetPointerLevel() == 0)
            ResultType.IncrementPointerLevel();
    }

    auto GEP = IRF->CreateGEP(ResultType, BaseValue, IndexValue);

    if (!GetLValueness() && ResultType.GetDimensions().size() == 0)
        return IRF->CreateLoad(ResultType, GEP);

    return GEP;
}

Value *ImplicitCastExpression::IRCodegen(IRFactory *IRF)
{
    auto SourceTypeVariant = CastableExpression->GetResultType().GetTypeVariant();
    auto DestTypeVariant   = GetResultType().GetTypeVariant();

    if (CastableExpression->GetResultType().IsArray() && GetResultType().IsPointerType())
    {
        assert(SourceTypeVariant == DestTypeVariant);

        auto RefExpr = dynamic_cast<ReferenceExpression *>(CastableExpression.get());
        assert(RefExpr);

        auto ReferID = RefExpr->GetIdentifier();
        auto Res     = IRF->GetSymbolValue(ReferID);

        if (!Res)
            Res = IRF->GetGlobalVar(ReferID);
        assert(Res);

        auto GEP = IRF->CreateGEP(GetIRTypeFromASTType(GetResultType()),
                                  Res,
                                  IRF->GetConstant((uint64_t)0));

        return GEP;
    }
    auto Val = CastableExpression->IRCodegen(IRF);

    if (Type::OnlySignednessDifference(SourceTypeVariant, DestTypeVariant))
        return Val;

    switch (SourceTypeVariant)
    {
        case Type::Char: {
            if (DestTypeVariant == Type::Int)
                return IRF->CreateSExt(Val, 32);

            if (DestTypeVariant == Type::UnsignedInt)
                return IRF->CreateZExt(Val, 32);

            if (DestTypeVariant == Type::Long || DestTypeVariant == Type::LongLong)
                return IRF->CreateSExt(Val, 64);

            if (DestTypeVariant == Type::UnsignedLong ||
                DestTypeVariant == Type::UnsignedLongLong)
                return IRF->CreateZExt(Val, 64);

            assert(!"Invalid conversion.");
        }

        case Type::UnsignedChar: {
            if (DestTypeVariant == Type::Int || DestTypeVariant == Type::UnsignedInt)
                return IRF->CreateZExt(Val, 32);

            if (DestTypeVariant == Type::Long || DestTypeVariant == Type::LongLong ||
                DestTypeVariant == Type::UnsignedLong ||
                DestTypeVariant == Type::UnsignedLongLong)

                return IRF->CreateZExt(Val, 64);

            assert(!"Invalid conversion.");
        }
        case Type::Int: {
            if (DestTypeVariant == Type::Double)
                return IRF->CreateIntToFloat(Val, 32);

            if ((DestTypeVariant == Type::Char || DestTypeVariant == Type::UnsignedChar))
                return IRF->CreateTrunc(Val, 8);

            if (DestTypeVariant == Type::Long || DestTypeVariant == Type::LongLong)
                return IRF->CreateSExt(Val, 64);

            if (DestTypeVariant == Type::UnsignedLong ||
                DestTypeVariant == Type::UnsignedLongLong)

                return IRF->CreateZExt(Val, 64);

            assert(!"Invalid conversion.");
        }

        case Type::UnsignedInt: {
            if (DestTypeVariant == Type::Double)
                return IRF->CreateIntToFloat(Val, 32);

            if ((DestTypeVariant == Type::Char || DestTypeVariant == Type::UnsignedChar))
                return IRF->CreateTrunc(Val, 8);

            if (DestTypeVariant == Type::Long || DestTypeVariant == Type::LongLong ||
                DestTypeVariant == Type::UnsignedLong ||
                DestTypeVariant == Type::UnsignedLongLong)

                return IRF->CreateZExt(Val, 64);

            assert(!"Invalid conversion.");
        }

        case Type::Long:
        case Type::LongLong: {
            if ((DestTypeVariant == Type::Char || DestTypeVariant == Type::UnsignedChar))
                return IRF->CreateTrunc(Val, 8);

            if ((DestTypeVariant == Type::Int || DestTypeVariant == Type::UnsignedInt))
                return IRF->CreateTrunc(Val, 32);

            assert(!"Invalid conversion.");
        }

        case Type::UnsignedLong:
        case Type::UnsignedLongLong: {
            if ((DestTypeVariant == Type::Char || DestTypeVariant == Type::UnsignedChar))
                return IRF->CreateTrunc(Val, 8);

            if ((DestTypeVariant == Type::Int || DestTypeVariant == Type::UnsignedInt))
                return IRF->CreateTrunc(Val, 32);

            assert(!"Invalid conversion.");
        }

        case Type::Double: {
            if (DestTypeVariant == Type::Int)
                return IRF->CreateFloatToInt(Val, 64);

            assert(!"Invalid conversion.");
        }


        default: assert(!"Invalid conversion.");
    }

    return nullptr;
}

Value *StructMemberReference::IRCodegen(IRFactory *IRF)
{
    assert(StructTypedExpression && "cannot be Null");

    auto BaseValue = StructTypedExpression->IRCodegen(IRF);
    auto ExprType  = BaseValue->GetType();

    assert(BaseValue && "Cannot be null.");
    assert(ExprType.IsStruct());
    assert(ExprType.GetMemberTypes().size() > MemberIndex);

    auto IndexValue = IRF->GetConstant((uint64_t)MemberIndex);

    // The result type is a pointer to the member type.
    // Ex:
    //    referred member is an i32 than an i32*.
    auto ResultType = ExprType.GetMemberTypes()[MemberIndex];
    ResultType.IncrementPointerLevel();

    auto BaseType = BaseValue->GetType();
    while (BaseType.GetPointerLevel() > 1)
    {
        BaseValue = IRF->CreateLoad(BaseType, BaseValue);
        BaseType  = BaseValue->GetType();
    }

    auto GEP = IRF->CreateGEP(ResultType, BaseValue, IndexValue);

    if (GetLValueness())
        return GEP;

    auto ResultIRType = GetIRTypeFromASTType(this->GetResultType());

    return IRF->CreateLoad(ResultIRType, GEP);
}

Value *StructInitExpression::IRCodegen(IRFactory *IRF)
{
    /// Allocate stack for struct first
    auto IRResultType = GetIRTypeFromASTType(this->ResultType);
    auto StructTemp   = IRF->CreateSA(ResultType.GetName() + ".temp", IRResultType);

    unsigned LoopIndex = 0;
    for (auto &InitExpr : InitValues)
    {
        auto InitExprCode = InitExpr->IRCodegen(IRF);
        auto MemberIndex  = MemberOrdering[LoopIndex];
        auto ResultType   = IRResultType.GetMemberTypes()[MemberIndex];

        ResultType.IncrementPointerLevel();

        auto MemberPtr = IRF->CreateGEP(ResultType,
                                        StructTemp,
                                        IRF->GetConstant((uint64_t)MemberIndex));

        IRF->CreateStore(InitExprCode, MemberPtr);
        ++LoopIndex;
    }

    return StructTemp;
}

Value *IntegerLiteralExpression::IRCodegen(IRFactory *IRF)
{
    return IRF->GetConstant(IntValue);
}

Value *FloatLiteralExpression::IRCodegen(IRFactory *IRF)
{
    return IRF->GetConstant(FPValue);
}

Value *UnaryExpression::IRCodegen(IRFactory *IRF)
{
    Value *E {nullptr};

    if (GetOperationKind() != Address && GetOperationKind() != Minus)
        E = Expr->IRCodegen(IRF);

    switch (GetOperationKind())
    {
        case UnaryOperation::Address: {
            auto RefExpr = dynamic_cast<ReferenceExpression *>(Expr.get());
            assert(RefExpr);

            auto ReferEE = RefExpr->GetIdentifier();
            auto Res     = IRF->GetSymbolValue(ReferEE);

            if (!Res)
            {
                Res = IRF->GetGlobalVar(ReferEE);
                Res->GetTypeRef().IncrementPointerLevel();
            }

            return Res;
        }

        case UnaryOperation::DeRef: {
            auto ResultType = E->GetType();
            return IRF->CreateLoad(ResultType, E);
        }

        case UnaryExpression::Minus: {
            if (auto ConstantExpr = dynamic_cast<IntegerLiteralExpression *>(Expr.get());
                ConstantExpr != nullptr)
            {
                // set the opposite of this value.
                ConstantExpr->SetValue(-ConstantExpr->GetSIntValue());
                return Expr->IRCodegen(IRF);
            }

            E = Expr->IRCodegen(IRF);

            return IRF->CreateSub(IRF->GetConstant((uint64_t)0), E);
        }
        case UnaryOperation::Not: {
            // goal IR:
            //    # E generated here
            //    sa $result
            //    str [$result], 1
            //    cmp.eq $c1, $E, 0
            //    br $c1, <end>
            // <true>
            //    str [$result], 0
            //    j <end>
            // <end>

            const auto FuncPtr = IRF->GetCurrentFunction();
            auto TrueBB        = std::make_unique<BasicBlock>("not_op_true", FuncPtr);
            auto FinalBB       = std::make_unique<BasicBlock>("not_op_final", FuncPtr);

            // LHS
            auto Result = IRF->CreateSA("result", IRType::CreateBool());
            // default true
            IRF->CreateStore(IRF->GetConstant((uint64_t)1), Result);

            if (auto LCMP = dynamic_cast<CompareInstruction *>(E); LCMP != nullptr)
            {
                LCMP->InvertRelation();
                IRF->CreateBranch(E, FinalBB.get());
            }
            else
            {
                auto LHSTest = IRF->CreateCmp(CompareInstruction::EQ,
                                              E,
                                              IRF->GetConstant((uint64_t)0));
                IRF->CreateBranch(LHSTest, FinalBB.get());
            }

            // true
            IRF->InsertBB(std::move(TrueBB));
            IRF->CreateStore(IRF->GetConstant((uint64_t)0), Result);
            IRF->CreateJump(FinalBB.get());

            IRF->InsertBB(std::move(FinalBB));


            // the result seems to be always rvalue so loading it.
            return IRF->CreateLoad(IRType::CreateBool(), Result);
        }

        case UnaryOperation::PostDecrement:
        case UnaryOperation::PostIncrement: {
            // make the assumption that the expression E is an LValue
            // which means its basically a pointer, so it
            // requires a load first for addition to work
            auto LoadedValType = E->GetTypeRef();
            LoadedValType.DecrementPointerLevel();

            auto LoadedExpr = IRF->CreateLoad(LoadedValType, E);

            Instruction *AddOrSub;
            if (GetOperationKind() == PostIncrement)
                AddOrSub = IRF->CreateAdd(LoadedExpr, IRF->GetConstant((uint64_t)1));
            else
                AddOrSub = IRF->CreateSub(LoadedExpr, IRF->GetConstant((uint64_t)1));

            IRF->CreateStore(AddOrSub, E);

            return LoadedExpr;
        }
        default: assert(!"Unimplemented");
    }

    return nullptr;
}

Value *BinaryExpression::IRCodegen(IRFactory *IRF)
{
    // TODO: simplify this, specially in case if there are actually multiple
    // logical operations like "a > 0 && a < 10 && a != 5"
    if (GetOperationKind() == LogicalAnd)
    {
        // goal IR:
        //    # L generated here
        //    sa $result
        //    cmp.eq $c1, $L, 0
        //    br $c1, <false>
        // <test_R>
        //    # R generated here
        //    cmp.eq $c2, $R, 0
        //    br $c2, <false>
        // <true>
        //    str [$result], 1
        //    j <end>
        // <false>
        //    str [$result], 0
        // <end>
        const auto FuncPtr = IRF->GetCurrentFunction();
        auto TestRhsBB     = std::make_unique<BasicBlock>("test_RHS", FuncPtr);
        auto TrueBB        = std::make_unique<BasicBlock>("true", FuncPtr);
        auto FalseBB       = std::make_unique<BasicBlock>("false", FuncPtr);
        auto FinalBB       = std::make_unique<BasicBlock>("final", FuncPtr);

        // LHS test
        auto Result = IRF->CreateSA("result", IRType::CreateBool());
        IRF->CreateStore(IRF->GetConstant((uint64_t)0), Result);

        auto L = Lhs->IRCodegen(IRF);

        // if L was a compare instruction then just revert its relation
        if (auto LCMP = dynamic_cast<CompareInstruction *>(L); LCMP != nullptr)
        {
            LCMP->InvertRelation();
            IRF->CreateBranch(L, FalseBB.get());
        }
        else
        {
            auto LHSTest =
                IRF->CreateCmp(CompareInstruction::EQ, L, IRF->GetConstant((uint64_t)0));
            IRF->CreateBranch(LHSTest, FalseBB.get());
        }

        // RHS test
        IRF->InsertBB(std::move(TestRhsBB));
        auto R = Rhs->IRCodegen(IRF);

        // If R was a compare instruction then just its relation.
        if (auto RCMP = dynamic_cast<CompareInstruction *>(R); RCMP != nullptr)
        {
            RCMP->InvertRelation();
            IRF->CreateBranch(R, FalseBB.get());
        }
        else
        {
            auto RHSTest =
                IRF->CreateCmp(CompareInstruction::EQ, R, IRF->GetConstant((uint64_t)0));
            IRF->CreateBranch(RHSTest, FalseBB.get());
        }

        // True
        IRF->InsertBB(std::move(TrueBB));
        IRF->CreateStore(IRF->GetConstant((uint64_t)1), Result);
        IRF->CreateJump(FinalBB.get());

        // False
        IRF->InsertBB(std::move(FalseBB));
        IRF->CreateStore(IRF->GetConstant((uint64_t)0), Result);
        IRF->CreateJump(FinalBB.get());

        IRF->InsertBB(std::move(FinalBB));

        // the result seems to be always an rvalue so loading it also
        return IRF->CreateLoad(IRType::CreateBool(), Result);
    }

    if (GetOperationKind() == Assign)
    {
        auto R = Rhs->IRCodegen(IRF);
        auto L = Lhs->IRCodegen(IRF);

        if (L == nullptr || R == nullptr)
            return nullptr;

        if (R->GetTypeRef().IsStruct())
            IRF->CreateMemCopy(L, R, R->GetTypeRef().GetByteSize());
        else
            IRF->CreateStore(R, L);

        return R;
    }

    if (GetOperationKind() == AddAssign || GetOperationKind() == SubAssign ||
        GetOperationKind() == MulAssign || GetOperationKind() == DivAssign)
    {
        // Assignment right associative so generate R first
        auto R = Rhs->IRCodegen(IRF);
        auto L = Lhs->IRCodegen(IRF);

        if (L == nullptr || R == nullptr)
            return nullptr;

        if (R->GetTypeRef().IsStruct())
        {
            IRF->CreateMemCopy(L, R, R->GetTypeRef().GetByteSize());
        }
        else
        {
            Instruction *OperationResult {nullptr};

            switch (GetOperationKind())
            {
                case AddAssign: OperationResult = IRF->CreateAdd(L, R); break;
                case SubAssign: OperationResult = IRF->CreateSub(L, R); break;
                case MulAssign: OperationResult = IRF->CreateMul(L, R); break;
                case DivAssign: OperationResult = IRF->CreateDiv(L, R); break;
                default: assert(!"Unreachable");
            }

            auto Load = dynamic_cast<LoadInstruction *>(L);
            assert(Load);

            IRF->CreateStore(OperationResult, Load->GetMemoryLocation());

            return OperationResult;
        }
    }


    auto L = Lhs->IRCodegen(IRF);
    auto R = Rhs->IRCodegen(IRF);

    if (L == nullptr || R == nullptr)
        return nullptr;


    if (L->IsConstant() && !R->IsConstant())
    {
        // and if its a commutative operation
        switch (GetOperationKind())
        {
            // then swap the operands, since most architecture supports immediate
            // as the last operand.
            // Ex.:
            //      AArch64 add x0, x1, #123 not add x0, #123, x1
            case Add:
            case Mul:
            case And:
            case Xor:
            case Equal:
            case NotEqual: std::swap(L, R); break;
            default: break;
        }
    }

    if (L->IsConstant())
        L = IRF->CreateMov(L, R->GetBitWidth());

    switch (GetOperationKind())
    {
        case LSL: return IRF->CreateLSL(L, R);
        case LSR: return IRF->CreateLSR(L, R);
        case Add: return IRF->CreateAdd(L, R);
        case Sub: return IRF->CreateSub(L, R);
        case Mul: return IRF->CreateMul(L, R);
        case Div: return IRF->CreateDiv(L, R);
        case Mod: return IRF->CreateMod(L, R);
        case And: return IRF->CreateAnd(L, R);
        case Xor: return IRF->CreateXOr(L, R);
        case DivU: return IRF->CreateDivU(L, R);
        case ModU: return IRF->CreateModU(L, R);
        case Equal: return IRF->CreateCmp(CompareInstruction::EQ, L, R);
        case Less: return IRF->CreateCmp(CompareInstruction::LT, L, R);
        case Greater: return IRF->CreateCmp(CompareInstruction::GT, L, R);
        case NotEqual: return IRF->CreateCmp(CompareInstruction::NE, L, R);
        case GreaterEqual: return IRF->CreateCmp(CompareInstruction::GE, L, R);
        case LessEqual: return IRF->CreateCmp(CompareInstruction::LE, L, R);

        default: assert(!"Unhandled binary instruction type"); break;
    }
}

// goal IR:
//    # Condition generated here
//    sa $result
//    cmp.eq $c1, $Condition, 0
//    br $c1, <false>
// <true>
//    # ExprIfTrue generated here
//    str [$result], $ExprIfTrue
//    j <end>
// <false>
//    # ExprIfFalse generated here
//    str [$result], ExprIfFalse
//    j <end>
// <end>

Value *TernaryExpression::IRCodegen(IRFactory *IRF)
{
    const auto FuncPtr = IRF->GetCurrentFunction();

    auto TrueBB  = std::make_unique<BasicBlock>("tenary_true", FuncPtr);
    auto FalseBB = std::make_unique<BasicBlock>("tenary_false", FuncPtr);
    auto FinalBB = std::make_unique<BasicBlock>("tenary_end", FuncPtr);

    auto C = Condition->IRCodegen(IRF);

    // Condition Test

    // if L was a compare instruction then just revert its relation
    if (auto LCMP = dynamic_cast<CompareInstruction *>(C); LCMP != nullptr)
    {
        LCMP->InvertRelation();
        IRF->CreateBranch(C, FalseBB.get());
    }
    else
    {
        auto LHSTest =
            IRF->CreateCmp(CompareInstruction::EQ, C, IRF->GetConstant((uint64_t)0));
        IRF->CreateBranch(LHSTest, FalseBB.get());
    }

    // TRUE
    IRF->InsertBB(std::move(TrueBB));
    auto TrueExpr = ExprIfTrue->IRCodegen(IRF);
    auto Result   = IRF->CreateSA("result", TrueExpr->GetType());
    IRF->CreateStore(TrueExpr, Result);
    IRF->CreateJump(FinalBB.get());

    // FALSE
    IRF->InsertBB(std::move(FalseBB));
    IRF->CreateStore(ExprIfFalse->IRCodegen(IRF), Result);
    IRF->CreateJump(FinalBB.get());

    IRF->InsertBB(std::move(FinalBB));


    return IRF->CreateLoad(Result->GetType(), Result);
}

Value *TranslationUnit::IRCodegen(IRFactory *IRF)
{
    for (auto &Declaration : Declarations)
    {
        IRF->SetGlobalScope();
        if (auto Decl = Declaration->IRCodegen(IRF); Decl != nullptr)
            IRF->AddGlobalVariable(Decl);
    }

    return nullptr;
}

Type FunctionDeclaration::CreateType(const Type &t,
                                     const FunctionDeclaration::ParamVec &params)
{
    Type funcType(t);

    for (auto &Argument : params)
    {
        auto type = Argument->GetType();
        funcType.GetArgTypes().push_back(type);
    }

    // if there are no arguments then set it to void
    if (params.empty())
        funcType.GetArgTypes().push_back(Type::Void);

    return funcType;
}

BinaryExpression::BinaryOperation BinaryExpression::GetOperationKind()
{
    switch (Operation.GetKind())
    {
        case Token::Assign: return Assign;
        case Token::PlusEqual: return AddAssign;
        case Token::MinusEuqal: return SubAssign;
        case Token::MulEqual: return MulAssign;
        case Token::DivEqual: return DivAssign;
        case Token::LeftShift: return LSL;
        case Token::RightShift: return LSR;
        case Token::Plus: return Add;
        case Token::Minus: return Sub;
        case Token::Mul: return Mul;
        case Token::Div: {
            if (GetResultType().IsUnsigned())
                return DivU;
            return Div;
        }
        case Token::Mod: {
            if (GetResultType().IsUnsigned())
                return ModU;
            return Mod;
        }
        case Token::And: return And;
        case Token::Xor: return Xor;
        case Token::Not: return Not;
        case Token::Equal: return Equal;
        case Token::Less: return Less;
        case Token::Greater: return Greater;
        case Token::LessEqual: return LessEqual;
        case Token::GreaterEqual: return GreaterEqual;
        case Token::NotEqual: return NotEqual;
        case Token::LogicalAnd: return LogicalAnd;
        default: assert(false && "Invalid binary Operator kind."); break;
    }
}

UnaryExpression::UnaryOperation UnaryExpression::GetOperationKind()
{
    switch (Operation.GetKind())
    {
        case Token::And: return UnaryOperation::Address;
        case Token::Mul: return UnaryOperation::DeRef;
        case Token::Minus: return UnaryOperation::Minus;
        case Token::Not: return UnaryOperation::Not;
        case Token::Inc: return UnaryOperation::PostIncrement;
        case Token::Dec: return UnaryOperation::PostDecrement;

        default: assert(!"Invalid unary operator kind."); break;
    }
}

//=--------------------------------------------------------------------------=//
//=------------------------- AST Accept functions --------------------------=//
//=--------------------------------------------------------------------------=//

void VariableDeclaration::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitVariableDeclaration(this);
}

void MemberDeclaration::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitMemberDeclaration(this);
}

void EnumDeclaration::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitEnumDeclaration(this);
}

void StructDeclaration::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitStructDeclaration(this);
}

void CompoundStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitCompoundStatement(this);
}

void ExpressionStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitExpressionStatement(this);
}

void IfStatement::Accept(ASTVisitor *Visitor) const { Visitor->VisitIfStatement(this); }

void SwitchStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitSwitchStatement(this);
}

void WhileStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitWhileStatement(this);
}

void ForStatement::Accept(ASTVisitor *Visitor) const { Visitor->VisitForStatement(this); }

void ReturnStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitReturnStatement(this);
}

void BreakStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitBreakStatement(this);
}

void ContinueStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitContinueStatement(this);
}

void FunctionParameterDeclaration::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitFunctionParameterDeclaration(this);
}

void FunctionDeclaration::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitFunctionDeclaration(this);
}

void BinaryExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitBinaryExpression(this);
}

void UnaryExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitUnaryExpression(this);
}

void TernaryExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitTernaryExpression(this);
}

void StructMemberReference::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitStructMemberReference(this);
}

void StructInitExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitStructInitExpression(this);
}

void CallExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitCallExpression(this);
}

void ReferenceExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitReferenceExpression(this);
}

void IntegerLiteralExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitIntegerLiteralExpression(this);
}

void FloatLiteralExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitFloatLiteralExpression(this);
}

void ArrayExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitArrayExpression(this);
}

void ImplicitCastExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitImplicitCastExpression(this);
}

void InitializerListExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitInitializerListExpression(this);
}

void TranslationUnit::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitTranslationUnit(this);
}
