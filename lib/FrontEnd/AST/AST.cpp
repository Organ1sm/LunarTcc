#include "FrontEnd/AST/AST.hpp"
#include "FrontEnd/AST/ASTVistor.hpp"
#include "FrontEnd/Support.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/IRType.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "Utils/DiagnosticPrinter.hpp"
#include "fmt/core.h"
#include <cassert>
#include <cstdint>
#include <memory>
#include <type_traits>

//=--------------------------------------------------------------------------=//
//=------------------------- Constructor  -----------------------------------=//
//=--------------------------------------------------------------------------=//

StructMemberReference::StructMemberReference(ExprPtr Expr,
                                             Token &Id,
                                             std::size_t Idx,
                                             bool Arrow)
    : StructTypedExpression(std::move(Expr)), MemberIdentifier(Id), MemberIndex(Idx),
      Arrow(Arrow)
{
    auto STEType = StructTypedExpression->GetResultType();

    if (MemberIndex < STEType.GetTypeList().size())
        this->ResultType = STEType.GetTypeList()[MemberIndex];
}

BinaryExpression::BinaryExpression(ExprPtr L, Token Op, ExprPtr R)
    : Lhs(std::move(L)), Operation(Op), Rhs(std::move(R))
{
    if (IsCondition())
        ResultType = Type(Type::Int);
    else
    {
        ResultType = Lhs->GetResultType();
    }
}

UnaryExpression::UnaryExpression(Token Op, ExprPtr E, bool PostFix)
    : Operation(Op), IsPostFix(PostFix)
{
    if (E)
        Expr = std::move(E);

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

        case UnaryOperation::BitWiseNot:
        case UnaryOperation::Not: ResultType = Type(Type::Int); break;
        case UnaryOperation::Minus:
        case UnaryOperation::PreIncrement:
        case UnaryOperation::PreDecrement:
        case UnaryOperation::PostIncrement:
        case UnaryOperation::PostDecrement: ResultType = Expr->GetResultType(); break;

        case UnaryOperation::Sizeof: {
            ResultType = Type(Type::UnsignedInt);

            if (Expr)
                SizeOfType = Expr->GetResultType();

            break;
        }

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

static IRType GetIRTypeFromVK(Type::VariantKind VK, TargetMachine *TM)
{
    // the standard says this (6.2.5.27)
    // "A pointer to void shall have the same representation and alignment
    // requirements as a pointer to a character type."
    // Therefore treating void* as i8* as LLVM does this as well.
    // Although here the pointer part is not checked, only the base type. This is
    // okay, since non pointer void types should have been already sorted out as
    // an error by the parser.
    switch (VK)
    {
        case Type::Void:
        case Type::Char: return IRType(IRType::SInt, 8);
        case Type::UnsignedChar: return IRType(IRType::UInt, 8);
        case Type::Short: return IRType(IRType::SInt, 16);
        case Type::UnsignedShort: return IRType(IRType::UInt, 16);
        case Type::Int: return IRType(IRType::SInt, TM->GetIntSize());
        case Type::UnsignedInt: return IRType(IRType::UInt, TM->GetIntSize());
        case Type::Long: return IRType(IRType::SInt, TM->GetLongSize());
        case Type::UnsignedLong: return IRType(IRType::UInt, TM->GetLongSize());
        case Type::LongLong: return IRType(IRType::SInt, 64);
        case Type::UnsignedLongLong: return IRType(IRType::UInt, 64);
        case Type::Float: return IRType(IRType::FP, 32);
        case Type::Double: return IRType(IRType::FP, 64);
        case Type::Composite: return IRType(IRType::Struct);
        default: assert(!"Invalid type."); break;
    }
}

static IRType GetIRTypeFromASTType(Type CT, TargetMachine *TM)
{
    // TODO: This should be in the semantic check.
    assert((CT.GetTypeVariant() != Type::Void || CT.GetPointerLevel() != 0) &&
           "void type is only allowed to be a pointer");
    IRType Result = GetIRTypeFromVK(CT.GetTypeVariant(), TM);

    if (Result.IsStruct())
    {
        auto StructName = CT.GetName();
        Result.SetStructName(StructName);

        // convert each member AST type to IRType (recursive)
        for (auto &MemberASTType : CT.GetTypeList())
            Result.GetMemberTypes().push_back(GetIRTypeFromASTType(MemberASTType, TM));
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

    // If the condition is a compile time computable constant, then generate
    // then if or else body based on it.
    if (Cond->IsConstant())
    {
        assert(!Cond->IsFPType() && "Only support integer converted to boolean");

        // If the condition is a constant true value, then
        if (static_cast<Constant *>(Cond)->GetIntValue() != 0)
        {
            IfBody->IRCodegen(IRF);
        }
        else if (HaveElse)
        {
            ElseBody->IRCodegen(IRF);
        }
        else
            ;    // if no else then do nothing

        return nullptr;
    }

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
    for (auto &[CaseExpr, Statements] : Cases)
    {
        auto CaseConst =
            dynamic_cast<IntegerLiteralExpression *>(CaseExpr.get())->GetSIntValue();
        auto CmpRes = IRF->CreateCmp(CompareInstruction::EQ,
                                     Cond,
                                     IRF->GetConstant((uint64_t)CaseConst));
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

    bool IsEndlessLoop = false;
    /// If the condition is a compile time computable constant, then generate
    /// the if or else body.
    if (Cond->IsConstant())
    {
        assert(!Cond->IsFPType() && "Only support integer converted to boolean");

        // If the condition is a constant false value, then
        if (static_cast<Constant *>(Cond)->GetIntValue() == 0)
        {
            // remove loop_header
            IRF->EraseLastBB();

            // remove jump to loop_header
            IRF->EraseLastInst();

            return nullptr;
        }
        else
        {
            IsEndlessLoop = true;
        }
    }

    if (!IsEndlessLoop)
    {
        if (auto CMP = dynamic_cast<CompareInstruction *>(Cond); CMP != nullptr)
        {
            CMP->InvertRelation();
            IRF->CreateBranch(Cond, LoopEnd.get());
        }
        else
        {
            auto Cmp = IRF->CreateCmp(CompareInstruction::EQ,
                                      Cond,
                                      IRF->GetConstant((uint64_t)0));
            IRF->CreateBranch(Cmp, LoopEnd.get());
        }
    }


    if (!IsEndlessLoop)
        IRF->InsertBB(std::move(LoopBody));

    Body->IRCodegen(IRF);

    IRF->CreateJump(HeaderPtr);

    IRF->InsertBB(std::move(LoopEnd));
    IRF->GetBreakEndBBsTable().pop_back();

    return nullptr;
}

//  <loop_body>
//    # generate code for the Body
//    j <loop_header>
//  <loop_header>
//    # generate code for the Condition
//    br $condition, <loop_body>    # goto loop_body if condition true
//  <loop_end>
Value *DoWhileStatement::IRCodegen(IRFactory *IRF)
{
    const auto FuncPtr = IRF->GetCurrentFunction();
    auto LoopHeader    = std::make_unique<BasicBlock>("loop_header", FuncPtr);
    auto LoopBody      = std::make_unique<BasicBlock>("loop_body", FuncPtr);
    auto LoopEnd       = std::make_unique<BasicBlock>("loop_end", FuncPtr);

    auto LoopHeaderPtr = LoopHeader.get();
    auto LoopBodyPtr   = LoopBody.get();

    // jump into loop header
    IRF->CreateJump(LoopBodyPtr);

    // Generate Loop Body
    IRF->GetBreakEndBBsTable().push_back(LoopEnd.get());

    IRF->InsertBB(std::move(LoopBody));
    Body->IRCodegen(IRF);

    IRF->GetBreakEndBBsTable().pop_back();

    IRF->CreateJump(LoopHeaderPtr);

    // Generate LoopHeader
    IRF->InsertBB(std::move(LoopHeader));
    auto Cond = Condition->IRCodegen(IRF);

    bool IsEndlessLoop = false;
    /// If the condition is a compile time computable constant, then generate
    /// the if or else body.
    if (Cond->IsConstant())
    {
        assert(!Cond->IsFPType() && "Only support integer converted to boolean");

        // remove loop_header
        IRF->EraseLastBB();
        // remove jump to loop_header
        IRF->EraseLastInst();

        // If the condition is a constant false value, then
        if (static_cast<Constant *>(Cond)->GetIntValue() == 0)
            return nullptr;
        else
            IsEndlessLoop = true;
    }

    if (!IsEndlessLoop)
    {
        // Check that the condition is or not 0
        auto CMP =
            IRF->CreateCmp(CompareInstruction::NE, Cond, IRF->GetConstant((uint64_t)0));

        IRF->CreateBranch(CMP, LoopBodyPtr);
    }
    else
    {
        IRF->CreateJump(LoopBodyPtr);
    }

    if (!IsEndlessLoop)
        IRF->InsertBB(std::move(LoopEnd));

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

    // TODO: Handle all cases (only condition missing, only increment etc)
    if (!Init && VarDecls.empty() && !Condition && !Increment)
    {
        auto LoopBodyPtr = LoopBody.get();
        IRF->InsertBB(std::move(LoopBody));

        IRF->GetLoopIncrementBBsTable().push_back(LoopEnd.get());
        IRF->GetBreakEndBBsTable().push_back(LoopEnd.get());

        Body->IRCodegen(IRF);

        IRF->GetBreakEndBBsTable().pop_back();
        IRF->GetLoopIncrementBBsTable().pop_back();

        IRF->CreateJump(LoopBodyPtr);
        IRF->InsertBB(std::move(LoopEnd));

        return nullptr;
    }

    IRF->GetLoopIncrementBBsTable().push_back(LoopIncrement.get());
    IRF->GetBreakEndBBsTable().push_back(LoopEnd.get());

    // Generating code for the initializing expression or the variable declarations and
    // adding and explicit unconditional jump to the loop header basic block
    if (Init)
        Init->IRCodegen(IRF);
    else
    {
        for (auto &VarDecl : VarDecls)
            VarDecl->IRCodegen(IRF);
    }

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
    assert(!IRF->GetBreakEndBBsTable().empty());
    return IRF->CreateJump(IRF->GetBreakEndBBsTable().back());
}

Value *ReturnStatement::IRCodegen(IRFactory *IRF)
{
    auto RetNum = IRF->GetCurrentFunction()->GetReturnNumber();
    IRF->GetCurrentFunction()->SetReturnNumber(RetNum - 1);

    bool HasRetVal =
        ReturnValue.has_value() && !IRF->GetCurrentFunction()->IsReturnTypeVoid();

    Value *RetVal = HasRetVal ? ReturnValue.value()->IRCodegen(IRF) : nullptr;

    // Issue a load in this case
    // TODO: This need to be reworked. ReferenceExpression should not have
    // the struct handling and may the ImplicitCastExpression should be
    // extended to be able to cast LValues to RValues and generate that when
    // needed just like LLVM. So loads can be emitted there, not in
    // ReferenceExpression. Now it is done here and not in ReferenceExpression,
    // because otherwise GEPs would load the struct as well but they should not.
    if (RetVal && IRF->GetCurrentFunction()->IsReturnTypeStruct())
        RetVal = IRF->CreateLoad(IRF->GetCurrentFunction()->GetReturnType(), RetVal);

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
                ReturnType = GetIRTypeFromASTType(FuncType, IRF->GetTargetMachine());

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

        case Type::Int:
            ReturnType = IRType(IRType::SInt, IRF->GetTargetMachine()->GetIntSize());
            break;
        case Type::UnsignedInt:
            ReturnType = IRType(IRType::UInt, IRF->GetTargetMachine()->GetIntSize());
            break;

        case Type::Long:
            ReturnType = IRType(IRType::SInt, IRF->GetTargetMachine()->GetLongSize());
            break;
        case Type::UnsignedLong:
            ReturnType = IRType(IRType::UInt, IRF->GetTargetMachine()->GetLongSize());
            break;

        case Type::LongLong: ReturnType = IRType(IRType::SInt, 64); break;
        case Type::UnsignedLongLong: ReturnType = IRType(IRType::UInt, 64); break;

        case Type::Float: ReturnType = IRType(IRType::FP, 32); break;
        case Type::Double: ReturnType = IRType(IRType::FP, 64); break;

        case Type::Void: ReturnType = IRType(IRType::None); break;

        default: assert(!"Invalid function return type."); break;
    }

    auto NameStr = Name.GetString();
    IRF->CreateNewFunction(NameStr, ReturnType);
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
                    auto ID = RefExpr->GetIdentifier();
                    IRF->GetCurrentFunction()->SetIgnorableStructName(ID);
                }
            }
        }
    }

    // if there are multiple returns, then create a local variable on the stack
    // which will hold the different return values
    auto HasMultipleReturn = ReturnNumber > 1 && !ReturnType.IsVoid();
    if (HasMultipleReturn)
        IRF->GetCurrentFunction()->SetReturnValue(
            IRF->CreateSA(Name.GetString() + ".return", ReturnType));

    Body->IRCodegen(IRF);

    // patching JUMP -s with nullptr destination to make them point to the last BB
    if (HasMultipleReturn)
    {
        auto BBName   = Name.GetString() + "_end";
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
    auto ParamType     = GetIRTypeFromASTType(Ty, IRF->GetTargetMachine());
    auto ParamName     = Name.GetString();
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

    auto Param = std::make_unique<FunctionParameter>(ParamName, ParamType);
    auto SA    = IRF->CreateSA(ParamName, ParamType);

    IRF->AddToSymbolTable(ParamName, SA);
    IRF->CreateStore(Param.get(), SA);
    IRF->Insert(std::move(Param));

    return nullptr;
}

Value *VariableDeclaration::IRCodegen(IRFactory *IRF)
{
    auto VarType = GetIRTypeFromASTType(AType, IRF->GetTargetMachine());
    auto VarName = Name.GetString();

    // If an array type, then change type to reflect this.
    if (AType.IsArray())
        VarType.SetDimensions(AType.GetDimensions());

    auto HandleExprWithNull = [](auto &&Expr, auto &&Result, auto *ToConvertedExpr) {
        if (!Result)
        {
            auto CastedExpr = dynamic_cast<ImplicitCastExpression *>(Expr.get())
                                  ->GetCastableExpression()
                                  .get();

            if (std::is_same_v<decltype(ToConvertedExpr), IntegerLiteralExpression *>)
                assert(instanceof <IntegerLiteralExpression>(CastedExpr) &&
                                      "Only support int literals for now.");

            Result = dynamic_cast<decltype(ToConvertedExpr)>(CastedExpr);
        }
    };

    // If we are in global scope, then its a global variable Declaration
    std::vector<uint64_t> InitList;
    if (IRF->IsGlobalScope() || AType.IsArray())
    {
        // if the initialization is done by an initializer.
        // TODO: assuming 2 dimensional init list like
        // '''
        //    int a[3][2] =  "{{ 1, 2 }, {3, 4}, {5, 6}}",
        // '''
        // add support aribitrary dimension.
        // There code should be more simplicity.
        auto HandleConstantExpr = [&](auto &&Expr, auto &&InitList) {
            if (auto ConstantExpr = dynamic_cast<IntegerLiteralExpression *>(Expr.get());
                ConstantExpr != nullptr ||
                instanceof
                <ImplicitCastExpression>(Expr.get()))
            {
                IntegerLiteralExpression *To {nullptr};
                HandleExprWithNull(Expr, ConstantExpr, To);

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
            InitListExpr != nullptr ||
            instanceof
            <ImplicitCastExpression>(Init.get()))
        {
            InitializerListExpression *To {nullptr};
            HandleExprWithNull(Init, InitListExpr, To);

            for (auto &Expr : InitListExpr->GetExprList())
                HandleIntegerLiteralExpr(Expr, InitList);
        }
        else
        {
            HandleConstantExpr(Init, InitList);

            /// string literal case like: char *str = "Hello World"
            if (auto StringLitExpr = dynamic_cast<StringLiteralExpression *>(Init.get());
                StringLitExpr != nullptr)
            {
                // generate code for the string literal by create global variable for it.
                auto GlobalStr = StringLitExpr->IRCodegen(IRF);

                // increase to pointer level since now the pointer to the data
                // is stored and not data itself.
                VarType.IncrementPointerLevel();
                return IRF->CreateGlobalVar(VarName, VarType, GlobalStr);
            }
        }

        if (IRF->IsGlobalScope())
        {
            return IRF->CreateGlobalVar(VarName, VarType, std::move(InitList));
        }
        else if (Init)
        {
            assert(AType.IsArray());

            auto InitializerName = "__const." + IRF->GetCurrentFunction()->GetName() +
                                   "." + Name.GetString();
            auto InitializerGV =
                IRF->CreateGlobalVar(InitializerName, VarType, std::move(InitList));

            IRF->AddGlobalVariable(InitializerGV);

            auto SA = IRF->CreateSA(VarName, VarType);

            IRF->CreateMemCopy(
                SA,
                InitializerGV,
                InitializerGV->GetTypeRef().GetByteSize(IRF->GetTargetMachine()));

            IRF->AddToSymbolTable(VarName, SA);
            return SA;
        }
    }

    if (IRF->GetCurrentFunction()->GetIgnorableStructVarName() == VarName)
    {
        auto &Parameters = IRF->GetCurrentFunction()->GetParameters();
        auto ParamValue  = Parameters[Parameters.size() - 1].get();

        IRF->AddToSymbolTable(VarName, ParamValue);

        return ParamValue;
    }

    /// Othewise we are in a local scope of a function.
    /// Allocate space on stack and update the local symbol Table.
    auto SA = IRF->CreateSA(VarName, VarType);

    // TODO: clean up there
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
                    ConstantExpr != nullptr ||
                    instanceof
                    <ImplicitCastExpression>(Expr.get()))
                {
                    // basically storing each entry to the right stack area
                    // TODO: problematic for big arrays, Clang and GCC create a global
                    // array to store there the initial values and use memcopy

                    IntegerLiteralExpression *To {nullptr};
                    HandleExprWithNull(Expr, ConstantExpr, To);

                    auto ResultType = SA->GetType();
                    ResultType.ReduceDimension();

                    if (ResultType.GetPointerLevel() == 0)
                        ResultType.IncrementPointerLevel();

                    auto GEP = IRF->CreateGEP(ResultType,
                                              SA,
                                              IRF->GetConstant((uint64_t)LoopCounter));

                    const auto SizeOfConstExpr =
                        GetIRTypeFromASTType(ConstantExpr->GetResultType(),
                                             IRF->GetTargetMachine())
                            .GetBaseTypeByteSize(IRF->GetTargetMachine()) *
                        8;

                    IRF->CreateStore(
                        IRF->GetConstant((uint64_t)ConstantExpr->GetUIntValue(),
                                         SizeOfConstExpr),
                        GEP);
                }

                LoopCounter++;
            }
        }
        else
        {
            auto InitExpr = Init->IRCodegen(IRF);
            if (InitExpr->GetType().IsStruct() &&
                InitExpr->GetType().GetPointerLevel() == SA->GetType().GetPointerLevel())
                IRF->CreateMemCopy(SA,
                                   InitExpr,
                                   InitExpr->GetTypeRef().GetBaseTypeByteSize());
            else
                IRF->CreateStore(InitExpr, SA);
        }
    }

    IRF->AddToSymbolTable(VarName, SA);

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

        auto IRTySize = ArgIRTy.GetBaseTypeByteSize() * 8;

        // if the generated IR result is a struct pointer, but the actual function
        // expects a struct by value, then issue an extra load
        if (ArgIR->GetTypeRef().IsStruct() && ArgIRTy.IsPointer() && ArgTy.IsStruct() &&
            !ArgTy.IsPointerType())
        {
            if (IRTySize <= MaxStructSize)
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
    auto FuncName   = Name.GetString();

    IRType ReturnIRType;
    StackAllocationInstruction *StructTemp {nullptr};
    bool IsRetChanged {false};
    int ImplicitStructIndex = -1;

    switch (ReturnType)
    {
        case Type::Int:
            ReturnIRType = IRType(IRType::SInt, IRF->GetTargetMachine()->GetIntSize());
            break;
        case Type::UnsignedInt:
            ReturnIRType = IRType(IRType::UInt, IRF->GetTargetMachine()->GetIntSize());
            break;

        case Type::Long:
            ReturnIRType = IRType(IRType::SInt, IRF->GetTargetMachine()->GetLongSize());
            break;
        case Type::UnsignedLong:
            ReturnIRType = IRType(IRType::UInt, IRF->GetTargetMachine()->GetLongSize());
            break;

        case Type::LongLong: ReturnIRType = IRType(IRType::SInt, 64); break;
        case Type::UnsignedLongLong: ReturnIRType = IRType(IRType::UInt, 64); break;

        case Type::Float: ReturnIRType = IRType(IRType::FP, 32); break;
        case Type::Double: ReturnIRType = IRType(IRType::FP, 64); break;

        case Type::Void: {
            if (!GetResultType().IsPointerType())
                ReturnIRType = IRType(IRType::None, 0);
            else
                ReturnIRType =
                    GetIRTypeFromASTType(GetResultType(), IRF->GetTargetMachine());

            break;
        }

        case Type::Composite: {
            ReturnIRType = GetIRTypeFromASTType(GetResultType(), IRF->GetTargetMachine());

            // If the return type is a struct, then also make a stack allocation
            // to use that as a temporary, where the result would be copied to after
            // the call
            StructTemp = IRF->CreateSA(FuncName + ".temp", ReturnIRType);

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
            ReturnIRType = IRType(IRType::None);

            break;
        }
        default: assert(!"Unreachable"); break;
    }

    assert(!ReturnIRType.IsInvalid() && "Must be a valid type.");

    // in case if the return type was a struct, so StructTemp not nullptr
    if (StructTemp)
    {
        // make the call
        auto CallRes = IRF->CreateCall(FuncName, Args, ReturnIRType, ImplicitStructIndex);
        // issue a store using the freshly allocated temporary StructTemp if needed
        if (!IsRetChanged)
            IRF->CreateStore(CallRes, StructTemp);
        return StructTemp;
    }

    return IRF->CreateCall(FuncName, Args, ReturnIRType);
}

Value *ReferenceExpression::IRCodegen(IRFactory *IRF)
{
    auto Local = IRF->GetSymbolValue(GetIdentifier());

    if (Local && this->GetResultType().IsStruct())
        return Local;

    if (Local)
    {
        if (GetLValueness())
            return Local;
        else
            return IRF->CreateLoad(Local->GetType(), Local);
    }

    auto GV = IRF->GetGlobalVar(GetIdentifier());
    assert(GV && "Cannot be null.");

    // If Lvalue, then return as a ptr to the global value.
    if (GetLValueness() ||
        (this->GetResultType().IsStruct() && !GetResultType().IsPointerType()))
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
        // if the base value is on the stack or global variable
        if (dynamic_cast<StackAllocationInstruction *>(BaseValue) != nullptr ||
            dynamic_cast<GlobalVariable *>(BaseValue) != nullptr)
        {
            BaseValue = IRF->CreateLoad(ResultType, BaseValue);

            // since we loaded it in, therefore the result indirection level
            // decreased by one
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

    if (!GetLValueness() && ResultType.GetDimensions().empty())
        return IRF->CreateLoad(ResultType, GEP);

    return GEP;
}

Value *ImplicitCastExpression::IRCodegen(IRFactory *IRF)
{
    auto TM          = IRF->GetTargetMachine();
    auto &SourceType = CastableExpression->GetResultType();
    auto &DestType   = GetResultType();

    auto SourceTypeVariant = SourceType.GetTypeVariant();
    auto DestTypeVariant   = DestType.GetTypeVariant();
    auto SourceIRType      = GetIRTypeFromASTType(SourceType, IRF->GetTargetMachine());
    auto DestIRType        = GetIRTypeFromASTType(DestType, IRF->GetTargetMachine());

    assert(SourceType != DestType && "Pointless cast");

    if (SourceType.IsArray() && DestType.IsPointerType())
    {
        assert(SourceTypeVariant == DestTypeVariant);

        auto RefExpr = dynamic_cast<ReferenceExpression *>(CastableExpression.get());

        // StringLiteral case.

        if (RefExpr == nullptr)
        {
            auto StringLitExpr =
                dynamic_cast<StringLiteralExpression *>(CastableExpression.get());
            assert(StringLitExpr && "It must be either a reference or a string literal");

            return StringLitExpr->IRCodegen(IRF);
        }

        auto ReferredSymbol = RefExpr->GetIdentifier();
        auto Val            = IRF->GetSymbolValue(ReferredSymbol);

        if (!Val)
            Val = IRF->GetGlobalVar(ReferredSymbol);
        assert(Val);

        auto GEP = IRF->CreateGEP(DestIRType, Val, IRF->GetConstant((uint64_t)0));

        return GEP;
    }
    auto Val = CastableExpression->IRCodegen(IRF);

    // in case if the expression to be cast is a constant, then no need to do
    // truncation or sign extension, but just masking down the appropriate bits to
    // fit into the desired Type
    if (Val->IsConstant())
    {
        const uint64_t DestBitSize =
            DestType.IsPointerType() ? TM->GetPointerSize() : DestIRType.GetBitSize();

        if (!DestType.IsFloatingPoint() && !SourceType.IsFloatingPoint())
        {
            uint64_t mask = ~0ull;

            // if the bit size is less than 64, then full 1s mask would be wrong, instead
            // we need one which last @DestBitSize bit is 1 and others are 0
            // example: DestBitSize = 16 -> mask = 0x0000_0000_0000_ffff
            if (DestBitSize < 64)
                mask = (1ull << DestBitSize) - 1;

            auto CV = static_cast<Constant *>(Val)->GetIntValue() & mask;

            return IRF->GetConstant(CV, DestBitSize);
        }

        if (DestType.IsFloatingPoint() && !SourceType.IsFloatingPoint())
        {
            double ConvertedVal = static_cast<Constant *>(Val)->GetIntValue();
            return IRF->GetConstant(ConvertedVal, DestIRType.GetBitSize());
        }
        else if (!DestType.IsFloatingPoint() && SourceType.IsFloatingPoint())
        {
            uint64_t ConvertedVal =
                (uint64_t) static_cast<Constant *>(Val)->GetFloatValue();

            return IRF->GetConstant(ConvertedVal, DestBitSize);
        }
        else
        {
            assert((DestIRType.GetBitSize() == 32 && SourceIRType.GetBitSize() == 64) ||
                   (DestIRType.GetBitSize() == 64 && DestIRType.GetBitSize() == 32));

            double ConvertedVal = static_cast<Constant *>(Val)->GetFloatValue();

            if (DestIRType.GetBitSize() == 32)
                ConvertedVal = static_cast<float>(ConvertedVal);

            return IRF->GetConstant((double)ConvertedVal, DestBitSize);
        }
    }

    // cast one pointer type to another
    if (SourceType.IsPointerType() && DestType.IsPointerType())
        return IRF->CreateBitCast(Val, DestIRType);

    // if a pointer type is cast to an integer type
    else if (DestType.IsIntegerType() && SourceType.IsPointerType())
    {
        const auto TargetPtrSize = TM->GetPointerSize();
        const auto IntTypeSize   = DestIRType.GetBitSize();

        if (TargetPtrSize == IntTypeSize)
            return Val;
        else if (TargetPtrSize < IntTypeSize)
            return IRF->CreateSExt(Val, IntTypeSize);
        else if (TargetPtrSize > IntTypeSize)
            return IRF->CreateTrunc(Val, IntTypeSize);
    }

    if (Type::OnlySignednessDifference(SourceTypeVariant, DestTypeVariant))
        return Val;

    const auto SourceTypeBitSize = SourceIRType.GetBitSize();
    const auto DestTypeBitSize   = DestIRType.GetBitSize();

    const bool SourceIsInt  = SourceIRType.IsInt();
    const bool SourceIsSInt = SourceIRType.IsSInt();
    const bool SourceIsFP   = SourceIRType.IsFP();

    const bool DestIsInt  = DestIRType.IsInt();
    const bool DestIsSInt = DestIRType.IsSInt();
    const bool DestIsFP   = DestIRType.IsFP();


    /// Integer cast to Integer
    if (SourceIsInt && DestIsInt)
    {
        // If both have the same size, then nothing to do. This can happen
        // for example in aarch64 case long and long long have the same size.
        if (DestTypeBitSize == SourceTypeBitSize)
            return Val;

        // Sign/Zero extension case
        if (DestTypeBitSize > SourceTypeBitSize)
        {
            if (SourceIsSInt && DestIsSInt)
                return IRF->CreateSExt(Val, DestTypeBitSize);
            else
                return IRF->CreateZExt(Val, DestTypeBitSize);
        }
        else
        {
            return IRF->CreateTrunc(Val, DestTypeBitSize);
        }
    }
    else if (SourceIsFP && DestIsInt)
        return IRF->CreateFloatToInt(Val, DestTypeBitSize);
    else if (SourceIsInt && DestIsFP)
        return IRF->CreateIntToFloat(Val, DestTypeBitSize);
    else
        assert(!"FP cast to FP is Unimplemented ");


    assert(!"Unreachable");
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

    // If the struct member is a pointer to this very same struct type, then
    // set its result type to this struct type. This needed because in this case
    // the type is incomplete, so it does not contain the member types. This way
    // it will.
    if (ResultType.IsStruct() && ResultType.IsPointer() &&
        (ResultType.GetStructName() == ExprType.GetStructName()))
        ResultType = ExprType;

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

    auto ResultIRType = ResultType;
    ResultIRType.SetPointerLevel(GEP->GetTypeRef().GetPointerLevel());

    return IRF->CreateLoad(ResultIRType, GEP);
}

Value *StructInitExpression::IRCodegen(IRFactory *IRF)
{
    /// Allocate stack for struct first
    auto IRResultType = GetIRTypeFromASTType(this->ResultType, IRF->GetTargetMachine());
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
    const auto BW =
        GetIRTypeFromASTType(GetResultType(), IRF->GetTargetMachine()).GetBitSize();
    return IRF->GetConstant(IntValue, BW);
}

Value *FloatLiteralExpression::IRCodegen(IRFactory *IRF)
{
    const auto BW =
        GetIRTypeFromASTType(GetResultType(), IRF->GetTargetMachine()).GetBitSize();
    return IRF->GetConstant(FPValue, BW);
}

Value *StringLiteralExpression::IRCodegen(IRFactory *IRF)
{
    // used to create unique name;
    static unsigned Counter = 0;

    std::string Name = fmt::format(".L.str{}", Counter++);

    auto Ty = GetIRTypeFromASTType(ResultType, IRF->GetTargetMachine());

    // the pointer to global variable data.
    Ty.IncrementPointerLevel();

    // create global variable for the string literal with the label name.
    auto GV = IRF->CreateGlobalVar(Name, Ty, StringValue);
    IRF->AddGlobalVariable(GV);

    return GV;
}

Value *UnaryExpression::IRCodegen(IRFactory *IRF)
{
    Value *E {nullptr};

    if (GetOperationKind() != Address && GetOperationKind() != Minus &&
        GetOperationKind() != Sizeof)
        E = Expr->IRCodegen(IRF);

    switch (GetOperationKind())
    {
        case UnaryOperation::Address: {
            Value *Res = nullptr;

            if (auto RefExpr = dynamic_cast<ReferenceExpression *>(Expr.get());
                RefExpr != nullptr)
            {
                auto ReferEE = RefExpr->GetIdentifier();
                Res          = IRF->GetSymbolValue(ReferEE);

                if (!Res)
                    Res = IRF->GetGlobalVar(ReferEE);
            }
            else
            {
                Expr->SetLValueness(true);
                Res = Expr->IRCodegen(IRF);
            }

            assert(Res);
            return Res;
        }

        case UnaryOperation::DeRef: {
            // if it used as a destination of an assignment, then load does not require
            // example: "*a = 1;"
            if (GetLValueness())
                return E;

            auto ResultType = E->GetType();

            // global vars technically pointer like, which means an "int a;"
            // should be treated as a i32* and not i32 for loads and stores
            if (E->IsGlobalVar())
                ResultType.IncrementPointerLevel();

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

        case BitWiseNot: {
            return IRF->CreateXOr(E, IRF->GetConstant((uint64_t)-1ll));
        }

        case UnaryOperation::PreIncrement:
        case UnaryOperation::PreDecrement: {
            // make the assumption that the expression E is an LValue
            // which means its basically a pointer, so it
            // requires a load first for addition to work

            auto LoadedValType = E->GetTypeRef();

            if (!E->IsGlobalVar())
                LoadedValType.DecrementPointerLevel();

            auto LoadedExpr = IRF->CreateLoad(LoadedValType, E);

            Value *AddOrSub {nullptr};
            if (GetOperationKind() == PreIncrement)
                AddOrSub = IRF->CreateAdd(LoadedExpr, IRF->GetConstant((uint64_t)1));
            else
                AddOrSub = IRF->CreateSub(LoadedExpr, IRF->GetConstant((uint64_t)1));

            IRF->CreateStore(AddOrSub, E);
            return AddOrSub;
        }

        case UnaryOperation::PostDecrement:
        case UnaryOperation::PostIncrement: {
            // make the assumption that the expression E is an LValue
            // which means its basically a pointer, so it
            // requires a load first for addition to work
            auto LoadedValType = E->GetTypeRef();
            LoadedValType.DecrementPointerLevel();

            auto LoadedExpr = IRF->CreateLoad(LoadedValType, E);

            Value *AddOrSub {nullptr};
            if (GetOperationKind() == PostIncrement)
                AddOrSub = IRF->CreateAdd(LoadedExpr, IRF->GetConstant((uint64_t)1));
            else
                AddOrSub = IRF->CreateSub(LoadedExpr, IRF->GetConstant((uint64_t)1));

            IRF->CreateStore(AddOrSub, E);

            return LoadedExpr;
        }

        case UnaryOperation::Sizeof: {
            uint64_t size = 0;
            Type TypeToBeExamined;

            // if there was an expression used with sizeof,
            // use thats result type instead
            if (Expr)
                TypeToBeExamined = Expr->GetResultType();
            else    // else use given type.
                TypeToBeExamined = SizeOfType.value();

            size = GetIRTypeFromASTType(TypeToBeExamined, IRF->GetTargetMachine())
                       .GetByteSize(IRF->GetTargetMachine());

            assert(size != 0 && "sizeof should not result in 0");

            return IRF->GetConstant(size);
        }


        default: assert(!"Unimplemented");
    }

    return nullptr;
}

Value *BinaryExpression::IRCodegen(IRFactory *IRF)
{
    // TODO: simplify this, specially in case if there are actually multiple
    // logical operations like "a > 0 && a < 10 && a != 5"
    if (GetOperationKind() == LogicalAnd || GetOperationKind() == LogicalOr)
    {
        // goal IR:
        //    # L generated here
        //    sa $result
        //    cmp.eq $c1, $L, 0
        //    br $c1, ANDL ? <false> : <TestRhsBB>
        //    j <true>  # only for ORL
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
        const auto IsLogicalAnd = (GetOperationKind() == LogicalAnd);
        const auto FuncPtr      = IRF->GetCurrentFunction();
        auto TestRhsBB          = std::make_unique<BasicBlock>("test_RHS", FuncPtr);
        auto TrueBB             = std::make_unique<BasicBlock>("true", FuncPtr);
        auto FalseBB            = std::make_unique<BasicBlock>("false", FuncPtr);
        auto FinalBB            = std::make_unique<BasicBlock>("final", FuncPtr);

        auto Result = IRF->CreateSA("result", IRType::CreateBool());
        auto Store  = IRF->CreateStore(IRF->GetConstant((uint64_t)0), Result);

        // LHS
        auto L = Lhs->IRCodegen(IRF);


        // If the left hand side is a constant
        if (L->IsConstant())
        {
            assert(!L->IsFPType() && "Only integer support converted to boolean value");

            IRF->EraseInst(Result);
            IRF->EraseInst(Store);

            // If the condition is a constant false value, then
            if (static_cast<Constant *>(L)->GetIntValue() == 0)
            {
                // false && expr -> false
                if (IsLogicalAnd)
                    return L;
                // false || expr -> expr
                else
                    return Rhs->IRCodegen(IRF);
            }
            /// L is true
            else
            {
                // true && expr -> expr
                if (IsLogicalAnd)
                    return Rhs->IRCodegen(IRF);
                // ture || expr -> is always true
                else
                    return L;
            }
        }

        // if L was a compare instruction then just revert its relation
        if (auto LCMP = dynamic_cast<CompareInstruction *>(L); LCMP != nullptr)
        {
            LCMP->InvertRelation();
            IRF->CreateBranch(L, IsLogicalAnd ? FalseBB.get() : TestRhsBB.get());
        }
        else
        {
            auto LHSTest =
                IRF->CreateCmp(CompareInstruction::EQ, L, IRF->GetConstant((uint64_t)0));
            IRF->CreateBranch(LHSTest, IsLogicalAnd ? FalseBB.get() : TestRhsBB.get());
        }

        if (!IsLogicalAnd)
            IRF->CreateJump(TrueBB.get());

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

        if (R->GetTypeRef().IsStruct() && L->GetTypeRef().GetPointerLevel() == 1 &&
            R->GetTypeRef().GetPointerLevel() == 1 &&
            (instanceof <StackAllocationInstruction>(L) ||
                            !GetResultType().IsPointerType()))
            IRF->CreateMemCopy(L, R, R->GetTypeRef().GetBaseTypeByteSize());
        else
            IRF->CreateStore(R, L);

        return R;
    }

    if (IsCompositeAssignmentOperator())
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
            Value *OperationResult {nullptr};

            // converting the LValue L to an RValue by loading it in
            auto ResultIRType = L->GetType();
            ResultIRType.DecrementPointerLevel();

            L = IRF->CreateLoad(ResultIRType, L);

            switch (GetOperationKind())
            {
                case AddAssign: OperationResult = IRF->CreateAdd(L, R); break;
                case SubAssign: OperationResult = IRF->CreateSub(L, R); break;
                case MulAssign: OperationResult = IRF->CreateMul(L, R); break;
                case DivAssign: OperationResult = IRF->CreateDiv(L, R); break;
                case ModAssign: OperationResult = IRF->CreateMod(L, R); break;
                case AndAssign: OperationResult = IRF->CreateAnd(L, R); break;
                case OrAssign: OperationResult = IRF->CreateOr(L, R); break;
                case XorAssign: OperationResult = IRF->CreateXOr(L, R); break;
                case LSLAssign: OperationResult = IRF->CreateLSL(L, R); break;
                case LSRAssign: OperationResult = IRF->CreateLSR(L, R); break;

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
            case AddF:
            case Mul:
            case MulF:
            case And:
            case Or:
            case Xor:
            case Equal:
            case NotEqual: std::swap(L, R); break;
            default: break;
        }
    }

    switch (GetOperationKind())
    {
        case LSL: return IRF->CreateLSL(L, R);
        case LSR: return IRF->CreateLSR(L, R);
        case Add: {
            // pointer add: pointer + constantexprssion -> emit a gep
            /* """
             *   int arr[] = {1, 2, 3, 4};
             *   int *ptr = arr;
             *   ptr + 1 -> ptr + 1 * size(int) -> *(ptr + 1) == 2
             *
             * """
             */

            if (L->GetTypeRef().IsPointer() && R->IsConstant())
                return IRF->CreateGEP(L->GetTypeRef(), L, R);

            return IRF->CreateAdd(L, R);
        }

        case Sub: return IRF->CreateSub(L, R);
        case Mul: return IRF->CreateMul(L, R);
        case Div: return IRF->CreateDiv(L, R);
        case Mod: return IRF->CreateMod(L, R);
        case And: return IRF->CreateAnd(L, R);
        case Or: return IRF->CreateOr(L, R);
        case Xor: return IRF->CreateXOr(L, R);
        case DivU: return IRF->CreateDivU(L, R);
        case ModU: return IRF->CreateModU(L, R);
        case AddF: return IRF->CreateAddF(L, R);
        case SubF: return IRF->CreateSubF(L, R);
        case MulF: return IRF->CreateMulF(L, R);
        case DivF: return IRF->CreateDivF(L, R);

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
    auto C = Condition->IRCodegen(IRF);

    /// If the condition is a compile time computable constant, then generate
    /// the if or else body.
    if (C->IsConstant())
    {
        assert(!C->IsFPType() && "Only support integer converted to boolean");

        // If the condition is a constant false value, then
        if (static_cast<Constant *>(C)->GetIntValue() == 0)
            return ExprIfFalse->IRCodegen(IRF);
        else
            return ExprIfTrue->IRCodegen(IRF);
    }

    const auto FuncPtr = IRF->GetCurrentFunction();

    auto TrueBB  = std::make_unique<BasicBlock>("tenary_true", FuncPtr);
    auto FalseBB = std::make_unique<BasicBlock>("tenary_false", FuncPtr);
    auto FinalBB = std::make_unique<BasicBlock>("tenary_end", FuncPtr);


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
        {
            assert(dynamic_cast<GlobalVariable *>(Decl));
            IRF->AddGlobalVariable(Decl);
        }
    }

    return nullptr;
}

Type FunctionDeclaration::CreateType(const Type &t,
                                     const FunctionDeclaration::ParamVec &params)
{
    Type funcType(t);

    for (const auto &Argument : params)
    {
        auto type = Argument->GetType();
        funcType.GetArgTypes().push_back(type);
    }

    // if there are no arguments then set it to void
    if (params.empty())
        funcType.GetArgTypes().emplace_back(Type::Void);

    return funcType;
}

BinaryExpression::BinaryOperation BinaryExpression::GetOperationKind() const
{
    switch (Operation.GetKind())
    {
        case Token::Assign: return Assign;
        case Token::PlusEqual: return AddAssign;
        case Token::MinusEuqal: return SubAssign;
        case Token::MulEqual: return MulAssign;
        case Token::DivEqual: return DivAssign;
        case Token::ModEqual: return ModAssign;
        case Token::AndEqual: return AndAssign;
        case Token::OrEqual: return OrAssign;
        case Token::XorEqual: return XorAssign;
        case Token::LeftShiftEqual: return LSLAssign;
        case Token::RightShiftEqual: return LSRAssign;
        case Token::LeftShift: return LSL;
        case Token::RightShift: return LSR;
        case Token::Plus: {
            if (GetResultType().IsFloatingPoint())
                return AddF;
            return Add;
        }

        case Token::Minus: {
            if (GetResultType().IsFloatingPoint())
                return SubF;
            return Sub;
        }

        case Token::Mul: {
            if (GetResultType().IsFloatingPoint())
                return MulF;
            return Mul;
        }

        case Token::Div: {
            if (GetResultType().IsUnsigned())
                return DivU;
            else if (GetResultType().IsFloatingPoint())
                return DivF;
            else
                return Div;
        }

        case Token::Mod: {
            if (GetResultType().IsUnsigned())
                return ModU;
            return Mod;
        }

        case Token::And: return And;
        case Token::Or: return Or;
        case Token::Xor: return Xor;
        case Token::Not: return Not;
        case Token::Equal: return Equal;
        case Token::Less: return Less;
        case Token::Greater: return Greater;
        case Token::LessEqual: return LessEqual;
        case Token::GreaterEqual: return GreaterEqual;
        case Token::NotEqual: return NotEqual;
        case Token::LogicalAnd: return LogicalAnd;
        case Token::LogicalOr: return LogicalOr;

        default: assert(false && "Invalid binary Operator kind."); break;
    }
}

bool BinaryExpression::IsCompositeAssignmentOperator() const
{
    switch (GetOperationKind())
    {
        case AddAssign:
        case SubAssign:
        case MulAssign:
        case DivAssign:
        case ModAssign:
        case AndAssign:
        case OrAssign:
        case XorAssign:
        case LSLAssign:
        case LSRAssign: return true;

        default: return false;
    }
}

bool BinaryExpression::IsModulo() const
{
    return GetOperationKind() == Mod || GetOperationKind() == ModU;
}

bool BinaryExpression::IsShift() const
{
    return GetOperationKind() == LSL || GetOperationKind() == LSR;
}

UnaryExpression::UnaryOperation UnaryExpression::GetOperationKind() const
{
    switch (Operation.GetKind())
    {
        case Token::And: return UnaryOperation::Address;
        case Token::Mul: return UnaryOperation::DeRef;
        case Token::Minus: return UnaryOperation::Minus;
        case Token::Not: return UnaryOperation::Not;
        case Token::Inc:
            return IsPostFix ? UnaryOperation::PostIncrement :
                               UnaryOperation::PreIncrement;
        case Token::Dec:
            return IsPostFix ? UnaryOperation::PostDecrement :
                               UnaryOperation::PreDecrement;

        case Token::Sizeof: return UnaryOperation::Sizeof;
        case Token::Tilde: return UnaryOperation::BitWiseNot;

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

void DoWhileStatement::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitDoWhileStatement(this);
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

void StringLiteralExpression::Accept(ASTVisitor *Visitor) const
{
    Visitor->VisitStringLiteralExpression(this);
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
