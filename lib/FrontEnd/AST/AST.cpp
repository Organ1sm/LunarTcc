#include "FrontEnd/AST/AST.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/IRType.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "Utils/ErrorLogger.hpp"
#include "fmt/core.h"
#include <cassert>
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

BinaryExpression::BinaryExpression(BinaryExpression::ExprPtr L,
                                   Token Op,
                                   BinaryExpression::ExprPtr R)
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

UnaryExpression::UnaryExpression(Token Op, UnaryExpression::ExprPtr E)
    : Operation(Op), Expr(std::move(E))
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
        case Type::Int: return IRType(IRType::SInt);
        case Type::UnsignedInt: return IRType(IRType::UInt);
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

    Result.SetPointerLevel(CT.GetPointerLevel());
    return Result;
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

                // If the statement is a "break" then insert jump to the default case
                // here, since cannot generate that jump simply, it would require context
                if (auto Break = dynamic_cast<BreakStatement *>(Statement.get());
                    Break != nullptr)
                    IRF->CreateJump(SwitchEnd.get());
            }

            CaseBodies.erase(CaseBodies.begin());
        }
    }

    // Generate default case
    IRF->InsertBB(std::move(DefaultCase));
    for (auto &Statement : DefaultBody)
        Statement->IRCodegen(IRF);

    IRF->InsertBB(std::move(SwitchEnd));

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

Value *ForStatement::IRCodegen(IRFactory *IRF)
{
    // Similar solution to WhileStatement. The difference is that here the
    // initialization part has to be generated before the loop_header basicblock
    // and also inserting the increment expression before the backward jump to the
    // loop_header
    const auto FuncPtr = IRF->GetCurrentFunction();
    auto Header        = std::make_unique<BasicBlock>("loop_header", FuncPtr);
    auto LoopBody      = std::make_unique<BasicBlock>("loop_body", FuncPtr);
    auto LoopEnd       = std::make_unique<BasicBlock>("loop_end", FuncPtr);
    auto HeaderPtr     = Header.get();

    // Generating code for the initializing expression and adding and explicit
    // unconditional jump to the loop header basic block
    Init->IRCodegen(IRF);
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
    Increment->IRCodegen(IRF);
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
        case Type::Composite:
            if (FuncType.IsStruct())
                ReturnType = GetIRTypeFromASTType(FuncType);
            else
                assert(!"Unhandled Return Type.");
            break;

        case Type::Char: ReturnType = IRType(IRType::SInt, 8); break;
        case Type::UnsignedChar: ReturnType = IRType(IRType::UInt, 8); break;
        case Type::Int: ReturnType = IRType(IRType::SInt); break;
        case Type::UnsignedInt: ReturnType = IRType(IRType::UInt); break;
        case Type::Double: ReturnType = IRType(IRType::FP, 64); break;
        case Type::Void: ReturnType = IRType(IRType::None); break;

        default: assert(!"Invalid function return type."); break;
    }

    IRF->CreateNewFunction(Name, ReturnType);


    for (auto &Argument : Arguments)
        Argument->IRCodegen(IRF);

    Body->IRCodegen(IRF);
    return nullptr;
}

Value *FunctionParameterDeclaration::IRCodegen(IRFactory *IRF)
{
    auto ParamType = GetIRTypeFromASTType(Ty);
    auto Param     = std::make_unique<FunctionParameter>(Name, ParamType);
    auto SA        = IRF->CreateSA(Name, ParamType);

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
        // FIXME: assuming 1 dimensional init list like "{ 1, 2 }", add support
        // for more complex case like "{ { 1, 2 }, { 3, 4 } }"

        if (auto InitListExpr = dynamic_cast<InitializerListExpression *>(Init.get());
            InitListExpr != nullptr)
        {
            for (auto &Expr : InitListExpr->GetExprList())
            {
                if (auto ConstantExpr =
                        dynamic_cast<IntegerLiteralExpression *>(Expr.get());
                    ConstantExpr != nullptr)
                    InitList.push_back(ConstantExpr->GetUIntValue());
                else
                    assert(!"Other types unhandled yet.");
            }
        }
        else
        {
            if (auto ConstantExpr = dynamic_cast<IntegerLiteralExpression *>(Init.get());
                ConstantExpr != nullptr)
            {
                InitList.push_back(ConstantExpr->GetUIntValue());
            }
        }

        return IRF->CreateGlobalVar(Name, VarType, std::move(InitList));
    }

    /// Othewise we are in a local scope of a function.
    /// Allocate space on stack and update the local symbol Table.
    auto SA = IRF->CreateSA(Name, VarType);

    if (Init)
        IRF->CreateStore(Init->IRCodegen(IRF), SA);

    IRF->AddToSymbolTable(Name, SA);

    return SA;
}

Value *MemberDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *StructDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *EnumDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *CallExpression::IRCodegen(IRFactory *IRF)
{
    std::vector<Value *> Args;

    for (auto &Arg : Arguments)

    {
        auto ArgIR = Arg->IRCodegen(IRF);
        auto IRTy  = ArgIR->GetTypeRef();
        auto ArgTy = Arg->GetResultType();

        if (IRTy.IsStruct() && IRTy.IsPointer() && ArgTy.IsStruct()
            && !ArgTy.IsPointerType())
        {
            ArgIR = IRF->CreateLoad(ArgIR->GetType(), ArgIR);
        }
        Args.push_back(ArgIR);
    }

    auto ReturnType = GetResultType().GetReturnType();

    IRType ReturnIRType;
    StackAllocationInstruction *StructTemp {nullptr};

    switch (ReturnType)
    {
        case Type::Int: ReturnIRType = IRType(IRType::SInt); break;
        case Type::Double: ReturnIRType = IRType(IRType::FP, 64); break;
        case Type::Void: ReturnIRType = IRType(IRType::None, 0); break;
        case Type::Composite: {
            ReturnIRType = GetIRTypeFromASTType(GetResultType());

            // If the return type is a struct, then also make a stack allocation
            // to use that as a temporary, where the result would be copied to after
            // the call
            StructTemp = IRF->CreateSA(Name + ".temp", ReturnIRType);
            break;
        }
        default: break;
    }

    // in case if the ret type was a struct, so StructTemp not nullptr
    if (StructTemp)
    {
        // make the call
        auto CallRes = IRF->CreateCall(Name, Args, ReturnIRType);
        // issue a store using the freshly allocated temporary StructTemp
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
    ResultType.ReduceDimension();

    if (ResultType.GetPointerLevel() == 0)
        ResultType.IncrementPointerLevel();

    auto GEP = IRF->CreateGEP(ResultType, BaseValue, IndexValue);

    if (!GetLValueness() && ResultType.GetDimensions().size() == 0)
        return IRF->CreateLoad(ResultType, GEP);

    return GEP;
}

Value *ImplicitCastExpression::IRCodegen(IRFactory *IRF)
{
    auto SourceTypeVariant = CastableExpression->GetResultType().GetTypeVariant();
    auto DestTypeVariant   = GetResultType().GetTypeVariant();
    auto Val               = CastableExpression->IRCodegen(IRF);

    if (Type::OnlySignednessDifference(SourceTypeVariant, DestTypeVariant))
        return Val;

    if (SourceTypeVariant == Type::Int && DestTypeVariant == Type::Double)
        return IRF->CreateIntToFloat(Val, 32);

    else if (SourceTypeVariant == Type::Double && DestTypeVariant == Type::Int)
        return IRF->CreateFloatToInt(Val, 64);

    else if (SourceTypeVariant == Type::Char && DestTypeVariant == Type::Int)
        return IRF->CreateSExt(Val, 32);

    else if (SourceTypeVariant == Type::UnsignedChar
             && (DestTypeVariant == Type::Int || DestTypeVariant == Type::UnsignedInt))
        return IRF->CreateZExt(Val, 32);

    else if (SourceTypeVariant == Type::Int
             && (DestTypeVariant == Type::Char || DestTypeVariant == Type::UnsignedChar))
        return IRF->CreateTrunc(Val, 8);

    else
        assert(!"Invalid conversion.");

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

Value *StructInitExpression::IRCodegen(IRFactory *IRF) { return nullptr; }

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

    if (GetOperationKind() != UnaryOperation::Address)
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
        auto TrueBB        = std::make_unique<BasicBlock>("ture", FuncPtr);
        auto FalseBB       = std::make_unique<BasicBlock>("false", FuncPtr);
        auto FinalBB       = std::make_unique<BasicBlock>("final", FuncPtr);


        auto L = Lhs->IRCodegen(IRF);

        // LHS test
        auto Result = IRF->CreateSA("result", IRType::CreateBool());

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
        IRF->CreateStore(IRF->GetConstant((uint64_t)1), Result);
        IRF->CreateJump(FinalBB.get());


        return Result;
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

    if (GetOperationKind() == AddAssign || GetOperationKind() == SubAssign
        || GetOperationKind() == MulAssign || GetOperationKind() == DivAssign)
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
            case Equal:
            case NotEqual: std::swap(L, R); break;
            default: break;
        }
    }

    switch (GetOperationKind())
    {
        case Add: return IRF->CreateAdd(L, R);
        case Sub: return IRF->CreateSub(L, R);
        case Mul: return IRF->CreateMul(L, R);
        case Div: return IRF->CreateDiv(L, R);
        case Mod: return IRF->CreateMod(L, R);
        case And: return IRF->CreateAnd(L, R);
        case Equal: return IRF->CreateCmp(CompareInstruction::EQ, L, R);
        case Less: return IRF->CreateCmp(CompareInstruction::LT, L, R);
        case Greater: return IRF->CreateCmp(CompareInstruction::GT, L, R);
        case NotEqual: return IRF->CreateCmp(CompareInstruction::NE, L, R);
        default: assert(!"Unhandled binary instruction type"); break;
    }
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

//=--------------------------------------------------------------------------=//
//=------------------------- AST ASTDump functions --------------------------=//
//=--------------------------------------------------------------------------=//

void VariableDeclaration::ASTDump(unsigned tab)
{
    auto TypeStr = "'" + AType.ToString() + "' ";
    auto NameStr = "'" + Name + "'";

    Print("VariableDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());

    if (Init)
        Init->ASTDump(tab + 2);
}

void MemberDeclaration::ASTDump(unsigned tab)
{
    auto TypeStr = "'" + AType.ToString() + "' ";
    auto NameStr = "'" + Name + "'";

    Print("MemberDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());
}

void StructDeclaration::ASTDump(unsigned tab)
{
    Print("StructDeclaration '", tab);
    Print(Name.c_str());
    PrintLn("' ");

    for (auto &M : Members)
        M->ASTDump(tab + 2);
}

void EnumDeclaration::ASTDump(unsigned tab)
{
    auto HeaderStr      = fmt::format("EnumDeclaration `{}`", BaseType.ToString());
    std::string BodyStr = "Enumerators ";

    std::size_t LoopCounter = 0;
    for (auto &[Enum, Val] : Enumerators)
    {
        BodyStr += fmt::format("`{}` = {}", Enum, Val);

        if (++LoopCounter < Enumerators.size())
            BodyStr += ", ";
    }

    PrintLn(HeaderStr.c_str(), tab);
    PrintLn(BodyStr.c_str(), tab + 2);
}

void CompoundStatement::ASTDump(unsigned int tab)
{
    PrintLn("CompoundStatement ", tab);

    for (auto &d : Declarations)
        d->ASTDump(tab + 2);

    for (auto &s : Statements)
        s->ASTDump(tab + 2);
}

void ExpressionStatement::ASTDump(unsigned int tab)
{
    PrintLn("ExpressionStatement ", tab);

    Expr->ASTDump(tab + 2);
}

void IfStatement::ASTDump(unsigned int tab)
{
    PrintLn("IfStatement ", tab);

    Condition->ASTDump(tab + 2);
    IfBody->ASTDump(tab + 2);

    if (ElseBody)
        ElseBody->ASTDump(tab + 2);
}

void SwitchStatement::ASTDump(unsigned int tab)
{
    PrintLn("SwitchStatement", tab);

    Condition->ASTDump(tab + 2);

    for (auto &[CaseConst, CaseBody] : Cases)
    {
        auto Str = fmt::format("Case `{}`", CaseConst);
        PrintLn(Str.c_str(), tab + 2);

        for (auto &CaseStmt : CaseBody)
            CaseStmt->ASTDump(tab + 4);
    }

    if (!DefaultBody.empty())
        PrintLn("DefaultCase", tab + 2);

    for (auto &DefaultStmt : DefaultBody)
        DefaultStmt->ASTDump(tab + 4);
}

void WhileStatement::ASTDump(unsigned int tab)
{
    PrintLn("WhileStatement ", tab);

    Condition->ASTDump(tab + 2);
    Body->ASTDump(tab + 2);
}

void ForStatement::ASTDump(unsigned int tab)
{
    PrintLn("ForStatement", tab);

    Init->ASTDump(tab + 2);
    Condition->ASTDump(tab + 2);
    Increment->ASTDump(tab + 2);
    Body->ASTDump(tab + 2);
}

void ReturnStatement::ASTDump(unsigned int tab)
{
    PrintLn("ReturnStatement ", tab);

    if (ReturnValue)
        ReturnValue.value()->ASTDump(tab + 2);
}

void FunctionParameterDeclaration::ASTDump(unsigned int tab)
{
    auto TypeStr = "'" + Ty.ToString() + "' ";
    auto NameStr = "'" + Name + "'";

    Print("FunctionParameterDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());
}

void FunctionDeclaration::ASTDump(unsigned int tab)
{
    auto TypeStr = "'" + FuncType.ToString() + "' ";
    auto NameStr = "'" + Name + "'";

    Print("FunctionDeclaration ", tab);
    Print(TypeStr.c_str());
    PrintLn(NameStr.c_str());

    for (auto &Argument : Arguments)
        Argument->ASTDump(tab + 2);

    Body->ASTDump(tab + 2);
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
        case Token::Plus: return Add;
        case Token::Minus: return Sub;
        case Token::Mul: return Mul;
        case Token::Div: return Div;
        case Token::Mod: return Mod;
        case Token::And: return And;
        case Token::Not: return Not;
        case Token::Equal: return Equal;
        case Token::Less: return Less;
        case Token::Greater: return Greater;
        case Token::NotEqual: return NotEqual;
        case Token::LogicalAnd: return LogicalAnd;
        default: assert(false && "Invalid binary Operator kind."); break;
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

void StructMemberReference::ASTDump(unsigned tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    // Todo: if it's struct pointer, should output `-> member`
    Str += "'." + MemberIdentifier + "'";

    Print("StructMemberReference ", tab);
    PrintLn(Str.c_str());

    StructTypedExpression->ASTDump(tab + 2);
}

void StructInitExpression::ASTDump(unsigned tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";

    Print("StructInitExpression ", tab);
    PrintLn(Str.c_str());

    for (auto &InitValue : InitValues)
        InitValue->ASTDump(tab + 2);
}


UnaryExpression::UnaryOperation UnaryExpression::GetOperationKind()
{
    switch (Operation.GetKind())
    {
        case Token::And: return UnaryOperation::Address;
        case Token::Mul: return UnaryOperation::DeRef;
        case Token::Inc: return UnaryOperation::PostIncrement;
        case Token::Dec: return UnaryOperation::PostDecrement;

        default: assert(!"Invalid unary operator kind."); break;
    }
}

void UnaryExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'" + Operation.GetString() + "'";

    Print("UnaryExpression ", tab);
    PrintLn(Str.c_str());

    Expr->ASTDump(tab + 2);
}

void CallExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'" + Name + "'";

    Print("CallExpression ", tab);
    PrintLn(Str.c_str());

    for (auto &Argument : Arguments)
        Argument->ASTDump(tab + 2);
}

void ReferenceExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'" + Identifier + "' ";

    Print("ReferenceExpression ", tab);
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
    Print("FloatLiteralExpression ", tab);

    auto TyStr = "'" + ResultType.ToString() + "' ";
    Print(TyStr.c_str());

    auto ValueStr = "'" + std::to_string(FPValue) + "'";
    PrintLn(ValueStr.c_str());
}

void ArrayExpression::ASTDump(unsigned int tab)
{
    std::string TypeStr = ResultType.ToString();

    for (auto Dim : BaseExpression->GetResultType().GetDimensions())
    {
        TypeStr += fmt::format("[{}]", Dim);
    }

    auto Str = fmt::format("`{}`", TypeStr);

    Print("ArrayExpression ", tab);
    PrintLn(Str.c_str());

    IndexExpression->ASTDump(tab + 2);
}

void TranslationUnit::ASTDump(unsigned int tab)
{
    PrintLn("TranslationUnit", tab);
    for (auto &Declaration : Declarations)
        Declaration->ASTDump(tab + 2);

    PrintLn("");
}

void ImplicitCastExpression::ASTDump(unsigned int tab)
{
    auto Str = "'" + ResultType.ToString() + "'";

    Print("ImplicitCastExpression ", tab);
    PrintLn(Str.c_str());

    CastableExpression->ASTDump(tab + 2);
}

void InitializerListExpression::ASTDump(unsigned int tab)
{
    PrintLn("InitializerListExpression", tab);

    for (auto &E : Expressions)
        E->ASTDump(tab + 2);
}
