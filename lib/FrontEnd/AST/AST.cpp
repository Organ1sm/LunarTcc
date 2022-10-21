#include "FrontEnd/AST/AST.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/IRType.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "Utils/ErrorLogger.hpp"
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
        ResultType = Type(Type::GetStrongestType(Lhs->GetResultType().GetTypeVariant(),
                                                 Rhs->GetResultType().GetTypeVariant()));
}

UnaryExpression::UnaryExpression(Token Op, UnaryExpression::ExprPtr E)
    : Operation(Op), Expr(std::move(E))
{
    if (GetOperationKind() == UnaryOperation::DeRef)
    {
        ResultType = Expr->GetResultType();
        ResultType.DecrementPointerLevel();
    }
    else
        assert(!"Unimplemented!");
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
        case Type::Char:
            return IRType(IRType::SInt, 8);
        case Type::Int:
            return IRType(IRType::SInt);
        case Type::Double:
            return IRType(IRType::FP, 64);
        case Type::Composite:
            return IRType(IRType::Struct);
        default:
            assert(!"Invalid type.");
            break;
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
        case Type::Char:
            ReturnType = IRType(IRType::SInt, 8);
            break;
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
            assert(!"Invalid function return type.");
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
    if (IRF->IsGlobalScope())
        return IRF->CreateGlobalVar(Name, VarType);

    /// Othewise we are in a localscope of a function.
    /// Allocate space on stack and update the local symbol Table.
    auto SA = IRF->CreateSA(Name, VarType);
    IRF->AddToSymbolTable(Name, SA);

    return SA;
}

Value *MemberDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *StructDeclaration::IRCodegen(IRFactory *IRF) { return nullptr; }

Value *CallExpression::IRCodegen(IRFactory *IRF)
{
    std::vector<Value *> Args;

    for (auto &Arg : Arguments)
        Args.push_back(Arg->IRCodegen(IRF));

    auto ReturnType = GetResultType().GetReturnType();

    IRType ReturnIRType;
    StackAllocationInstruction *StructTemp {nullptr};

    switch (ReturnType)
    {
        case Type::Int:
            ReturnIRType = IRType(IRType::SInt);
            break;
        case Type::Double:
            ReturnIRType = IRType(IRType::FP, 64);
            break;
        case Type::Void:
            ReturnIRType = IRType(IRType::None, 0);
            break;
        case Type::Composite:
            {
                ReturnIRType = GetIRTypeFromASTType(GetResultType());

                // If the return type is a struct, then also make a stack allocation
                // to use that as a temporary, where the result would be copied to after
                // the call
                StructTemp = IRF->CreateSA(Name + ".temp", ReturnIRType);
                break;
            }
        default:
            break;
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

    if (this->GetResultType().IsStruct())
        return Local;

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

    auto ArrayBaseType = BaseValue->GetType().GetBaseType();
    ArrayBaseType.IncrementPointerLevel();

    auto GEP = IRF->CreateGEP(ArrayBaseType, BaseValue, IndexValue);

    if (!GetLValueness())
        return IRF->CreateLoad(ArrayBaseType, GEP);

    return GEP;
}

Value *ImplicitCastExpression::IRCodegen(IRFactory *IRF)
{
    auto SourceTypeVariant = CastableExpression->GetResultType().GetTypeVariant();
    auto DestTypeVariant   = GetResultType().GetTypeVariant();
    auto Val               = CastableExpression->IRCodegen(IRF);

    if (SourceTypeVariant == Type::Int && DestTypeVariant == Type::Double)
        return IRF->CreateIntToFloat(Val, 32);
    else if (SourceTypeVariant == Type::Double && DestTypeVariant == Type::Int)
        return IRF->CreateFloatToInt(Val, 64);
    else if (SourceTypeVariant == Type::Char && DestTypeVariant == Type::Int)
        return IRF->CreateSExt(Val, 32);
    else if (SourceTypeVariant == Type::Int && DestTypeVariant == Type::Char)
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

    assert(ExprType.IsStruct());

    auto IndexValue = IRF->GetConstant((uint64_t)MemberIndex);
    auto GEP        = IRF->CreateGEP(ExprType, BaseValue, IndexValue);

    if (GetLValueness())
        return GEP;

    auto ResultType = GetIRTypeFromASTType(this->GetResultType());

    return IRF->CreateLoad(ResultType, GEP);
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
    auto E = Expr->IRCodegen(IRF);

    if (GetOperationKind() == UnaryOperation::DeRef)
    {
        auto ResultType = E->GetType();
        return IRF->CreateLoad(ResultType, E);
    }
    else
    {
        assert(!"Unimplemented");
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
            case NotEqual:
                std::swap(L, R);
                break;
            default:
                break;
        }
    }

    switch (GetOperationKind())
    {
        case Add:
            return IRF->CreateAdd(L, R);
        case Sub:
            return IRF->CreateSub(L, R);
        case Mul:
            return IRF->CreateMul(L, R);
        case Div:
            return IRF->CreateDiv(L, R);
        case Mod:
            return IRF->CreateMod(L, R);
        case And:
            return IRF->CreateAnd(L, R);
        case Equal:
            return IRF->CreateCmp(CompareInstruction::EQ, L, R);
        case Less:
            return IRF->CreateCmp(CompareInstruction::LT, L, R);
        case Greater:
            return IRF->CreateCmp(CompareInstruction::GT, L, R);
        case NotEqual:
            return IRF->CreateCmp(CompareInstruction::NE, L, R);
        default:
            assert(!"Unhandled binary instruction type");
            break;
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
    ElseBody->ASTDump(tab + 2);
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

void StructMemberReference::ASTDump(unsigned tab)
{
    auto Str = "'" + ResultType.ToString() + "' ";
    Str += "'." + MemberIdentifier + "'";

    Print("StructMemberReference ", tab);
    PrintLn(Str.c_str());

    StructTypedExpression->ASTDump(tab + 2);
}

UnaryExpression::UnaryOperation UnaryExpression::GetOperationKind()
{
    switch (Operation.GetKind())
    {
        case Token::Mul:
            return UnaryOperation::DeRef;
        default:
            assert(!"Invalid unary operator kind.");
            break;
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
    auto Str = "'" + ResultType.ToString() + "' ";

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
