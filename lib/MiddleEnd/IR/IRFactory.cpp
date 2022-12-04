//
// Created by Organ1sm.
//
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "MiddleEnd/IR/Module.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include <cstdint>
#include <memory>

Instruction *
    IRFactory::CreateBinaryInstruction(Instruction::InstructionKind K, Value *L, Value *R)
{
    auto Inst = std::make_unique<BinaryInstruction>(K, L, R, GetCurrentBB());
    Inst->SetId(ID++);
    auto InstPtr = Inst.get();
    Insert(std::move(Inst));

    return InstPtr;
}

BasicBlock *IRFactory::GetCurrentBB() { return CurrentModule.CurrentBB(); }

Instruction *IRFactory::Insert(std::unique_ptr<Instruction> I)
{
    return this->GetCurrentBB()->Insert(std::move(I));
}

Instruction *IRFactory::CreateAdd(Value *LHS, Value *RHS)
{
    if (LHS->IsConstant() && RHS->IsConstant())
    {
        auto ConstL = dynamic_cast<Constant *>(LHS);
        auto ConstR = dynamic_cast<Constant *>(RHS);

        assert(ConstL && ConstR);

        const auto Val = ConstL->GetIntValue() + ConstR->GetIntValue();
        return CreateMov(GetConstant((uint64_t)Val));
    }

    return CreateBinaryInstruction(Instruction::Add, LHS, RHS);
}

Instruction *IRFactory::CreateAddF(Value *LHS, Value *RHS)
{
    if (LHS->IsConstant() && RHS->IsConstant())
    {
        auto ConstL = dynamic_cast<Constant *>(LHS);
        auto ConstR = dynamic_cast<Constant *>(RHS);

        assert(ConstL && ConstR);

        const auto Val = ConstL->GetFloatValue() + ConstR->GetFloatValue();
        return CreateMovF(GetConstant(Val));
    }

    return CreateBinaryInstruction(Instruction::AddF, LHS, RHS);
}

Instruction *IRFactory::CreateSub(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Sub, LHS, RHS);
}

Instruction *IRFactory::CreateSubF(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::SubF, LHS, RHS);
}

Instruction *IRFactory::CreateMul(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Mul, LHS, RHS);
}

Instruction *IRFactory::CreateMulF(Value *LHS, Value *RHS)
{
    if (LHS->IsConstant() && RHS->IsConstant())
    {
        const auto Val = static_cast<Constant *>(LHS)->GetFloatValue() *
                         static_cast<Constant *>(RHS)->GetFloatValue();

        return CreateMovF(GetConstant(Val));
    }

    return CreateBinaryInstruction(Instruction::MulF, LHS, RHS);
}

Instruction *IRFactory::CreateDiv(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Div, LHS, RHS);
}

Instruction *IRFactory::CreateDivF(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::DivF, LHS, RHS);
}

Instruction *IRFactory::CreateMod(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Mod, LHS, RHS);
}

Instruction *IRFactory::CreateDivU(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::DivU, LHS, RHS);
}

Instruction *IRFactory::CreateModU(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::ModU, LHS, RHS);
}

Instruction *IRFactory::CreateOr(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Or, LHS, RHS);
}

Instruction *IRFactory::CreateXOr(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::XOr, LHS, RHS);
}

Instruction *IRFactory::CreateAnd(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::And, LHS, RHS);
}

Instruction *IRFactory::CreateLSL(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::LSL, LHS, RHS);
}

Instruction *IRFactory::CreateLSR(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::LSR, LHS, RHS);
}

UnaryInstruction *IRFactory::CreateMov(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::Mov,
                                                   IRType::CreateInt(BitWidth),
                                                   Operand,
                                                   GetCurrentBB());

    Inst->SetId(ID++);
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateMovF(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::MovF,
                                                   IRType::CreateFloat(BitWidth),
                                                   Operand,
                                                   GetCurrentBB());

    Inst->SetId(ID++);
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateSExt(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::SExt,
                                                   IRType::CreateInt(BitWidth),
                                                   Operand,
                                                   GetCurrentBB());

    Inst->SetId(ID++);
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateZExt(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::ZExt,
                                                   IRType::CreateInt(BitWidth),
                                                   Operand,
                                                   GetCurrentBB());
    Inst->SetId(ID++);
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateTrunc(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::Trunc,
                                                   IRType::CreateInt(BitWidth),
                                                   Operand,
                                                   GetCurrentBB());

    Inst->SetId(ID++);
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateFloatToInt(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::FloatToInt,
                                                   IRType::CreateInt(BitWidth),
                                                   Operand,
                                                   GetCurrentBB());
    Inst->SetId(ID++);
    auto InstPtr = Inst.get();
    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateIntToFloat(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::IntToFloat,
                                                   IRType::CreateFloat(BitWidth),
                                                   Operand,
                                                   GetCurrentBB());
    Inst->SetId(ID++);
    auto InstPtr = Inst.get();
    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateBitCast(Value *Operand, const IRType &To)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::BitCast,
                                                   To,
                                                   Operand,
                                                   GetCurrentBB());

    Inst->SetId(ID++);
    auto InstPtr = Inst.get();
    Insert(std::move(Inst));

    return InstPtr;
}

CallInstruction *IRFactory::CreateCall(std::string &FuncName,
                                       std::vector<Value *> Args,
                                       IRType Type,
                                       int StructIdx)
{
    auto Inst    = std::make_unique<CallInstruction>(FuncName,
                                                  Args,
                                                  Type,
                                                  GetCurrentBB(),
                                                  StructIdx);
    auto InstPtr = Inst.get();

    if (!Type.IsVoid())
        Inst->SetId(ID++);

    Insert(std::move(Inst));

    return InstPtr;
}

ReturnInstruction *IRFactory::CreateRet(Value *ReturnVal)
{
    auto Inst    = std::make_unique<ReturnInstruction>(ReturnVal, GetCurrentBB());
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

StackAllocationInstruction *IRFactory::CreateSA(std::string Indentifier, IRType Type)
{
    auto Inst    = std::make_unique<StackAllocationInstruction>(Indentifier,
                                                             Type,
                                                             CurrentModule.GetBB(0));
    auto InstPtr = Inst.get();

    Inst->SetId(ID++);
    CurrentModule.GetBB(0)->InsertSA(std::move(Inst));

    return InstPtr;
}

GetElemPointerInstruction *
    IRFactory::CreateGEP(IRType ResultType, Value *Source, Value *Index)
{
    auto Inst    = std::make_unique<GetElemPointerInstruction>(ResultType,
                                                            Source,
                                                            Index,
                                                            GetCurrentBB());
    auto InstPtr = Inst.get();

    Inst->SetId(ID++);
    Insert(std::move(Inst));

    return InstPtr;
}

StoreInstruction *IRFactory::CreateStore(Value *Source, Value *Destination)
{
    auto Inst = std::make_unique<StoreInstruction>(Source, Destination, GetCurrentBB());
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));
    return InstPtr;
}

LoadInstruction *IRFactory::CreateLoad(IRType ResultType, Value *Source, Value *Offset)
{
    auto Inst =
        std::make_unique<LoadInstruction>(ResultType, Source, Offset, GetCurrentBB());
    auto InstPtr = Inst.get();

    Inst->SetId(ID++);
    Insert(std::move(Inst));

    return InstPtr;
}

MemoryCopyInstruction *
    IRFactory::CreateMemCopy(Value *Destination, Value *Source, std::size_t Bytes)
{
    auto Inst    = std::make_unique<MemoryCopyInstruction>(Destination,
                                                        Source,
                                                        Bytes,
                                                        GetCurrentBB());
    auto InstPtr = Inst.get();

    Inst->SetId(ID++);
    Insert(std::move(Inst));

    return InstPtr;
}

CompareInstruction *IRFactory::CreateCmp(CompareInstruction::CompareRelation Relation,
                                         Value *LHS,
                                         Value *RHS)
{
    auto Inst = std::make_unique<CompareInstruction>(LHS, RHS, Relation, GetCurrentBB());
    auto InstPtr = Inst.get();

    Inst->SetId(ID++);
    Insert(std::move(Inst));

    return InstPtr;
}

JumpInstruction *IRFactory::CreateJump(BasicBlock *Destination)
{
    auto Inst    = std::make_unique<JumpInstruction>(Destination, GetCurrentBB());
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

BranchInstruction *
    IRFactory::CreateBranch(Value *Condition, BasicBlock *True, BasicBlock *False)
{
    auto Inst =
        std::make_unique<BranchInstruction>(Condition, True, False, GetCurrentBB());
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

GlobalVariable *IRFactory::CreateGlobalVar(std::string &Identifier, const IRType Type)
{
    auto GlobalVar = new GlobalVariable(Identifier, Type);
    GlobalVar->SetId(ID++);

    return GlobalVar;
}

GlobalVariable *IRFactory::CreateGlobalVar(std::string &Identifier,
                                           const IRType Type,
                                           std::vector<uint64_t> InitList)
{
    auto GlobalVar = new GlobalVariable(Identifier, Type, std::move(InitList));
    GlobalVar->SetId(ID++);

    return GlobalVar;
}

GlobalVariable *
    IRFactory::CreateGlobalVar(std::string &Identifier, const IRType Type, Value *Val)
{
    auto GlobalVar = new GlobalVariable(Identifier, Type, Val);
    GlobalVar->SetId(ID++);

    return GlobalVar;
}

GlobalVariable *IRFactory::CreateGlobalVar(std::string &Identifier,
                                           const IRType Type,
                                           std::string StrValue)
{
    auto GlobalVar = new GlobalVariable(Identifier, Type, StrValue);
    GlobalVar->SetId(ID++);

    return GlobalVar;
}

void IRFactory::CreateNewFunction(std::string &Name, IRType ReturnType)
{
    CurrentModule.AddFunction(std::move(Function(Name, ReturnType)));
    SymbolTable.clear();
    LabelTable.clear();
    ID = 0;
}

void IRFactory::AddGlobalVariable(Value *GlobalValue)
{
    std::unique_ptr<Value> GV(GlobalValue);
    CurrentModule.AddGlobalVar(std::move(GV));
    SetGlobalScope(false);
}

Value *IRFactory::GetGlobalVar(const std::string &Identifier)
{
    return CurrentModule.GetGlobalVar(Identifier);
}

bool IRFactory::IsGlobalValue(Value *V) const { return CurrentModule.IsGlobalVar(V); }

bool IRFactory::IsGlobalScope() const { return GlobalScope; }

void IRFactory::SetGlobalScope(const bool v) { GlobalScope = v; }

BasicBlock *IRFactory::CreateBasicBlock() { return CurrentModule.CreateBasicBlock(); }

Function *IRFactory::GetCurrentFunction() { return CurrentModule.CurrentFunction(); }

void IRFactory::InsertBB(std::unique_ptr<BasicBlock> BB)
{
    auto NewName = BB->GetName() + std::to_string(LabelTable[BB->GetName()]);
    LabelTable[BB->GetName()]++;
    BB->SetName(NewName);

    GetCurrentFunction()->Insert(std::move(BB));
}

void IRFactory::Insert(std::unique_ptr<FunctionParameter> FP)
{
    FP->SetId(ID++);
    GetCurrentFunction()->Insert(std::move(FP));
}

void IRFactory::AddToSymbolTable(std::string &Identifier, Value *V)
{
    SymbolTable[Identifier] = V;
}

Value *IRFactory::GetSymbolValue(const std::string &Identifier)
{
    return SymbolTable[Identifier];
}

Constant *IRFactory::GetConstant(uint64_t C, uint8_t BW)
{
    std::pair<uint64_t, uint8_t> RequestedConstant {C, BW};
    if (auto ConstVal = IntConstantPool[RequestedConstant].get(); ConstVal != nullptr)
        return ConstVal;

    IntConstantPool[RequestedConstant] = std::make_unique<Constant>(C, BW);

    return IntConstantPool[RequestedConstant].get();
}

Constant *IRFactory::GetConstant(double C, unsigned BW)
{
    if (auto ConstVal = FloatConstantPool[C].get(); ConstVal != nullptr)
        return ConstVal;

    FloatConstantPool[C] = std::make_unique<Constant>(C, BW);
    return FloatConstantPool[C].get();
}

std::vector<BasicBlock *> &IRFactory::GetLoopIncrementBBsTable()
{
    return LoopIncrementBBsTable;
}

std::vector<BasicBlock *> &IRFactory::GetBreakEndBBsTable()
{
    return BreaksTargetBBsTable;
}
