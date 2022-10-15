//
// Created by Organ1sm.
//
#include "MiddleEnd/IR/Value.hpp"
#include "MiddleEnd/IR/Module.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/Function.hpp"

BinaryInstruction *
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

BinaryInstruction *IRFactory::CreateAdd(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Add, LHS, RHS);
}

BinaryInstruction *IRFactory::CreateSub(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Sub, LHS, RHS);
}

BinaryInstruction *IRFactory::CreateMul(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Mul, LHS, RHS);
}

BinaryInstruction *IRFactory::CreateDiv(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Div, LHS, RHS);
}

BinaryInstruction *IRFactory::CreateMod(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Mod, LHS, RHS);
}

BinaryInstruction *IRFactory::CreateOr(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::Or, LHS, RHS);
}

BinaryInstruction *IRFactory::CreateAnd(Value *LHS, Value *RHS)
{
    return CreateBinaryInstruction(Instruction::And, LHS, RHS);
}

UnaryInstruction *IRFactory::CreateSExt(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(
        Instruction::SExt, IRType::CreateInt(BitWidth), Operand, GetCurrentBB());

    Inst->SetId(ID++);
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateTrunc(Value *Operand, uint8_t BitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(
        Instruction::Trunc, IRType::CreateInt(BitWidth), Operand, GetCurrentBB());

    Inst->SetId(ID++);
    auto InstPtr = Inst.get();

    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateFloatToInt(Value *Operand, uint8_t FloatBitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::FloatToInt,
                                                   IRType::CreateFloat(FloatBitWidth),
                                                   Operand, GetCurrentBB());
    Inst->SetId(ID++);
    auto InstPtr = Inst.get();
    Insert(std::move(Inst));

    return InstPtr;
}

UnaryInstruction *IRFactory::CreateIntToFloat(Value *Operand, uint8_t IntBitWidth)
{
    auto Inst = std::make_unique<UnaryInstruction>(Instruction::IntToFloat,
                                                   IRType::CreateFloat(IntBitWidth),
                                                   Operand, GetCurrentBB());
    Inst->SetId(ID++);
    auto InstPtr = Inst.get();
    Insert(std::move(Inst));

    return InstPtr;
}

CallInstruction *
    IRFactory::CreateCall(std::string &FuncName, std::vector<Value *> Args, IRType Type)
{
    auto Inst = std::make_unique<CallInstruction>(FuncName, Args, Type, GetCurrentBB());
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
    auto Inst    = std::make_unique<StackAllocationInstruction>(Indentifier, Type,
                                                             CurrentModule.GetBB(0));
    auto InstPtr = Inst.get();

    Inst->SetId(ID++);
    CurrentModule.GetBB(0)->Insert(std::move(Inst));

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

GlobalVariable *IRFactory::CreateGlobalVar(std::string &Identifier, IRType Type)
{
    auto GlobalVar = new GlobalVariable(Identifier, Type);
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

Constant *IRFactory::GetConstant(uint64_t C)
{
    if (auto ConstVal = IntConstantPool[C].get(); ConstVal != nullptr)
        return ConstVal;

    IntConstantPool[C] = std::make_unique<Constant>(C);
    return IntConstantPool[C].get();
}

Constant *IRFactory::GetConstant(double C)
{
    if (auto ConstVal = FloatConstantPool[C].get(); ConstVal != nullptr)
        return ConstVal;

    FloatConstantPool[C] = std::make_unique<Constant>(C);
    return FloatConstantPool[C].get();
}
