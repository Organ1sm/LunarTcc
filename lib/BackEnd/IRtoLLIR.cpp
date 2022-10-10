#include <cassert>
#include <type_traits>
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/IRtoLLIR.hpp"
#include "BackEnd/LowLevelType.hpp"
#include "BackEnd/TargetInstructionLegalizer.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"


MachineOperand GetMachineOperandFromValue(Value *Val)
{
    if (Val->IsRegister())
    {
        return MachineOperand::CreateVirtualRegister(Val->GetID());
    }
    else if (Val->IsParameter())
    {
        MachineOperand Result;

        // FIXME: Only handling int params now, handle others too
        // And add type to registers and others too
        Result.SetTypeToParameter();
        Result.SetType(LowLevelType::CreateInt(32));
        Result.SetValue(Val->GetID());

        return Result;
    }
    else if (Val->IsConstant())
    {
        auto C = dynamic_cast<Constant *>(Val);
        assert(!C->IsFPConst() && "TODO");

        return MachineOperand::CreateImmediate(C->GetIntValue());
    }
    else
    {
        assert(!"Unhandled MachineOperand case.");
    }

    return MachineOperand();
}

MachineInstruction ConvertToMachineInstr(Instruction *Instr,
                                         MachineBasicBlock *BB,
                                         std::vector<MachineBasicBlock> &BBS)
{
    auto Operation      = Instr->GetInstructionKind();
    auto ParentFunction = BB->GetParent();
    auto ResultMI       = MachineInstruction((unsigned)Operation + (1 << 16), BB);

    // Three Address Instructions: Instr Result, Op1, Op2
    if (auto I = dynamic_cast<BinaryInstruction *>(Instr); I != nullptr)
    {
        auto Result     = GetMachineOperandFromValue((Value *)I);
        auto FirstStep  = GetMachineOperandFromValue(I->GetLHS());
        auto SecondStep = GetMachineOperandFromValue(I->GetRHS());

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(FirstStep);
        ResultMI.AddOperand(SecondStep);
    }
    // Store Instruction: Store [Address], Src
    else if (auto I = dynamic_cast<StoreInstruction *>(Instr); I != nullptr)
    {
        assert(I->GetMemoryLocation()->IsRegister() && "Must be a register");

        auto AddressReg = I->GetMemoryLocation()->GetID();

        if (ParentFunction->IsStackSlot(AddressReg))
            ResultMI.AddStackAccess(AddressReg);
        else
            ResultMI.AddMemory(AddressReg);

        ResultMI.AddOperand(GetMachineOperandFromValue(I->GetSavedValue()));
    }
    // Load Instruction: Load Dest, [Address]
    else if (auto I = dynamic_cast<LoadInstruction *>(Instr); I != nullptr)
    {
        assert(I->GetMemoryLocation()->IsRegister() && "Must be a register");

        auto AddressReg = I->GetMemoryLocation()->GetID();
        ResultMI.AddOperand(GetMachineOperandFromValue((Value *)I));

        if (ParentFunction->IsStackSlot(AddressReg))
            ResultMI.AddStackAccess(AddressReg);
        else
            ResultMI.AddMemory(AddressReg);
    }
    // Jump Instruction: Jump Label
    else if (auto I = dynamic_cast<JumpInstruction *>(Instr); I != nullptr)
    {
        for (auto &bb : BBS)
        {
            if (I->GetTargetLabelName() == bb.GetName())
            {
                ResultMI.AddLabel(bb.GetName().c_str());
                break;
            }
        }
    }
    // Branch Instruction : Br op label1 label2
    else if (auto I = dynamic_cast<BranchInstruction *>(Instr); I != nullptr)
    {
        const char *TrueLabel  = nullptr;
        const char *FalseLabel = nullptr;

        for (auto &bb : BBS)
        {
            if (TrueLabel == nullptr && I->GetTrueLabelName() == bb.GetName())
                TrueLabel = bb.GetName().c_str();

            if (FalseLabel == nullptr && I->HasFalseLabel()
                && I->GetFalseLabelName() == bb.GetName())
                FalseLabel = bb.GetName().c_str();
        }

        ResultMI.AddOperand(GetMachineOperandFromValue(I->GetCondition()));
        ResultMI.AddLabel(TrueLabel);

        if (I->HasFalseLabel())
            ResultMI.AddLabel(TrueLabel);    // Fixme: There should be false-lable?
    }
    // Compare Instruction: cmp relation br1, br2
    else if (auto I = dynamic_cast<CompareInstruction *>(Instr); I != nullptr)
    {
        auto Result      = GetMachineOperandFromValue((Value *)I);
        auto FirstSrcOp  = GetMachineOperandFromValue(I->GetLHS());
        auto SecondSrcOp = GetMachineOperandFromValue(I->GetRHS());

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(FirstSrcOp);
        ResultMI.AddOperand(SecondSrcOp);

        ResultMI.SetAttributes(I->GetRelation());
    }
    // Ret Instruction: ret op
    else if (auto I = dynamic_cast<ReturnInstruction *>(Instr); I != nullptr)
    {
        auto Result = GetMachineOperandFromValue(I->GetRetVal());
        ResultMI.AddOperand(Result);
    }
    else
    {
        assert(!"Unimplemented Instruction.");
    }

    return ResultMI;
}



void HandleStackAllocation(StackAllocationInstruction *Instr, MachineFunction *Func)
{
    Func->InsertStackSlot(Instr->GetID(), Instr->GetType().GetByteSize());
}

void HandleFunctionParams(Function &F, MachineFunction *Func)
{
    for (auto &Param : F.GetParameters())
    {
        auto ParamID = Param->GetID();
        assert(Param->IsIntType() && "Other types Unimplemented yet.");

        auto ParamSize = Param->GetBitWidth();

        Func->InsertParameter(ParamID, LowLevelType::CreateInt(ParamSize));
    }
}

void IRtoLLIR::GenerateLLIRFromIR()
{
    for (auto &Func : IRM.GetFunctions())
    {
        auto NewMachineFunc = MachineFunction {};
        std::vector<MachineBasicBlock> MBBs;

        this->TU->AddFunction(NewMachineFunc);

        MachineFunction *MFunction = TU->GetCurrentFunction();

        MFunction->SetName(Func.GetName());
        HandleFunctionParams(Func, MFunction);

        // Create all basic block first with their name , so jumps can
        // refer to them already
        for (auto &BB : Func.GetBasicBlocks())
            MBBs.push_back(MachineBasicBlock {BB.get()->GetName(), MFunction});

        // Assign the basic block to the new function
        MFunction->SetBasicBlocks(std::move(MBBs));
        auto &MFuncMBBs = MFunction->GetBasicBlocks();


        unsigned BBCounter = 0;
        for (auto &BB : Func.GetBasicBlocks())
        {
            for (auto &Instr : BB->GetInstructions())
            {
                auto InstrPtr = Instr.get();

                if (InstrPtr->IsStackAllocation())
                {
                    HandleStackAllocation((StackAllocationInstruction *)InstrPtr,
                                          MFunction);
                    continue;
                }

                MFuncMBBs[BBCounter].InsertInstr(
                    ConvertToMachineInstr(InstrPtr, &MFuncMBBs[BBCounter], MFuncMBBs));
            }

            BBCounter++;
        }
    }
}
