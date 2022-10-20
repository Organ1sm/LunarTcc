#include <cassert>
#include <type_traits>
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/IRtoLLIR.hpp"
#include "BackEnd/LowLevelType.hpp"
#include "BackEnd/Support.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/TargetInstructionLegalizer.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "MiddleEnd/IR/Module.hpp"


MachineOperand GetMachineOperandFromValue(Value *Val, TargetMachine *TM)
{
    if (Val->IsRegister())
    {
        auto BitWidth = Val->GetBitWidth();
        auto VReg     = MachineOperand::CreateVirtualRegister(Val->GetID());

        if (Val->GetTypeRef().IsPointer())
            VReg.SetType(LowLevelType::CreatePtr(TM->GetPointerSize()));
        else
            VReg.SetType(LowLevelType::CreateInt(BitWidth));

        return VReg;
    }
    else if (Val->IsParameter())
    {
        auto Result   = MachineOperand::CreateParameter(Val->GetID());
        auto BitWidth = Val->GetBitWidth();

        // FIXME: Only handling int params now, handle others too
        // And add type to registers and others too
        if (Val->GetTypeRef().IsPointer())
            Result.SetType(LowLevelType::CreatePtr(TM->GetPointerSize()));
        else
            Result.SetType(LowLevelType::CreateInt(BitWidth));

        return Result;
    }
    else if (Val->IsConstant())
    {
        auto C = dynamic_cast<Constant *>(Val);
        assert(!C->IsFPConst() && "TODO");

        return MachineOperand::CreateImmediate(C->GetIntValue(), 32);
    }
    else
    {
        assert(!"Unhandled MachineOperand case.");
    }

    return MachineOperand();
}

MachineInstruction IRtoLLIR::ConvertToMachineInstr(Instruction *Instr,
                                                   MachineBasicBlock *BB,
                                                   std::vector<MachineBasicBlock> &BBS)
{
    auto Operation      = Instr->GetInstructionKind();
    auto ParentFunction = BB->GetParent();
    auto ResultMI       = MachineInstruction((unsigned)Operation + (1 << 16), BB);

    // Three Address Instructions: Instr Result, Op1, Op2
    if (auto I = dynamic_cast<BinaryInstruction *>(Instr); I != nullptr)
    {
        auto Result     = GetMachineOperandFromValue((Value *)I, TM);
        auto FirstStep  = GetMachineOperandFromValue(I->GetLHS(), TM);
        auto SecondStep = GetMachineOperandFromValue(I->GetRHS(), TM);

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(FirstStep);
        ResultMI.AddOperand(SecondStep);
    }
    // Two address ALU instructions: Instr Result, Op
    else if (auto I = dynamic_cast<UnaryInstruction *>(Instr); I != nullptr)
    {
        auto Result = GetMachineOperandFromValue((Value *)I, TM);
        auto Op     = GetMachineOperandFromValue(I->GetOperand(), TM);

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(Op);
    }
    // Store Instruction: Store [Address], Src
    else if (auto I = dynamic_cast<StoreInstruction *>(Instr); I != nullptr)
    {
        assert(I->GetMemoryLocation()->IsRegister() && "Must be a register");

        ResultMI.AddAttribute(MachineInstruction::IsSTORE);

        auto AddressReg = I->GetMemoryLocation()->GetID();

        if (ParentFunction->IsStackSlot(AddressReg))
            ResultMI.AddStackAccess(AddressReg);
        else
            ResultMI.AddMemory(AddressReg);

        // if Source is a struct an not struct pointer
        if (I->GetSavedValue()->GetTypeRef().IsStruct()
            && !I->GetSavedValue()->GetTypeRef().IsPointer())
        {
            unsigned RegSize = TM->GetPointerSize();
            auto StructName =
                static_cast<FunctionParameter *>(I->GetSavedValue())->GetName();

            assert(!StructToRegMap[StructName].empty() && "Unknown struct name");

            MachineInstruction CurrentStore;
            unsigned Counter = 0;

            // Create Store for the Register which holds the struct parts
            for (auto ParamId : StructToRegMap[StructName])
            {
                CurrentStore = MachineInstruction(MachineInstruction::Store, BB);

                CurrentStore.AddStackAccess(AddressReg, Counter * RegSize / 8);
                CurrentStore.AddVirtualRegister(ParamId, RegSize);

                Counter++;

                // insert all the stores but the last one, that will be the return value
                if (Counter < StructToRegMap[StructName].size())
                    BB->InsertInstr(CurrentStore);
            }

            return CurrentStore;
        }
        else
        {
            ResultMI.AddOperand(GetMachineOperandFromValue(I->GetSavedValue(), TM));
        }
    }
    // Load Instruction: Load Dest, [Address]
    else if (auto I = dynamic_cast<LoadInstruction *>(Instr); I != nullptr)
    {
        assert(I->GetMemoryLocation()->IsRegister() && "Must be a register");

        ResultMI.AddAttribute(MachineInstruction::IsLOAD);
        ResultMI.AddOperand(GetMachineOperandFromValue((Value *)I, TM));

        auto AddressReg = I->GetMemoryLocation()->GetID();

        if (ParentFunction->IsStackSlot(AddressReg))
            ResultMI.AddStackAccess(AddressReg);
        else
            ResultMI.AddMemory(AddressReg);
    }
    // GEP instruction: GEP Dest, Source, list of indexes
    // to
    //   STACK_ADDRESS Dest, Source
    // **arithmetic instructions to calculate the index** ex: 1 index which is 6
    //   MUL idx, sizeof(Source[0]), 6
    //   ADD Dest, Dest, idx
    else if (auto I = dynamic_cast<GetElemPointerInstruction *>(Instr); I != nullptr)
    {
        auto SA = MachineInstruction(MachineInstruction::StackAddress, BB);

        auto Dest = GetMachineOperandFromValue((Value *)I, TM);
        SA.AddOperand(Dest);

        auto AddressReg = I->GetSource()->GetID();
        assert(ParentFunction->IsStackSlot(AddressReg) && "Must be stack slot");
        SA.AddStackAccess(AddressReg);

        BB->InsertInstr(SA);

        auto &SourceType           = I->GetSource()->GetTypeRef();
        unsigned ConstantIndexPart = 0;
        auto Index                 = ((Constant *)I->GetIndex())->GetIntValue();
        if (I->GetIndex()->IsConstant())
        {
            if (!SourceType.IsStruct())
                ConstantIndexPart = (SourceType.CalcElemSize(0) * Index);
            else    // its a struct and has to determine the offset other way
                ConstantIndexPart = SourceType.GetElemByteOffset(Index);
        }
        else
            assert(!"Unimplemented for expression indexes");

        // FIXME: For unknown reason this not work as expected. Investigate it!
        // If there is nothing to add, then exit now
        //    if (ConstantIndexPart == 0)
        //      return SA;

        auto ADD = MachineInstruction(MachineInstruction::Add, BB);
        ADD.AddOperand(Dest);
        ADD.AddOperand(Dest);
        ADD.AddImmediate(ConstantIndexPart, Dest.GetSize());

        return ADD;
    }
    // Call Instruction: call Result functionName(param1, ...)
    else if (auto I = dynamic_cast<CallInstruction *>(Instr); I != nullptr)
    {
        // The function has a call Instruction
        ParentFunction->SetToCaller();

        // insert copy/Mov -s for each paramter to move them to the right registers
        // ignoring the case when there is too much paramter and has to pass
        // some parameters on the stack

        auto &TargetArgRegs   = TM->GetABI()->GetArgumentRegisters();
        unsigned ParamCounter = 0;

        for (auto *Param : I->GetArgs())
        {
            MachineInstruction Ins;

            if (Param->GetTypeRef().IsStruct())
            {
                // how many register are used to pass this struct
                unsigned StructBitSize = (Param->GetTypeRef().GetByteSize() * 8);
                unsigned MaxRegSize    = TM->GetPointerSize();
                unsigned RegsCount =
                    GetNextAlignedValue(StructBitSize, MaxRegSize) / MaxRegSize;

                for (std::size_t i = 0; i < RegsCount; i++)
                {
                    Ins = MachineInstruction(MachineInstruction::Load, BB);

                    Ins.AddRegister(TargetArgRegs[ParamCounter]->GetID(),
                                    TargetArgRegs[ParamCounter]->GetBitWidth());
                    Ins.AddStackAccess(Param->GetID(), i * (TM->GetPointerSize() / 8));

                    BB->InsertInstr(Ins);
                    ParamCounter++;
                }
            }
            else
            {
                Ins = MachineInstruction(MachineInstruction::Mov, BB);

                Ins.AddRegister(TargetArgRegs[ParamCounter]->GetID(),
                                TargetArgRegs[ParamCounter]->GetBitWidth());
                Ins.AddOperand(GetMachineOperandFromValue(Param, TM));

                BB->InsertInstr(Ins);
                ParamCounter++;
            }
        }
        ResultMI.AddFunctionName(I->GetName().c_str());

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

        ResultMI.AddOperand(GetMachineOperandFromValue(I->GetCondition(), TM));
        ResultMI.AddLabel(TrueLabel);

        if (I->HasFalseLabel())
            ResultMI.AddLabel(TrueLabel);    // Fixme: There should be false-lable?
    }
    // Compare Instruction: cmp relation br1, br2
    else if (auto I = dynamic_cast<CompareInstruction *>(Instr); I != nullptr)
    {
        auto Result      = GetMachineOperandFromValue((Value *)I, TM);
        auto FirstSrcOp  = GetMachineOperandFromValue(I->GetLHS(), TM);
        auto SecondSrcOp = GetMachineOperandFromValue(I->GetRHS(), TM);

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(FirstSrcOp);
        ResultMI.AddOperand(SecondSrcOp);

        ResultMI.SetAttributes(I->GetRelation());
    }
    // Ret Instruction: ret op
    else if (auto I = dynamic_cast<ReturnInstruction *>(Instr); I != nullptr)
    {
        auto Result = GetMachineOperandFromValue(I->GetRetVal(), TM);
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

void IRtoLLIR::HandleFunctionParams(Function &F, MachineFunction *Func)
{
    for (auto &Param : F.GetParameters())
    {
        auto ParamID   = Param->GetID();
        auto ParamSize = Param->GetBitWidth();

        // Handle structs
        if (Param->GetTypeRef().IsStruct())
        {
            auto StructName = Param->GetName();
            // Pointer size also represents the architecture bit size and more
            // importantly the largest bitwidth a general register can have for the
            // given target
            // TODO: revisit this statement later and refine the implementation
            // for example have a function which check all registers and decide the
            // max size that way, or the max possible size of parameter registers
            // but for AArch64 and RISC-V its sure the bit size of the architecture

            // FIXME: The maximum allowed structure size which allowed to be passed
            // by the target is target dependent. Remove the hardcoded value and
            // ask the target for the right size.
            unsigned MaxStructSize = 128;    // bit size
            for (size_t i = 0; i < MaxStructSize / TM->GetPointerSize(); i++)
            {
                auto NextVreg = Func->GetNextAvailableVirtualRegister();
                StructToRegMap[StructName].push_back(ParamID + 1000000 + i);
                Func->InsertParameter(ParamID + 1000000 + i,
                                      LowLevelType::CreateInt(TM->GetPointerSize()));
            }

            continue;
        }

        if (Param->GetTypeRef().IsPointer())
            Func->InsertParameter(ParamID, LowLevelType::CreatePtr(TM->GetPointerSize()));
        else
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
