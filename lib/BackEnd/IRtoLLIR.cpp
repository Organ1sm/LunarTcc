#include <cassert>
#include <type_traits>
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
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


void IRtoLLIR::Reset()
{
    StructToRegMap.clear();
    StructByIDToRegMap.clear();
    IRVregToLLIRVreg.clear();
}

MachineOperand IRtoLLIR::GetMachineOperandFromValue(Value *Val, MachineFunction *MF)
{
    if (Val->IsRegister())
    {
        unsigned NextVReg;

        if (IRVregToLLIRVreg.count(Val->GetID()) > 0)
            NextVReg = IRVregToLLIRVreg[Val->GetID()];
        else
        {
            NextVReg                       = MF->GetNextAvailableVirtualRegister();
            IRVregToLLIRVreg[Val->GetID()] = NextVReg;
        }

        auto BitWidth = Val->GetBitWidth();
        auto VReg     = MachineOperand::CreateVirtualRegister(NextVReg);

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
        auto Result     = GetMachineOperandFromValue((Value *)I, ParentFunction);
        auto FirstStep  = GetMachineOperandFromValue(I->GetLHS(), ParentFunction);
        auto SecondStep = GetMachineOperandFromValue(I->GetRHS(), ParentFunction);

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(FirstStep);
        ResultMI.AddOperand(SecondStep);
    }
    // Two address ALU instructions: Instr Result, Op
    else if (auto I = dynamic_cast<UnaryInstruction *>(Instr); I != nullptr)
    {
        auto Result = GetMachineOperandFromValue((Value *)I, ParentFunction);
        auto Op     = GetMachineOperandFromValue(I->GetOperand(), ParentFunction);

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(Op);
    }
    // Store Instruction: Store [Address], Src
    else if (auto I = dynamic_cast<StoreInstruction *>(Instr); I != nullptr)
    {
        assert(I->GetMemoryLocation()->IsRegister() && "Must be a register");

        ResultMI.AddAttribute(MachineInstruction::IsSTORE);

        auto AddressReg = I->GetMemoryLocation()->GetID();
        if (IRVregToLLIRVreg.count(AddressReg) > 0)
            AddressReg = IRVregToLLIRVreg[AddressReg];

        if (ParentFunction->IsStackSlot(AddressReg))
            ResultMI.AddStackAccess(AddressReg);
        else
            ResultMI.AddMemory(AddressReg);

        // if Source is a struct an not struct pointer
        if (I->GetSavedValue()->GetTypeRef().IsStruct()
            && !I->GetSavedValue()->GetTypeRef().IsPointer())
        {
            if (auto FP = dynamic_cast<FunctionParameter *>(I->GetSavedValue());
                FP != nullptr)
            {
                unsigned RegSize = TM->GetPointerSize();
                auto StructName  = FP->GetName();

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

                    // insert all the stores but the last one, that will be the return
                    // value
                    if (Counter < StructToRegMap[StructName].size())
                        BB->InsertInstr(CurrentStore);
                }

                return CurrentStore;
            }
            // Handle other cases, like when the structure is a return value from a
            // function
            else
            {
                unsigned StructBitSize =
                    (I->GetSavedValue()->GetTypeRef().GetByteSize() * 8);
                unsigned MaxRegSize = TM->GetPointerSize();
                unsigned RegsCount =
                    GetNextAlignedValue(StructBitSize, MaxRegSize) / MaxRegSize;

                auto &RetRegs = TM->GetABI()->GetReturnRegisters();

                assert(RegsCount <= RetRegs.size());

                MachineInstruction Store;
                for (std::size_t i = 0; i < RegsCount; i++)
                {
                    Store = MachineInstruction(MachineInstruction::Store, BB);

                    Store.AddStackAccess(AddressReg, (TM->GetPointerSize() / 8) * i);
                    Store.AddRegister(RetRegs[i]->GetID(), TM->GetPointerSize());

                    if (i == (RegsCount - 1))
                        return Store;

                    BB->InsertInstr(Store);
                }
            }
        }
        else
        {
            ResultMI.AddOperand(
                GetMachineOperandFromValue(I->GetSavedValue(), ParentFunction));
        }
    }
    // Load Instruction: Load Dest, [Address]
    else if (auto I = dynamic_cast<LoadInstruction *>(Instr); I != nullptr)
    {
        assert(I->GetMemoryLocation()->IsRegister() && "Must be a register");

        ResultMI.AddAttribute(MachineInstruction::IsLOAD);
        ResultMI.AddOperand(GetMachineOperandFromValue((Value *)I, ParentFunction));

        auto AddressReg = I->GetMemoryLocation()->GetID();
        if (IRVregToLLIRVreg.count(AddressReg) > 0)
            AddressReg = IRVregToLLIRVreg[AddressReg];

        if (ParentFunction->IsStackSlot(AddressReg))
            ResultMI.AddStackAccess(AddressReg);
        else
            ResultMI.AddMemory(AddressReg);

        // if the destination is a struct and not a struct pointer
        if (I->GetTypeRef().IsStruct() && !I->GetTypeRef().IsPointer())
        {
            unsigned StructBitSize = (I->GetTypeRef().GetByteSize() * 8);
            unsigned RegSize       = TM->GetPointerSize();
            unsigned RegsCount = GetNextAlignedValue(StructBitSize, RegSize) / RegSize;

            // Create loads for the registers which holds the struct parts
            for (size_t i = 0; i < RegsCount; i++)
            {
                auto CurrentLoad = MachineInstruction(MachineInstruction::Load, BB);
                auto NewVReg     = ParentFunction->GetNextAvailableVirtualRegister();

                CurrentLoad.AddVirtualRegister(NewVReg, RegSize);
                StructByIDToRegMap[I->GetID()].push_back(NewVReg);
                CurrentLoad.AddStackAccess(AddressReg, i * RegSize / 8);

                // insert all the stores but the last one, that will be the return value
                if (i + 1 < RegsCount)
                    BB->InsertInstr(CurrentLoad);
                else
                    return CurrentLoad;
            }
        }
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

        auto Dest = GetMachineOperandFromValue((Value *)I, ParentFunction);
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
                assert(StructByIDToRegMap.count(Param->GetID()) > 0
                       && "The map does not know about this struct param");
                // how many register are used to pass this struct

                for (auto VReg : StructByIDToRegMap[Param->GetID()])
                {
                    Ins = MachineInstruction(MachineInstruction::Mov, BB);

                    Ins.AddRegister(TargetArgRegs[ParamCounter]->GetID(),
                                    TargetArgRegs[ParamCounter]->GetBitWidth());
                    Ins.AddVirtualRegister(VReg, TM->GetPointerSize());

                    BB->InsertInstr(Ins);
                    ParamCounter++;
                }
            }
            else
            {
                Ins = MachineInstruction(MachineInstruction::Mov, BB);

                Ins.AddRegister(TargetArgRegs[ParamCounter]->GetID(),
                                TargetArgRegs[ParamCounter]->GetBitWidth());
                Ins.AddOperand(GetMachineOperandFromValue(Param, ParentFunction));

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

        ResultMI.AddOperand(
            GetMachineOperandFromValue(I->GetCondition(), ParentFunction));
        ResultMI.AddLabel(TrueLabel);

        if (I->HasFalseLabel())
            ResultMI.AddLabel(TrueLabel);    // Fixme: There should be false-lable?
    }
    // Compare Instruction: cmp relation br1, br2
    else if (auto I = dynamic_cast<CompareInstruction *>(Instr); I != nullptr)
    {
        auto Result      = GetMachineOperandFromValue((Value *)I, ParentFunction);
        auto FirstSrcOp  = GetMachineOperandFromValue(I->GetLHS(), ParentFunction);
        auto SecondSrcOp = GetMachineOperandFromValue(I->GetRHS(), ParentFunction);

        ResultMI.AddOperand(Result);
        ResultMI.AddOperand(FirstSrcOp);
        ResultMI.AddOperand(SecondSrcOp);

        ResultMI.SetAttributes(I->GetRelation());
    }
    // Ret Instruction: ret op
    else if (auto I = dynamic_cast<ReturnInstruction *>(Instr); I != nullptr)
    {
        auto Result = GetMachineOperandFromValue(I->GetRetVal(), ParentFunction);
        ResultMI.AddOperand(Result);


        // insert load to load in the return val to the return registers
        auto &TargetRetRegs = TM->GetABI()->GetReturnRegisters();
        if (I->GetRetVal()->GetTypeRef().IsStruct())
        {
            // how many register are used to pass this struct
            unsigned StructBitSize = (I->GetRetVal()->GetTypeRef().GetByteSize() * 8);
            unsigned MaxRegSize    = TM->GetPointerSize();
            unsigned RegsCount =
                GetNextAlignedValue(StructBitSize, MaxRegSize) / MaxRegSize;

            for (size_t i = 0; i < RegsCount; i++)
            {
                auto Instr = MachineInstruction(MachineInstruction::Load, BB);

                Instr.AddRegister(TargetRetRegs[i]->GetID(),
                                  TargetRetRegs[i]->GetBitWidth());
                Instr.AddStackAccess(I->GetRetVal()->GetID(),
                                     i * (TM->GetPointerSize() / 8));

                BB->InsertInstr(Instr);
            }
        }
        else if (I->GetRetVal()->IsConstant())
        {
            auto &RetRegs = TM->GetABI()->GetReturnRegisters();
            auto LoadImm  = MachineInstruction(MachineInstruction::LoadImm, BB);

            LoadImm.AddRegister(RetRegs[0]->GetID(), RetRegs[0]->GetBitWidth());
            LoadImm.AddOperand(
                GetMachineOperandFromValue(I->GetRetVal(), ParentFunction));

            BB->InsertInstr(LoadImm);
        }
    }
    else if (auto I = dynamic_cast<MemoryCopyInstruction *>(Instr); I != nullptr)
    {
        // lower this into load and store pairs if used with structs lower then
        // a certain size (for now be it the size which can be passed by value)
        // otherwise create a call maybe to an intrinsic memcopy function
        for (size_t i = 0; i < (I->GetSize() / /* TODO: use alignment here */ 4); i++)
        {
            auto Load    = MachineInstruction(MachineInstruction::Load, BB);
            auto NewVReg = ParentFunction->GetNextAvailableVirtualRegister();
            Load.AddRegister(NewVReg, /* TODO: use alignment here */ 32);
            Load.AddStackAccess(I->GetSource()->GetID(),
                                i * /* TODO: use alignment here */ 4);
            BB->InsertInstr(Load);

            auto Store = MachineInstruction(MachineInstruction::Store, BB);
            Store.AddStackAccess(I->GetDestination()->GetID(),
                                 i * /* TODO: use alignment here */ 4);
            Store.AddRegister(NewVReg, /* TODO: use alignment here */ 32);
            // TODO: Change the function so it does not return the instruction but
            // insert it in the function so don't have to do these annoying returns
            if (i == ((I->GetSize() / /* TODO: use alignment here */ 4) - 1))
                return Store;
            BB->InsertInstr(Store);
        }
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
        if (Param->GetTypeRef().IsStruct() && !Param->GetTypeRef().IsPointer())
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
                StructToRegMap[StructName].push_back(NextVreg);
                Func->InsertParameter(NextVreg,
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
        Reset();
        
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
