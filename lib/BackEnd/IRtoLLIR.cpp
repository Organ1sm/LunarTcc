#include <cassert>
#include <type_traits>
#include "BackEnd/GlobalData.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/IRtoLLIR.hpp"
#include "BackEnd/LowLevelType.hpp"
#include "BackEnd/Support.hpp"
#include "BackEnd/TargetArchs/RISCV/RISCVTargetMachine.hpp"
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
    ParamByIDToRegMap.clear();
    IRVregToLLIRVreg.clear();
    SpilledReturnValuesIDToStackID.clear();
}

MachineOperand IRtoLLIR::GetMachineOperandFromValue(Value *Val, bool IsDef)
{
    assert(Val && CurrentBB && ParentFunction);

    if (Val->IsRegister())
    {
        unsigned NextVReg;
        auto BitWidth = Val->GetBitWidth();

        if (Val->GetTypeRef().IsPointer() &&
            dynamic_cast<StackAllocationInstruction *>(Val) == nullptr)
            BitWidth = TM->GetPointerSize();

        // If the register were spilled,
        // (example: function return values are spilled to the stack),
        // then load the value in first into a VReg and return this VReg as LLIR VReg.
        // TODO: Investigate if this is the appropriate place and way to do this
        if (ParentFunction->IsStackSlot(Val->GetID()) && !IsDef &&
            IRVregToLLIRVreg.count(Val->GetID()) == 0)
        {
            auto Instr = MachineInstruction(MachineInstruction::Load, CurrentBB);

            NextVReg = ParentFunction->GetNextAvailableVirtualRegister();
            Instr.AddVirtualRegister(NextVReg, BitWidth);
            Instr.AddStackAccess(Val->GetID());

            CurrentBB->InsertInstr(Instr);
        }
        // If the IR VReg is mapped already to an LLIR VReg then use that
        else if (IRVregToLLIRVreg.count(Val->GetID()) > 0)
        {
            if (!IsDef && ParentFunction->IsStackSlot(IRVregToLLIRVreg[Val->GetID()]))
            {
                auto Instr = MachineInstruction(MachineInstruction::Load, CurrentBB);
                NextVReg   = ParentFunction->GetNextAvailableVirtualRegister();

                Instr.AddVirtualRegister(NextVReg, BitWidth);
                Instr.AddStackAccess(IRVregToLLIRVreg[Val->GetID()]);

                CurrentBB->InsertInstr(Instr);
            }
            else
                NextVReg = IRVregToLLIRVreg[Val->GetID()];
        }
        else if (SpilledReturnValuesIDToStackID.count(Val->GetID()))
        {
            auto Instr = MachineInstruction(MachineInstruction::Load, CurrentBB);
            NextVReg   = ParentFunction->GetNextAvailableVirtualRegister();

            Instr.AddVirtualRegister(NextVReg, BitWidth);
            Instr.AddStackAccess(SpilledReturnValuesIDToStackID[Val->GetID()]);

            CurrentBB->InsertInstr(Instr);
        }
        // Otherwise get the next available LLIR VReg and create a mapping entry
        else
        {
            NextVReg = ParentFunction->GetNextAvailableVirtualRegister();
            IRVregToLLIRVreg[Val->GetID()] = NextVReg;
        }

        auto VReg = MachineOperand::CreateVirtualRegister(NextVReg);

        if (Val->GetTypeRef().IsPointer())
            VReg.SetType(LowLevelType::CreatePtr(TM->GetPointerSize()));
        else
            VReg.SetType(LowLevelType::CreateScalar(BitWidth));

        return VReg;
    }
    else if (Val->IsParameter())
    {
        auto Result   = MachineOperand::CreateParameter(Val->GetID());
        auto BitWidth = Val->GetBitWidth();

        if (Val->GetTypeRef().IsPointer())
            Result.SetType(LowLevelType::CreatePtr(TM->GetPointerSize()));
        else
            Result.SetType(LowLevelType::CreateScalar(BitWidth));

        return Result;
    }
    else if (Val->IsConstant())
    {
        auto C = dynamic_cast<Constant *>(Val);

        MachineOperand Result =
            C->IsFPType() ?
                MachineOperand::CreateFPImmediate(C->GetFloatValue(), C->GetBitWidth()) :
                MachineOperand::CreateImmediate(C->GetIntValue(), C->GetBitWidth());

        return Result;
    }
    else if (Val->IsGlobalVar())
    {
        auto Instr    = MachineInstruction(MachineInstruction::GlobalAddress, CurrentBB);
        auto NextVReg = ParentFunction->GetNextAvailableVirtualRegister();

        Instr.AddVirtualRegister(NextVReg, TM->GetPointerSize());
        Instr.AddGlobalSymbol(static_cast<GlobalVariable *>(Val)->GetName());

        CurrentBB->InsertInstr(Instr);

        auto MO = MachineOperand::CreateVirtualRegister(NextVReg);
        MO.SetType(LowLevelType::CreatePtr(TM->GetPointerSize()));

        return MO;
    }
    else
    {
        assert(!"Unhandled MachineOperand case.");
    }

    return MachineOperand();
}

unsigned IRtoLLIR::GetIDFromValue(Value *Val)
{
    assert(Val);

    unsigned Ret = IRVregToLLIRVreg.count(Val->GetID()) > 0 ?
                       IRVregToLLIRVreg[Val->GetID()] :
                       Val->GetID();

    return Ret;
}

MachineInstruction IRtoLLIR::HandleBinaryInstruction(BinaryInstruction *I)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    auto Result     = GetMachineOperandFromValue((Value *)I, true);
    auto FirstStep  = GetMachineOperandFromValue(I->GetLHS());
    auto SecondStep = GetMachineOperandFromValue(I->GetRHS());

    ResultMI.AddOperand(Result);
    ResultMI.AddOperand(FirstStep);
    ResultMI.AddOperand(SecondStep);

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleUnaryInstruction(UnaryInstruction *I)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    auto Result = GetMachineOperandFromValue((Value *)I, true);

    MachineOperand OP;
    // if a ptr to ptr cast end both pointer at the same ptr level
    // ex:
    //   i32* to i8*, then issue a STACK_ADDRESS instruction
    auto LHS = I->GetTypeRef();
    auto RHS = I->GetOperand()->GetTypeRef();
    if (Operation == Instruction::BitCast)
    {
        if (LHS.IsPointer() && RHS.IsPointer() &&
            LHS.GetPointerLevel() == RHS.GetPointerLevel() &&
            ParentFunction->IsStackSlot(I->GetOperand()->GetID()))
        {
            if (SpilledReturnValuesIDToStackID.count(I->GetOperand()->GetID()) == 0)
            {
                ResultMI.SetOpcode(MachineInstruction::StackAddress);
                OP = MachineOperand::CreateStackAccess(I->GetOperand()->GetID());
            }
            // If the stack slot is actually a spilled return value, then the cast
            // actually if for the spilled value, therefore a load must be issued.
            // Also for casting pointer to pointers at this level no more
            // instruction is required, therefore the single load is enough here.
            else
            {
                ResultMI.SetOpcode(MachineInstruction::Load);
                OP = MachineOperand::CreateStackAccess(GetIDFromValue(I->GetOperand()));
            }
        }
        else
        {
            // otherwise use a move
            ResultMI.SetOpcode(MachineInstruction::Mov);
            OP = GetMachineOperandFromValue(
                I->GetOperand(),
                static_cast<unsigned>(Operation) ==
                    static_cast<unsigned>(MachineInstruction::BitCast));
        }
    }
    else
    {
        OP = GetMachineOperandFromValue(I->GetOperand());
    }

    ResultMI.AddOperand(Result);
    ResultMI.AddOperand(OP);

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleStoreInstruction(StoreInstruction *I)
{
    assert(I->GetMemoryLocation()->IsRegister() ||
           I->GetMemoryLocation()->IsGlobalVar() && "Forbidden destination");

    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    unsigned GlobAddrReg;
    unsigned AddressReg;

    if (I->GetMemoryLocation()->IsGlobalVar())
    {
        auto GlobalAddress =
            MachineInstruction(MachineInstruction::GlobalAddress, CurrentBB);

        GlobAddrReg = ParentFunction->GetNextAvailableVirtualRegister();
        GlobalAddress.AddVirtualRegister(GlobAddrReg, TM->GetPointerSize());
        GlobalAddress.AddGlobalSymbol(
            ((GlobalVariable *)I->GetMemoryLocation())->GetName());

        CurrentBB->InsertInstr(GlobalAddress);
        AddressReg = GlobAddrReg;
    }
    else
    {
        AddressReg = GetIDFromValue(I->GetMemoryLocation());
    }

    ResultMI.AddAttribute(MachineInstruction::IsSTORE);

    if (ParentFunction->IsStackSlot(AddressReg))
        ResultMI.AddStackAccess(AddressReg);
    else
        ResultMI.AddMemory(AddressReg, TM->GetPointerSize());

    // if Source is a struct an not struct pointer
    if (I->GetSavedValue()->GetTypeRef().IsStruct() &&
        !I->GetSavedValue()->GetTypeRef().IsPointer())
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
                CurrentStore = MachineInstruction(MachineInstruction::Store, CurrentBB);

                CurrentStore.AddStackAccess(AddressReg, Counter * RegSize / 8);
                CurrentStore.AddVirtualRegister(ParamId, RegSize);

                Counter++;

                // insert all the stores but the last one, that will be the return
                // value
                if (Counter < StructToRegMap[StructName].size())
                    CurrentBB->InsertInstr(CurrentStore);
            }

            return CurrentStore;
        }
        // Handle other cases, like when the structure is a return value from a
        // function
        else
        {
            // clang-format off
           const unsigned StructBitSize = (I->GetSavedValue()->GetTypeRef().GetBaseTypeByteSize() * 8);
           const unsigned MaxRegSize    = TM->GetPointerSize();
           const unsigned RegsCount     = GetNextAlignedValue(StructBitSize, MaxRegSize) / MaxRegSize;
            // clang-format on

            auto &RetRegs = TM->GetABI()->GetReturnRegisters();

            assert(RegsCount <= RetRegs.size());

            MachineInstruction Store;
            for (std::size_t i = 0; i < RegsCount; i++)
            {
                Store = MachineInstruction(MachineInstruction::Store, CurrentBB);

                Store.AddStackAccess(AddressReg, (TM->GetPointerSize() / 8) * i);
                Store.AddRegister(RetRegs[i]->GetID(), TM->GetPointerSize());

                if (i == (RegsCount - 1))
                    return Store;

                CurrentBB->InsertInstr(Store);
            }
        }
    }
    else if (!ParamByIDToRegMap[I->GetSavedValue()->GetID()].empty())
    {
        assert(dynamic_cast<FunctionParameter *>(I->GetSavedValue()));
        const unsigned RegSize = TM->GetPointerSize();

        MachineInstruction CurrentStore;
        unsigned Counter = 0;

        // Create stores for the register which holds the struct parts
        for (auto ParamID : ParamByIDToRegMap[I->GetSavedValue()->GetID()])
        {
            CurrentStore = MachineInstruction(MachineInstruction::Store, CurrentBB);
            CurrentStore.AddStackAccess(AddressReg, Counter * RegSize / 8);
            CurrentStore.AddVirtualRegister(ParamID, RegSize);
            Counter++;
            // insert all the stores but the last one, that will be the return
            // value
            if (Counter < ParamByIDToRegMap[I->GetSavedValue()->GetID()].size())
                CurrentBB->InsertInstr(CurrentStore);
        }

        return CurrentStore;
    }
    else if (I->GetSavedValue()->IsGlobalVar())
    {
        auto GlobalAddress =
            MachineInstruction(MachineInstruction::GlobalAddress, CurrentBB);
        auto SourceReg = ParentFunction->GetNextAvailableVirtualRegister();

        GlobalAddress.AddVirtualRegister(SourceReg, TM->GetPointerSize());
        GlobalAddress.AddGlobalSymbol(
            static_cast<GlobalVariable *>(I->GetSavedValue())->GetName());

        CurrentBB->InsertInstr(GlobalAddress);

        ResultMI.AddVirtualRegister(SourceReg, TM->GetPointerSize());
    }
    // if the source is a stackallocation instruction, then its address which needs to be
    // stored, therefore it has to be materialized by StackAccess instruction
    else if (dynamic_cast<StackAllocationInstruction *>(I->GetSavedValue()))
    {
        assert(ParentFunction->IsStackSlot(I->GetSavedValue()->GetID()));

        auto SA        = MachineInstruction(MachineInstruction::StackAddress, CurrentBB);
        auto SourceReg = ParentFunction->GetNextAvailableVirtualRegister();

        SA.AddVirtualRegister(SourceReg, TM->GetPointerSize());
        SA.AddStackAccess(I->GetSavedValue()->GetID());
        CurrentBB->InsertInstr(SA);

        ResultMI.AddVirtualRegister(SourceReg, TM->GetPointerSize());
    }
    else
    {
        ResultMI.AddOperand(GetMachineOperandFromValue(I->GetSavedValue()));
    }

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleLoadInstruction(LoadInstruction *I)
{
    assert(I->GetMemoryLocation()->IsRegister() ||
           I->GetMemoryLocation()->IsGlobalVar() && "Forbidden destination");

    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    unsigned GlobAddrReg;
    unsigned AddressReg;
    if (I->GetMemoryLocation()->IsGlobalVar())
    {
        auto GlobalAddress =
            MachineInstruction(MachineInstruction::GlobalAddress, CurrentBB);

        GlobAddrReg = ParentFunction->GetNextAvailableVirtualRegister();
        GlobalAddress.AddVirtualRegister(GlobAddrReg, TM->GetPointerSize());
        GlobalAddress.AddGlobalSymbol(
            ((GlobalVariable *)I->GetMemoryLocation())->GetName());

        CurrentBB->InsertInstr(GlobalAddress);
        AddressReg = GlobAddrReg;
    }
    else
    {
        AddressReg = GetIDFromValue(I->GetMemoryLocation());
    }

    ResultMI.AddAttribute(MachineInstruction::IsLOAD);
    ResultMI.AddOperand(GetMachineOperandFromValue((Value *)I, true));

    if (ParentFunction->IsStackSlot(AddressReg))
        ResultMI.AddStackAccess(AddressReg);
    else
        ResultMI.AddMemory(AddressReg, TM->GetPointerSize());

    // if the destination is a struct and not a struct pointer
    if (I->GetTypeRef().IsStruct() && !I->GetTypeRef().IsPointer())
    {
        unsigned StructBitSize = (I->GetTypeRef().GetByteSize() * 8);
        unsigned RegSize       = TM->GetPointerSize();
        unsigned RegsCount     = GetNextAlignedValue(StructBitSize, RegSize) / RegSize;

        // Create loads for the registers which holds the struct parts
        for (size_t i = 0; i < RegsCount; i++)
        {
            auto CurrentLoad = MachineInstruction(MachineInstruction::Load, CurrentBB);
            auto NewVReg     = ParentFunction->GetNextAvailableVirtualRegister();

            CurrentLoad.AddVirtualRegister(NewVReg, RegSize);
            StructByIDToRegMap[I->GetID()].push_back(NewVReg);
            CurrentLoad.AddStackAccess(AddressReg, i * RegSize / 8);

            // insert all the stores but the last one, that will be the return value
            if (i + 1 < RegsCount)
                CurrentBB->InsertInstr(CurrentLoad);
            else
                return CurrentLoad;
        }
    }

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleCallInstruction(CallInstruction *I)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

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

        // In case if its a struct by value param, then it is already loaded
        // in into registers, so issue move instructions to move these into
        // the parameter registers
        if (Param->GetTypeRef().IsStruct() && !Param->GetTypeRef().IsPointer() &&
            !Param->IsGlobalVar())
        {
            assert(StructByIDToRegMap.count(Param->GetID()) > 0 &&
                   "The map does not know about this struct param");
            // how many register are used to pass this struct

            for (auto VReg : StructByIDToRegMap[Param->GetID()])
            {
                Ins = MachineInstruction(MachineInstruction::Mov, CurrentBB);

                Ins.AddRegister(TargetArgRegs[ParamCounter]->GetID(),
                                TargetArgRegs[ParamCounter]->GetBitWidth());
                Ins.AddVirtualRegister(VReg, TM->GetPointerSize());

                CurrentBB->InsertInstr(Ins);
                ParamCounter++;
            }
        }
        // Handle pointer case for both local and global objects
        else if (Param->GetTypeRef().IsPointer() &&
                 (Param->IsGlobalVar() || ParentFunction->IsStackSlot(Param->GetID())))
        {
            unsigned DestinationReg;
            unsigned RegBitWidth = TargetArgRegs[ParamCounter]->GetBitWidth();

            if ((int)ParamCounter == I->GetImplicitStructArgIndex())
            {
                DestinationReg = TM->GetRegInfo()->GetStructPtrRegister();
            }
            else
            {
                DestinationReg = TargetArgRegs[ParamCounter]->GetID();
            }

            if (Param->IsGlobalVar())
            {
                Ins = MachineInstruction(MachineInstruction::GlobalAddress, CurrentBB);

                auto Symbol = ((GlobalVariable *)Param)->GetName();
                Ins.AddRegister(DestinationReg, RegBitWidth);
                Ins.AddGlobalSymbol(Symbol);

                CurrentBB->InsertInstr(Ins);
                ParamCounter++;
            }
            else
            {
                Ins = MachineInstruction(MachineInstruction::StackAddress, CurrentBB);

                Ins.AddRegister(DestinationReg, RegBitWidth);
                Ins.AddStackAccess(Param->GetID());

                CurrentBB->InsertInstr(Ins);
                ParamCounter++;
            }
        }
        // default case is to just move into the right parameter register
        else
        {
            Ins = MachineInstruction(MachineInstruction::Mov, CurrentBB);

            unsigned ParamIdx = ParamCounter;

            if (Param->IsFPType())
            {
                Ins.SetOpcode(MachineInstruction::MovF);
                ParamIdx += TM->GetABI()->GetFirstFPRetRegIdx();
            }

            auto Src              = GetMachineOperandFromValue(Param);
            auto ParamPhysReg     = TargetArgRegs[ParamIdx]->GetID();
            auto ParamPhysRegSize = TargetArgRegs[ParamIdx]->GetBitWidth();

            if (Src.GetSize() < ParamPhysRegSize &&
                !TargetArgRegs[ParamIdx]->GetSubRegs().empty())
            {
                ParamPhysReg = TargetArgRegs[ParamIdx]->GetSubRegs()[0];
                ParamPhysRegSize =
                    TM->GetRegInfo()->GetRegisterByID(ParamPhysReg)->GetBitWidth();
            }

            Ins.AddRegister(ParamPhysReg, ParamPhysRegSize);
            Ins.AddOperand(Src);

            CurrentBB->InsertInstr(Ins);
            ParamCounter++;
        }
    }
    ResultMI.AddFunctionName(I->GetName().c_str());

    // if no return value then we are done.
    if (I->GetTypeRef().IsVoid())
        return ResultMI;

    /// Handle the case when there are returned values and spill them to the stack
    CurrentBB->InsertInstr(ResultMI);

    unsigned RetBitSize       = I->GetTypeRef().GetByteSize() * 8;
    const unsigned MaxRegSize = TM->GetPointerSize();
    const unsigned RegsCount  = GetNextAlignedValue(RetBitSize, MaxRegSize) / MaxRegSize;

    assert(RegsCount > 0 && RegsCount <= 2);
    auto &RetRegs  = TM->GetABI()->GetReturnRegisters();
    auto StackSlot = ParentFunction->GetNextAvailableVirtualRegister();

    SpilledReturnValuesIDToStackID[I->GetID()] = StackSlot;
    ParentFunction->InsertStackSlot(StackSlot, RetBitSize / 8, RetBitSize / 8);
    for (size_t i = 0; i < RegsCount; i++)
    {
        // Note: actual its not a vreg, but this make sure it will be a unique ID
        auto Store = MachineInstruction(MachineInstruction::Store, CurrentBB);
        auto StackSlotMO =
            MachineOperand::CreateStackAccess(StackSlot, i * (MaxRegSize / 8));
        Store.AddOperand(StackSlotMO);

        // find the appropriate return register for the size
        unsigned TargetRetReg;

        std::size_t ParamIdx = i;
        if (I->GetTypeRef().IsFP())
            ParamIdx += TM->GetABI()->GetFirstFPRetRegIdx();

        // if the return value can use the return register
        if (std::min(RetBitSize, MaxRegSize) >= TM->GetPointerSize())
            TargetRetReg = RetRegs[ParamIdx]->GetID();
        // need to find an appropriate sized subregister of the actual return reg
        else
            // FIXME: Temporary solution, only work for AArch64
            TargetRetReg = RetRegs[ParamIdx]->GetSubRegs()[0];

        Store.AddRegister(TargetRetReg, std::min(RetBitSize, MaxRegSize));

        if (i + 1 == RegsCount)
            return Store;

        CurrentBB->InsertInstr(Store);
        RetBitSize -= MaxRegSize;
    }

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleBranchInstruction(BranchInstruction *I,
                                                     std::vector<MachineBasicBlock> &BBS)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    const char *TrueLabel  = nullptr;
    const char *FalseLabel = nullptr;

    for (auto &bb : BBS)
    {
        if (TrueLabel == nullptr && I->GetTrueLabelName() == bb.GetName())
            TrueLabel = bb.GetName().c_str();

        if (FalseLabel == nullptr && I->HasFalseLabel() &&
            I->GetFalseLabelName() == bb.GetName())
            FalseLabel = bb.GetName().c_str();
    }

    ResultMI.AddOperand(GetMachineOperandFromValue(I->GetCondition()));
    ResultMI.AddLabel(TrueLabel);

    if (I->HasFalseLabel())
        ResultMI.AddLabel(TrueLabel);    // Fixme: There should be false-lable?

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleGetElemPtrInstruction(GetElemPointerInstruction *I)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    MachineInstruction GoalInst;

    // Used for to look up GoalInstr if it was inserted
    int GoalInstrIdx = -1;

    auto SourceID       = GetIDFromValue(I->GetSource());
    const bool IsGlobal = I->GetSource()->IsGlobalVar();
    const bool IsStack  = ParentFunction->IsStackSlot(SourceID);
    const bool IsReg    = !IsGlobal && !IsStack;

    if (IsGlobal)
        GoalInst = MachineInstruction(MachineInstruction::GlobalAddress, CurrentBB);
    else if (IsStack)
        GoalInst = MachineInstruction(MachineInstruction::StackAddress, CurrentBB);

    auto Dest = GetMachineOperandFromValue((Value *)I, true);
    GoalInst.AddOperand(Dest);

    if (IsGlobal)
        GoalInst.AddGlobalSymbol(((GlobalVariable *)I->GetSource())->GetName());
    else if (IsStack)
        GoalInst.AddStackAccess(SourceID);


    unsigned ConstantIndexPart = 0;
    bool IndexIsInReg          = false;
    unsigned MULResVReg        = 0;

    auto &SourceType = I->GetSource()->GetTypeRef();
    auto IndexReg    = GetMachineOperandFromValue(I->GetIndex());

    if (I->GetIndex()->IsConstant())
    {
        auto Index = ((Constant *)I->GetIndex())->GetIntValue();
        if (!SourceType.IsStruct())
            ConstantIndexPart = (SourceType.CalcElemSize(0) * Index);
        else    // its a struct and has to determine the offset other way
            ConstantIndexPart = SourceType.GetElemByteOffset(Index);

        // If there is nothing to add, then exit now
        if (ConstantIndexPart == 0 && !GoalInst.IsInvalid())
            return GoalInst;

        // rather then issue an addition, it more effective to set the
        // StackAccess operand's offset to the index value
        if (IsStack)
        {
            GoalInst.GetOperands()[1].SetOffset(ConstantIndexPart);
            return GoalInst;
        }
    }
    else
    {
        IndexIsInReg = true;
        if (!SourceType.IsStruct() || (SourceType.GetPointerLevel() > 2))
        {
            if (!GoalInst.IsInvalid())
            {
                CurrentBB->InsertInstr(GoalInst);
                GoalInstrIdx = CurrentBB->GetInstructions().size() - 1;
            }

            auto Multiplier = SourceType.CalcElemSize(0);

            // edge case, identity: x * 1 = x
            // in this case only do a MOV or SEXT rather then MUL
            if (Multiplier == 1)
            {
                MULResVReg = ParentFunction->GetNextAvailableVirtualRegister();
                auto MOV   = MachineInstruction(MachineInstruction::Mov, CurrentBB);
                MOV.AddVirtualRegister(MULResVReg, TM->GetPointerSize());
                MOV.AddOperand(IndexReg);

                // if sign extension needed, then swap the mov to that
                if (IndexReg.GetSize() < TM->GetPointerSize())
                    MOV.SetOpcode(MachineInstruction::SExt);
                CurrentBB->InsertInstr(MOV);
            }
            // general case
            // MOV the multiplier into a register
            // TODO: this should not needed, only done because AArch64 does not
            // support immediate operands for MUL, this should be handled by the
            // target legalizer.
            else
            {
                auto ImmediateVReg = ParentFunction->GetNextAvailableVirtualRegister();
                auto MOV = MachineInstruction(MachineInstruction::Mov, CurrentBB);
                MOV.AddVirtualRegister(ImmediateVReg, TM->GetPointerSize());
                MOV.AddImmediate(Multiplier);
                CurrentBB->InsertInstr(MOV);

                // if sign extension needed, then insert a sign extending first
                MachineInstruction SEXT;
                unsigned SEXTResVReg = 0;
                if (IndexReg.GetSize() < TM->GetPointerSize())
                {
                    SEXTResVReg = ParentFunction->GetNextAvailableVirtualRegister();
                    SEXT        = MachineInstruction(MachineInstruction::SExt, CurrentBB);
                    SEXT.AddVirtualRegister(SEXTResVReg, TM->GetPointerSize());
                    SEXT.AddOperand(IndexReg);
                    CurrentBB->InsertInstr(SEXT);
                }

                MULResVReg = ParentFunction->GetNextAvailableVirtualRegister();
                auto MUL   = MachineInstruction(MachineInstruction::Mul, CurrentBB);
                MUL.AddVirtualRegister(MULResVReg, TM->GetPointerSize());
                // if sign extension did not happened, then jus use the IndexReg
                if (SEXT.IsInvalid())
                    MUL.AddOperand(IndexReg);
                else    // otherwise the result register of the SEXT operaton
                    MUL.AddVirtualRegister(SEXTResVReg, TM->GetPointerSize());
                MUL.AddVirtualRegister(ImmediateVReg, TM->GetPointerSize());
                CurrentBB->InsertInstr(MUL);
            }
        }
        else    // its a struct and has to determine the offset other way
            assert(!"TODO");
    }

    // Since the result of gep will be Add instruction's destination operand,
    // therefore the GoalInstr definition must be renamed to make it unique.
    // This only requires if the GoalInstr is a stack or a global address instruction.

    if (IsGlobal)
    {
        if (!IndexIsInReg)
            GoalInst.GetDefine()->SetReg(
                ParentFunction->GetNextAvailableVirtualRegister());
        else
            CurrentBB->GetInstructions()[GoalInstrIdx].GetDefine()->SetReg(
                ParentFunction->GetNextAvailableVirtualRegister());
    }

    if (!GoalInst.IsInvalid() && !IndexIsInReg)
        CurrentBB->InsertInstr(GoalInst);

    auto ADD = MachineInstruction(MachineInstruction::Add, CurrentBB);
    ADD.AddOperand(Dest);

    if (IsReg)
        ADD.AddOperand(GetMachineOperandFromValue(I->GetSource()));
    else
        // Otherwise (stack or global case) the base address is loaded in Dest by
        // the preceding STACK_ADDRESS or GLOBAL_ADDRESS instruction
        // the Def of the GoalInstruction to use the updated destination.
        ADD.AddOperand(GoalInstrIdx != -1 ?
                           *CurrentBB->GetInstructions()[GoalInstrIdx].GetDefine() :
                           *GoalInst.GetDefine());

    if (IndexIsInReg)
        ADD.AddVirtualRegister(MULResVReg, TM->GetPointerSize());
    else
        ADD.AddImmediate(ConstantIndexPart, Dest.GetSize());

    return ADD;
}

MachineInstruction IRtoLLIR::HandleJumpInstruction(JumpInstruction *I,
                                                   std::vector<MachineBasicBlock> &BBS)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    for (auto &bb : BBS)
    {
        if (I->GetTargetLabelName() == bb.GetName())
        {
            ResultMI.AddLabel(bb.GetName().c_str());
            break;
        }
    }

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleCompareInstruction(CompareInstruction *I)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    auto Result      = GetMachineOperandFromValue((Value *)I);
    auto FirstSrcOp  = GetMachineOperandFromValue(I->GetLHS());
    auto SecondSrcOp = GetMachineOperandFromValue(I->GetRHS());

    ResultMI.AddOperand(Result);
    ResultMI.AddOperand(FirstSrcOp);
    ResultMI.AddOperand(SecondSrcOp);

    ResultMI.SetAttributes(I->GetRelation());

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleReturnInstruction(ReturnInstruction *I)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    if (I->GetRetVal() == nullptr)
        return ResultMI;

    const bool IsFP = I->GetRetVal()->GetTypeRef().IsFP();

    // insert moves to move in the return val to the return registers
    // TODO: Maybe make a backward search in the BB for the load instructions
    // which defines these IDs and change them to the right physical registers
    // this way the moves does not needed. Although if good enough copy
    // propagation were implemented, then this would be handled by it
    auto &TargetRetRegs = TM->GetABI()->GetReturnRegisters();
    if (I->GetRetVal()->GetTypeRef().IsStruct() &&
        !I->GetRetVal()->GetTypeRef().IsPointer())
    {
        assert(StructByIDToRegMap[I->GetRetVal()->GetID()].size() <= 2);

        std::size_t RetRegCounter = 0;
        for (auto VReg : StructByIDToRegMap[I->GetRetVal()->GetID()])
        {
            auto Instr = MachineInstruction(MachineInstruction::Mov, CurrentBB);

            Instr.AddRegister(TargetRetRegs[RetRegCounter]->GetID(),
                              TargetRetRegs[RetRegCounter]->GetBitWidth());
            Instr.AddVirtualRegister(VReg, TM->GetPointerSize());

            CurrentBB->InsertInstr(Instr);
            RetRegCounter++;
        }
    }
    else if (I->GetRetVal()->IsConstant())
    {
        auto &RetRegs = TM->GetABI()->GetReturnRegisters();

        if (I->GetRetVal()->GetTypeRef().GetBitSize() <= TM->GetPointerSize())
        {
            MachineInstruction LoadImm;

            if (IsFP)
                LoadImm = MachineInstruction(MachineInstruction::MovF, CurrentBB);
            else
                LoadImm = MachineInstruction(MachineInstruction::LoadImm, CurrentBB);

            // TODO: make it target independent by searching for the right sized register,
            // do it like register allocator.
            unsigned RetRegIdx = IsFP ? TM->GetABI()->GetFirstFPRetRegIdx() : 0;
            if (RetRegs[0]->GetBitWidth() == I->GetBitWidth())
            {
                LoadImm.AddRegister(RetRegs[RetRegIdx]->GetID(),
                                    RetRegs[RetRegIdx]->GetBitWidth());
            }
            else if (I->GetRetVal()->GetTypeRef().GetBitSize() <= TM->GetPointerSize())
            {
                if (!RetRegs[RetRegIdx]->GetSubRegs().empty())
                {
                    LoadImm.AddRegister(
                        RetRegs[RetRegIdx]->GetSubRegs()[0],
                        TM->GetRegInfo()
                            ->GetRegisterByID(RetRegs[RetRegIdx]->GetSubRegs()[0])
                            ->GetBitWidth());
                }
                else if (dynamic_cast<RISCV::RISCVTargetMachine *>(TM))
                {
                    LoadImm.AddRegister(RetRegs[RetRegIdx]->GetID(),
                                        RetRegs[RetRegIdx]->GetBitWidth());
                }
            }
            else
            {
                assert(!"Cannot find return register candidate");
            }

            LoadImm.AddOperand(GetMachineOperandFromValue(I->GetRetVal()));

            // change ret operand to the destination register of the LOAD_IMM
            ResultMI.AddOperand(*LoadImm.GetOperand(0));

            CurrentBB->InsertInstr(LoadImm);
        }
        else
        {
            // If the target cannot return the immediate in one register then if
            // the target allows return it in multiple registers
            // TODO: actually it is not really checked if the target allows it or
            //  not or how many register are there for this reason, it is assumed
            //  here a riscv32 like case -> 2 32 bit register for returning the value

            const unsigned RetBitSize = I->GetTypeRef().GetByteSize() * 8;
            const unsigned MaxRegSize = TM->GetPointerSize();
            const unsigned RegsCount =
                GetNextAlignedValue(RetBitSize, MaxRegSize) / MaxRegSize;

            assert(RegsCount == 2 && "Only supporting two return registers for now");
            assert(!IsFP && "FP values cannot be divided into multiple registers");

            auto Const = dynamic_cast<Constant *>(I->GetRetVal());
            auto ImmMO = GetMachineOperandFromValue(I->GetRetVal(), CurrentBB);

            for (size_t i = 0; i < RegsCount; i++)
            {
                auto LI = MachineInstruction(MachineInstruction::LoadImm, CurrentBB);

                LI.AddRegister(TargetRetRegs[i]->GetID(),
                               TargetRetRegs[i]->GetBitWidth());

                LI.AddImmediate((Const->GetIntValue() >> (i * 32)) & 0xffffffff);

                CurrentBB->InsertInstr(LI);
            }
        }
    }
    // If the return value must be put into multiple registers like s64 for
    // RISCV32
    else if (I->GetRetVal()->GetTypeRef().GetBitSize() > TM->GetPointerSize())
    {
        assert(I->GetRetVal()->GetTypeRef().GetBitSize() <= 64 &&
               "TODO: for now expecting only max 64 bit types");

        // First SPLIT the value into two 32 bit register
        auto Split = MachineInstruction(MachineInstruction::Split, CurrentBB);

        auto Lo32 = MachineOperand::CreateVirtualRegister(
            ParentFunction->GetNextAvailableVirtualRegister());
        auto Hi32 = MachineOperand::CreateVirtualRegister(
            ParentFunction->GetNextAvailableVirtualRegister());

        std::vector<MachineOperand> SplittedVRegs = {Lo32, Hi32};

        Split.AddOperand(Lo32);
        Split.AddOperand(Hi32);
        Split.AddOperand(GetMachineOperandFromValue(I->GetRetVal(), CurrentBB));
        CurrentBB->InsertInstr(Split);

        // Move the splitted registers into the physical return registers
        const unsigned RetBitSize = I->GetTypeRef().GetByteSize() * 8;
        const unsigned MaxRegSize = TM->GetPointerSize();
        const unsigned RegsCount =
            GetNextAlignedValue(RetBitSize, MaxRegSize) / MaxRegSize;

        assert(RegsCount == 2 && "Only supporting two return registers for now");

        for (size_t i = 0; i < RegsCount; i++)
        {
            auto MOV = MachineInstruction(MachineInstruction::Mov, CurrentBB);

            MOV.AddRegister(TargetRetRegs[i]->GetID(), TargetRetRegs[i]->GetBitWidth());

            MOV.AddOperand(SplittedVRegs[i]);
            CurrentBB->InsertInstr(MOV);
        }
    }
    else
    {
        auto Result = GetMachineOperandFromValue(I->GetRetVal(), CurrentBB);
        ResultMI.AddOperand(Result);
    }

    return ResultMI;
}

MachineInstruction IRtoLLIR::HandleMemoryCopyInstruction(MemoryCopyInstruction *I)
{
    auto Operation = I->GetInstructionKind();
    auto ResultMI  = MachineInstruction((unsigned)Operation + (1 << 16), CurrentBB);

    auto SrcID  = GetIDFromValue(I->GetSource());
    auto DestID = GetIDFromValue(I->GetDestination());

    // lower this into load and store pairs if used with structs lower then
    // a certain size (for now be it the size which can be passed by value)
    // otherwise create a call maybe to an intrinsic memcopy function
    for (size_t i = 0; i < (I->GetSize() / /* TODO: use alignment here */ 4); i++)
    {
        auto Load    = MachineInstruction(MachineInstruction::Load, CurrentBB);
        auto NewVReg = ParentFunction->GetNextAvailableVirtualRegister();

        Load.AddVirtualRegister(NewVReg, /* TODO: use alignment here */ 32);

        if (ParentFunction->IsStackSlot(SrcID))
            Load.AddStackAccess(SrcID, i * /* TODO: use alignment here */ 4);
        else
            Load.AddMemory(SrcID, i * 4, TM->GetPointerSize());

        CurrentBB->InsertInstr(Load);

        auto Store = MachineInstruction(MachineInstruction::Store, CurrentBB);

        if (ParentFunction->IsStackSlot(I->GetDestination()->GetID()))
        {
            Store.AddStackAccess(DestID, i * 4);    /// TODO: use alignment here
        }
        else
        {
            Store.AddMemory(DestID, TM->GetPointerSize());
            Store.GetOperands()[0].SetOffset(i * 4);
        }

        Store.AddVirtualRegister(NewVReg, /* TODO: use alignment here */ 32);

        // TODO: Change the function so it does not return the instruction but
        // insert it in the function so don't have to do these annoying returns
        if (i == ((I->GetSize() / /* TODO: use alignment here */ 4) - 1))
            return Store;

        CurrentBB->InsertInstr(Store);
    }

    return ResultMI;
}

MachineInstruction IRtoLLIR::ConvertToMachineInstr(Instruction *Instr,
                                                   std::vector<MachineBasicBlock> &BBS)
{
    // Three Address Instructions: Instr Result, Op1, Op2
    if (auto I = dynamic_cast<BinaryInstruction *>(Instr); I != nullptr)
    {
        return HandleBinaryInstruction(I);
    }
    // Two address ALU instructions: Instr Result, Op
    else if (auto I = dynamic_cast<UnaryInstruction *>(Instr); I != nullptr)
    {
        return HandleUnaryInstruction(I);
    }
    // Store Instruction: Store [Address], Src
    else if (auto I = dynamic_cast<StoreInstruction *>(Instr); I != nullptr)
    {
        return HandleStoreInstruction(I);
    }
    // Load Instruction: Load Dest, [Address]
    else if (auto I = dynamic_cast<LoadInstruction *>(Instr); I != nullptr)
    {
        return HandleLoadInstruction(I);
    }
    // GEP instruction: GEP Dest, Source, list of indexes
    // to
    //   STACK_ADDRESS Dest, Source
    // **arithmetic instructions to calculate the index** ex: 1 index which is 6
    //   MUL idx, sizeof(Source[0]), 6
    //   ADD Dest, Dest, idx
    else if (auto I = dynamic_cast<GetElemPointerInstruction *>(Instr); I != nullptr)
    {
        return HandleGetElemPtrInstruction(I);
    }
    // Call Instruction: call Result functionName(param1, ...)
    else if (auto I = dynamic_cast<CallInstruction *>(Instr); I != nullptr)
    {
        return HandleCallInstruction(I);
    }
    // Jump Instruction: Jump Label
    else if (auto I = dynamic_cast<JumpInstruction *>(Instr); I != nullptr)
    {
        return HandleJumpInstruction(I, BBS);
    }
    // Branch Instruction : Br op label1 label2
    else if (auto I = dynamic_cast<BranchInstruction *>(Instr); I != nullptr)
    {
        return HandleBranchInstruction(I, BBS);
    }
    // Compare Instruction: cmp relation dest br1, br2
    else if (auto I = dynamic_cast<CompareInstruction *>(Instr); I != nullptr)
    {
        return HandleCompareInstruction(I);
    }
    // Ret Instruction: ret op
    else if (auto I = dynamic_cast<ReturnInstruction *>(Instr); I != nullptr)
    {
        return HandleReturnInstruction(I);
    }
    else if (auto I = dynamic_cast<MemoryCopyInstruction *>(Instr); I != nullptr)
    {
        return HandleMemoryCopyInstruction(I);
    }
    else
    {
        assert(!"Unimplemented Instruction.");
    }

    return {};
}

void HandleStackAllocation(StackAllocationInstruction *Instr,
                           MachineFunction *Func,
                           TargetMachine *TM)
{
    auto ReferedType = Instr->GetType();
    assert(ReferedType.GetPointerLevel() > 0);
    ReferedType.DecrementPointerLevel();
    const auto IsPTR    = ReferedType.GetPointerLevel() > 0;
    const auto IsStruct = ReferedType.IsStruct();

    const size_t Alignment = IsPTR    ? TM->GetPointerSize() / 8 :
                             IsStruct ? ReferedType.GetStructMaxAlignment(TM) :
                                        ReferedType.GetBaseTypeByteSize();

    const size_t Size = IsPTR ? TM->GetPointerSize() / 8 : ReferedType.GetByteSize();

    Func->InsertStackSlot(Instr->GetID(), Size, Alignment);
}

void IRtoLLIR::HandleFunctionParams(Function &F, MachineFunction *Func)
{
    for (auto &Param : F.GetParameters())
    {
        auto ParamID     = Param->GetID();
        auto ParamSize   = Param->GetBitWidth();
        auto IsStructPtr = Param->IsImplicitStructPtr();

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

            unsigned MaxStructSize = TM->GetABI()->GetMaxStructSizePassedByValue();
            for (size_t i = 0; i < MaxStructSize / TM->GetPointerSize(); i++)
            {
                auto NextVReg = Func->GetNextAvailableVirtualRegister();
                StructToRegMap[StructName].push_back(NextVReg);
                Func->InsertParameter(NextVReg,
                                      LowLevelType::CreateScalar(TM->GetPointerSize()));
            }

            continue;
        }

        if (Param->GetTypeRef().IsPointer())
        {
            Func->InsertParameter(ParamID,
                                  LowLevelType::CreatePtr(TM->GetPointerSize()),
                                  IsStructPtr);
        }
        // TODO: quick "hack" to use ptr size
        else if (ParamSize <= TM->GetPointerSize())
        {
            Func->InsertParameter(ParamID,
                                  LowLevelType::CreateScalar(ParamSize),
                                  IsStructPtr,
                                  Param->GetTypeRef().IsFP());
        }
        else
        {
            // if the parameter does not fit into the parameter registers then it is
            // passed
            // in multiple registers like 64 bit integers in RISCV32 passed in 2 registers
            for (size_t i = 0; i < ParamSize / TM->GetPointerSize(); i++)
            {
                auto NextVReg = Func->GetNextAvailableVirtualRegister();
                ParamByIDToRegMap[ParamID].push_back(NextVReg);
                Func->InsertParameter(NextVReg,
                                      LowLevelType::CreateScalar(TM->GetPointerSize()),
                                      IsStructPtr,
                                      Param->GetTypeRef().IsFP());
            }
        }
    }
}

void IRtoLLIR::GenerateLLIRFromIR()
{
    // reserving enough size for the functions otherwise the underlying vector
    // would reallocate it self and would made invalid the existing pointers
    // pointing to these functions
    // FIXME: Would be nice to auto update the pointers somehow if necessary.
    // Like LLVM does it, but that might be too complicated for the scope of this
    // project.

    TU->GetFunctions().reserve(IRM.GetFunctions().size());

    for (auto &Func : IRM.GetFunctions())
    {
        Reset();

        if (Func.IsDeclarationOnly())
            continue;

        this->TU->AddNewFunction();

        MachineFunction *MFunction = TU->GetCurrentFunction();
        assert(MFunction);

        MFunction->SetName(Func.GetName());
        HandleFunctionParams(Func, MFunction);

        // Create all basic block first with their name , so jumps can
        // refer to them already
        auto &MFuncMBBs = MFunction->GetBasicBlocks();
        for (auto &BB : Func.GetBasicBlocks())
            MFuncMBBs.push_back(MachineBasicBlock {BB.get()->GetName(), MFunction});

        unsigned BBCounter = 0;
        for (auto &BB : Func.GetBasicBlocks())
        {
            for (auto &Instr : BB->GetInstructions())
            {
                auto InstrPtr = Instr.get();

                if (InstrPtr->IsStackAllocation())
                {
                    HandleStackAllocation((StackAllocationInstruction *)InstrPtr,
                                          MFunction,
                                          TM);
                    continue;
                }

                CurrentBB      = &MFuncMBBs[BBCounter];
                ParentFunction = CurrentBB->GetParent();
                CurrentBB->InsertInstr(ConvertToMachineInstr(InstrPtr, MFuncMBBs));

                // everything after a return is dead code so skip those
                // TODO: add unconditional branch aswell, but it would be better to
                // just not handle this here but in some optimization pass for
                // example in dead code elimination
                if (CurrentBB->GetInstructions().back().IsReturn())
                    break;
            }

            BBCounter++;
        }
    }

    for (auto &GlobalVar : IRM.GetGlobalVars())
    {
        auto Name = ((GlobalVariable *)GlobalVar.get())->GetName();
        auto Size = GlobalVar->GetTypeRef().GetByteSize();

        auto GD        = GlobalData(Name, Size);
        auto &InitList = ((GlobalVariable *)GlobalVar.get())->GetInitList();

        if (GlobalVar->GetTypeRef().IsStruct() || GlobalVar->GetTypeRef().IsArray())
        {
            // If the init list is empty, then just allocate Size amount of zeros
            if (InitList.empty())
            {
                auto InitStr =
                    static_cast<GlobalVariable *>(GlobalVar.get())->GetInitString();
                auto InitVal =
                    static_cast<GlobalVariable *>(GlobalVar.get())->GetInitValue();

                if (InitStr.empty() && InitVal == nullptr)
                    GD.InsertAllocation(Size, 0);
                else
                    // string literal case
                    GD.InsertAllocation(
                        InitVal ? static_cast<GlobalVariable *>(InitVal)->GetName() :
                                  InitStr);
            }
            // if the list is not empty then allocate the appropriate type of memories
            // with initialization
            else
            {
                // struct case
                if (GlobalVar->GetTypeRef().IsStruct())
                {
                    size_t InitListIndex = 0;
                    for (auto &MemberType : GlobalVar->GetTypeRef().GetMemberTypes())
                    {
                        assert(InitListIndex < InitList.size());
                        GD.InsertAllocation(MemberType.GetByteSize(),
                                            InitList[InitListIndex]);
                        InitListIndex++;
                    }
                }
                // array case
                else
                {
                    const auto Size = GlobalVar->GetTypeRef().GetBaseType().GetByteSize();
                    for (auto InitVal : InitList)
                        GD.InsertAllocation(Size, InitVal);
                }
            }
        }
        // scalar case
        else if (InitList.empty())
        {
            auto InitVal = static_cast<GlobalVariable *>(GlobalVar.get())->GetInitValue();

            // zero initialized scalar
            if (InitVal == nullptr)
                GD.InsertAllocation(Size, 0);
            else
            {
                // initialized by another global variable (ptr), typical use case: string
                // literals
                GlobalData::Directives d;

                switch (TM->GetPointerSize())
                {
                    case 32: d = GlobalData::Word; break;
                    case 64: d = GlobalData::DoubleWord; break;

                    default: assert(!"Unhandled pointer size"); break;
                }

                GD.InsertAllocation(static_cast<GlobalVariable *>(InitVal)->GetName(), d);
            }
        }
        else
        {
            GD.InsertAllocation(Size, InitList[0]);
        }

        TU->AddGlobalData(GD);
    }
}
