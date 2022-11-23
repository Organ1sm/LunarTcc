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
            auto Instr = MachineInstruction(MachineInstruction::Load,
                                            &ParentFunction->GetBasicBlocks().back());

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
                auto Instr = MachineInstruction(MachineInstruction::Load,
                                                &ParentFunction->GetBasicBlocks().back());
                NextVReg   = ParentFunction->GetNextAvailableVirtualRegister();

                Instr.AddVirtualRegister(NextVReg, BitWidth);
                Instr.AddStackAccess(IRVregToLLIRVreg[Val->GetID()]);

                CurrentBB->InsertInstr(Instr);
            }
            else
                NextVReg = IRVregToLLIRVreg[Val->GetID()];
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
    auto Op     = GetMachineOperandFromValue(I->GetOperand());

    ResultMI.AddOperand(Result);
    ResultMI.AddOperand(Op);

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
            unsigned StructBitSize = (I->GetSavedValue()->GetTypeRef().GetByteSize() * 8);
            unsigned MaxRegSize    = TM->GetPointerSize();
            unsigned RegsCount =
                GetNextAlignedValue(StructBitSize, MaxRegSize) / MaxRegSize;

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
        if (Param->GetTypeRef().IsStruct() && !Param->GetTypeRef().IsPointer())
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

            auto Src              = GetMachineOperandFromValue(Param);
            auto ParamPhysReg     = TargetArgRegs[ParamCounter]->GetID();
            auto ParamPhysRegSize = TargetArgRegs[ParamCounter]->GetBitWidth();

            if (Src.GetSize() < ParamPhysRegSize)
            {
                ParamPhysReg = TargetArgRegs[ParamCounter]->GetSubRegs()[0];
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

    assert(RegsCount > 0);

    auto &RetRegs = TM->GetABI()->GetReturnRegisters();
    for (size_t i = 0; i < RegsCount; i++)
    {
        // FIXME: actual its not a vreg, but this make sure it will be a unique ID
        auto StackSlot = ParentFunction->GetNextAvailableVirtualRegister();
        ParentFunction->InsertStackSlot(StackSlot, std::min(RetBitSize, MaxRegSize) / 8);
        IRVregToLLIRVreg[I->GetID()] = StackSlot;

        auto Store = MachineInstruction(MachineInstruction::Store, CurrentBB);
        Store.AddStackAccess(StackSlot);

        // find the appropriate return register for the size
        unsigned TargetRetReg;

        // if the return value can use the return register
        if (std::min(RetBitSize, MaxRegSize) >= TM->GetPointerSize())
            TargetRetReg = RetRegs[i]->GetID();
        // need to find an appropriate sized subregister of the actual return reg
        else
            // FIXME: Temporary solution, only work for AArch64
            TargetRetReg = RetRegs[i]->GetSubRegs()[0];

        Store.AddRegister(TargetRetReg, std::min(RetBitSize, MaxRegSize));
        // TODO: ...
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

    auto SourceID       = GetIDFromValue(I->GetSource());
    const bool IsGlobal = I->GetSource()->IsGlobalVar();
    const bool IsStack  = ParentFunction->IsStackSlot(SourceID);
    const bool IsReg    = !IsGlobal && !IsStack;

    if (IsGlobal)
        GoalInst = MachineInstruction(MachineInstruction::GlobalAddress, CurrentBB);
    else if (IsStack)
        GoalInst = MachineInstruction(MachineInstruction::StackAddress, CurrentBB);

    auto Dest = GetMachineOperandFromValue((Value *)I);
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
        if (!SourceType.IsStruct())
        {
            if (!GoalInst.IsInvalid())
                CurrentBB->InsertInstr(GoalInst);

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
            // FIXME: this should not needed, only done because AArch64 does not
            // support immediate operands for MUL, this should be handled by the
            // target legalizer
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
        // ConstantIndexPart = SourceType.GetElemByteOffset(Index);
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
        ADD.AddOperand(Dest);

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

    auto Result = GetMachineOperandFromValue(I->GetRetVal());

    ResultMI.AddOperand(Result);

    // insert load to load in the return val to the return registers
    auto &TargetRetRegs = TM->GetABI()->GetReturnRegisters();
    if (I->GetRetVal()->GetTypeRef().IsStruct())
    {
        // how many register are used to pass this struct
        unsigned StructBitSize = (I->GetRetVal()->GetTypeRef().GetByteSize() * 8);
        unsigned MaxRegSize    = TM->GetPointerSize();
        unsigned RegsCount = GetNextAlignedValue(StructBitSize, MaxRegSize) / MaxRegSize;

        auto RetID = GetIDFromValue(I->GetRetVal());
        for (size_t i = 0; i < RegsCount; i++)
        {
            auto Instr = MachineInstruction(MachineInstruction::Load, CurrentBB);

            Instr.AddRegister(TargetRetRegs[i]->GetID(), TargetRetRegs[i]->GetBitWidth());
            Instr.AddStackAccess(RetID, i * (TM->GetPointerSize() / 8));

            CurrentBB->InsertInstr(Instr);
        }
    }
    else if (I->GetRetVal()->IsConstant())
    {
        auto &RetRegs = TM->GetABI()->GetReturnRegisters();
        auto LoadImm  = MachineInstruction(MachineInstruction::LoadImm, CurrentBB);

        // TODO: make it target independent by searching for the right sized register,
        // do it like register allocator.
        if (RetRegs[0]->GetBitWidth() == I->GetBitWidth())
            LoadImm.AddRegister(RetRegs[0]->GetID(), RetRegs[0]->GetBitWidth());
        else
            LoadImm.AddRegister(RetRegs[0]->GetSubRegs()[0],
                                TM->GetRegInfo()
                                    ->GetRegisterByID(RetRegs[0]->GetSubRegs()[0])
                                    ->GetBitWidth());

        LoadImm.AddOperand(GetMachineOperandFromValue(I->GetRetVal()));

        CurrentBB->InsertInstr(LoadImm);
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

    auto IsPointer = ReferedType.GetPointerLevel() > 0;

    Func->InsertStackSlot(Instr->GetID(),
                          IsPointer ? TM->GetPointerSize() / 8 :
                                      ReferedType.GetByteSize());
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

            // FIXME: The maximum allowed structure size which allowed to be passed
            // by the target is target dependent. Remove the hardcoded value and
            // ask the target for the right size.
            unsigned MaxStructSize = 128;    // bit size
            for (size_t i = 0; i < MaxStructSize / TM->GetPointerSize(); i++)
            {
                auto NextVReg = Func->GetNextAvailableVirtualRegister();
                StructToRegMap[StructName].push_back(NextVReg);
                Func->InsertParameter(NextVReg,
                                      LowLevelType::CreateInt(TM->GetPointerSize()));
            }

            continue;
        }

        if (Param->GetTypeRef().IsPointer())
            Func->InsertParameter(ParamID,
                                  LowLevelType::CreatePtr(TM->GetPointerSize()),
                                  IsStructPtr);
        else
            Func->InsertParameter(ParamID,
                                  LowLevelType::CreateInt(ParamSize),
                                  IsStructPtr);
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
                GD.InsertAllocation(Size, 0);
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
            GD.InsertAllocation(Size, 0);
        else
        {
            GD.InsertAllocation(Size, InitList[0]);
        }

        TU->AddGlobalData(GD);
    }
}
