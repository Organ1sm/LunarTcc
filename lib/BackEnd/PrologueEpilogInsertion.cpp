#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/PrologueEpilogInsertion.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/Support.hpp"
#include <cassert>

/// TODO: Solve the stack issue: inserting physregs can collide with existing
/// stack slot with the same ID
static int NextStackSlot = 0;
std::map<unsigned, unsigned> LocalPhysRegToStackSlotMap;

MachineInstruction
    PrologueEpilogInsertion::CreateAddInstruction(int64_t StackAdjustmentSize)
{
    MachineInstruction Add(MachineInstruction::Add, nullptr);

    auto SPReg     = TM->GetRegInfo()->GetStackRegister();
    auto SPRegSize = TM->GetRegInfo()->GetRegisterByID(SPReg)->GetBitWidth();

    Add.AddRegister(SPReg, SPRegSize);
    Add.AddRegister(SPReg, SPRegSize);
    Add.AddImmediate(StackAdjustmentSize);

    if (!TM->SelectInstruction(&Add))
    {
        assert(!"Unable to select instruction");
        return MachineInstruction();
    }

    return Add;
}

void PrologueEpilogInsertion::InsertLinkRegisterSave(MachineFunction &Func)
{
    if (!Func.IsCaller())
        return;

    MachineInstruction STR(MachineInstruction::Store, nullptr);

    auto LROffset = Func.GetStackObjectPosition(
        LocalPhysRegToStackSlotMap[TM->GetRegInfo()->GetLinkRegister()]);
    LROffset = GetNextAlignedValue(LROffset, 16);

    auto SPReg = TM->GetRegInfo()->GetStackRegister();
    auto Dest  = TM->GetRegInfo()->GetLinkRegister();

    STR.AddRegister(Dest);
    STR.AddRegister(SPReg);
    STR.AddImmediate(LROffset);

    if (!TM->SelectInstruction(&STR))
        assert(!"Unable to select instruction");

    // Stack adjusting instruction already inserted at this point so this have
    // to be inserted after it, hence the position 1
    Func.GetBasicBlocks().front().InsertInstr(STR, 1);
}

void PrologueEpilogInsertion::InsertLinkRegisterReload(MachineFunction &Func)
{
    if (!Func.IsCaller())
        return;

    MachineInstruction LOAD(MachineInstruction::Load, nullptr);

    auto LROffset = Func.GetStackObjectPosition(
        LocalPhysRegToStackSlotMap[TM->GetRegInfo()->GetLinkRegister()]);
    LROffset = GetNextAlignedValue(LROffset, 16);

    auto SPReg = TM->GetRegInfo()->GetStackRegister();
    auto Dest  = TM->GetRegInfo()->GetLinkRegister();

    LOAD.AddRegister(Dest);
    LOAD.AddRegister(SPReg);
    LOAD.AddImmediate(LROffset);

    if (!TM->SelectInstruction(&LOAD))
        assert(!"Unable to select instruction");

    auto &LastBB = Func.GetBasicBlocks().back();
    LastBB.InsertInstr(LOAD, LastBB.GetInstructions().size() - 1);
}

void PrologueEpilogInsertion::InsertStackAdjustmentUpward(MachineFunction &Func)
{
    unsigned StackAlignment = TM->GetABI()->GetStackAlignment();
    int64_t StackAdjustmentSize =
        GetNextAlignedValue(Func.GetStackFrameSize(), StackAlignment) * -1;

    MachineInstruction AddToSP = CreateAddInstruction(StackAdjustmentSize);

    Func.GetBasicBlocks().front().InsertInstrToFront(AddToSP);
}

void PrologueEpilogInsertion::InsertStackAdjustmentDownward(MachineFunction &Func)
{
    unsigned StackAlignment = TM->GetABI()->GetStackAlignment();
    int64_t StackAdjustmentSize =
        GetNextAlignedValue(Func.GetStackFrameSize(), StackAlignment);

    MachineInstruction AddToSP = CreateAddInstruction(StackAdjustmentSize);

    // NOTE: for now assuming that there is only one ret instruction and its
    // the last one of the last basic block, so we want to insert above it
    auto &LastBB = Func.GetBasicBlocks().back();
    assert(LastBB.GetInstructions().size() > 0);

    LastBB.InsertInstr(AddToSP, LastBB.GetInstructions().size() - 1);
}


MachineInstruction PrologueEpilogInsertion::CreateStore(MachineFunction &Func,
                                                        unsigned Register)
{
    MachineInstruction STR(MachineInstruction::Store, nullptr);

    auto StackID = Register;
    if (LocalPhysRegToStackSlotMap.count(Register) > 0)
        StackID = LocalPhysRegToStackSlotMap[Register];

    auto Offset = GetNextAlignedValue(Func.GetStackObjectPosition(StackID),
                                      TM->GetPointerSize() / 8);
    auto SPReg  = TM->GetRegInfo()->GetStackRegister();

    STR.AddRegister(Register, TM->GetPointerSize());
    STR.AddRegister(SPReg);
    STR.AddImmediate(Offset);

    if (!TM->SelectInstruction(&STR))
        assert(!"Unable to select instruction");

    return STR;
}

MachineInstruction PrologueEpilogInsertion::CreateLoad(MachineFunction &Func,
                                                       unsigned Register)
{
    MachineInstruction LOAD(MachineInstruction::Load, nullptr);

    auto StackID = Register;
    if (LocalPhysRegToStackSlotMap.count(Register) > 0)
        StackID = LocalPhysRegToStackSlotMap[Register];

    auto Offset = GetNextAlignedValue(Func.GetStackObjectPosition(StackID),
                                      TM->GetPointerSize() / 8);
    auto SPReg  = TM->GetRegInfo()->GetStackRegister();

    LOAD.AddRegister(Register, TM->GetPointerSize());
    LOAD.AddRegister(SPReg);
    LOAD.AddImmediate(Offset);

    if (!TM->SelectInstruction(&LOAD))
        assert(!"Unable to select instruction");

    return LOAD;
}

void PrologueEpilogInsertion::SpillClobberedCalleeSavedRegisters(MachineFunction &Func)
{
    unsigned Counter                = 0;
    const unsigned StartOfInsertion = Func.IsCaller() ? 2 : 1;

    for (auto Reg : Func.GetUsedCalleeSavedRegs())
    {
        auto STR = CreateStore(Func, Reg);
        Func.GetBasicBlocks().front().InsertInstr(STR, StartOfInsertion + Counter);
        Counter++;
    }
}

void PrologueEpilogInsertion::ReloadClobberedCalleeSavedRegisters(MachineFunction &Func)
{
    unsigned Counter = 0;
    auto &LastBB     = Func.GetBasicBlocks().back();

    for (auto Reg : Func.GetUsedCalleeSavedRegs())
    {
        auto LOAD = CreateLoad(Func, Reg);
        LastBB.InsertInstr(LOAD, LastBB.GetInstructions().size() - 1 - Counter);
        Counter++;
    }
}

void PrologueEpilogInsertion::Run()
{
    for (auto &Func : MIRM->GetFunctions())
    {
        if (Func.GetStackFrameSize() == 0 && Func.GetUsedCalleeSavedRegs().empty() &&
            Func.IsCaller())
            continue;

        LocalPhysRegToStackSlotMap.clear();
        NextStackSlot = 10000;

        for (auto CalleSavedReg : Func.GetUsedCalleeSavedRegs())
        {
            Func.GetStackFrame().InsertStackSlot(NextStackSlot, TM->GetPointerSize() / 8);
            LocalPhysRegToStackSlotMap[CalleSavedReg] = NextStackSlot++;
        }

        if (Func.IsCaller())
        {
            Func.GetStackFrame().InsertStackSlot(NextStackSlot, 16);
            LocalPhysRegToStackSlotMap[TM->GetRegInfo()->GetLinkRegister()] =
                NextStackSlot++;
        }

        InsertStackAdjustmentUpward(Func);
        InsertLinkRegisterSave(Func);
        SpillClobberedCalleeSavedRegisters(Func);
        ReloadClobberedCalleeSavedRegisters(Func);
        InsertLinkRegisterReload(Func);
        InsertStackAdjustmentDownward(Func);
    }
}
