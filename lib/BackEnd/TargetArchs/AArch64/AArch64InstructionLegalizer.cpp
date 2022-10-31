#include "BackEnd/TargetArchs/AArch64/AArch64InstructionLegalizer.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64InstructionDefinitions.hpp"
#include "BackEnd/TargetMachine.hpp"
#include <cassert>

using namespace AArch64;

// Mod operation is not legal on ARM, has to be expanded
bool AArch64InstructionLegalizer::Check(MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod: return false;
        case MachineInstruction::Sub:
            if (MI->GetOperand(1)->IsImmediate())
                return false;
            break;
        case MachineInstruction::Store:
            if (MI->GetOperands().back().IsImmediate())
                return false;
            break;
        case MachineInstruction::ZExt:
        case MachineInstruction::GlobalAddress: return false;

        default: break;
    }

    return true;
}

bool AArch64InstructionLegalizer::IsExpandable(const MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod:
        case MachineInstruction::Sub:
        case MachineInstruction::Store:
        case MachineInstruction::ZExt:
        case MachineInstruction::GlobalAddress: return true;

        default: break;
    }

    return false;
}

/// Since AArch64 does not support for immediate operand as 1st source operand
/// for SUB (and for all arithmetic instruction as well), there for it has to
/// be materialized first into a register
/// TODO: expand the implementation for all arithmetic instruction
bool AArch64InstructionLegalizer::ExpandSub(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "Sub must have exactly 3 operands");
    auto ParentBB = MI->GetParent();

    auto Mov     = MachineInstruction(MachineInstruction::LoadImm, nullptr);
    auto DestReg = ParentBB->GetParent()->GetNextAvailableVirtualRegister();

    // replace the immediate operand with the destination of the immediate load
    Mov.AddVirtualRegister(DestReg, MI->GetOperand(1)->GetSize());
    Mov.AddOperand(*MI->GetOperand(1));

    MI->RemoveOperand(1);
    MI->InsertOperand(
        1,
        MachineOperand::CreateVirtualRegister(DestReg, MI->GetOperand(0)->GetSize()));

    ParentBB->InsertBefore(std::move(Mov), MI);

    return true;
}

/// Since AArch64 do sign extension when loading therefore if the ZEXT is
/// used only to zero extend a load result then it can be merged with the
/// previous load into a ZExtLoad.
bool AArch64InstructionLegalizer::ExpandZExt(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "ZEXT must have exactly 2 operands");
    auto ParentBB = MI->GetParent();

    auto PrevInst = ParentBB->GetPrecedingInstr(MI);

    // If not a LOAD then do nothing
    if (!(PrevInst->GetOpcode() == MachineInstruction::Load))
        return true;

    auto ZEXTDest = *MI->GetOperand(0);
    PrevInst->InsertOperand(0, ZEXTDest);
    PrevInst->SetOpcode(MachineInstruction::ZExtLoad);
    ParentBB->Erase(MI);

    return true;
}

/// Use wzr register if the stored immediate is 0
bool AArch64InstructionLegalizer::ExpandStore(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "Store must have exactly 2 operands");

    auto ParentBB       = MI->GetParent();
    auto ParentFunction = ParentBB->GetParent();
    auto Immediate      = *MI->GetOperand(1);

    assert(Immediate.IsImmediate() && "Operand #2 must be an immediate");

    if (Immediate.GetImmediate() == 0)
    {
        auto WZR = TM->GetRegInfo()->GetZeroRegister();

        MI->RemoveOperand(1);
        MI->AddRegister(WZR, TM->GetRegInfo()->GetRegisterByID(WZR)->GetBitWidth());

        return true;
    }

    auto LoadImmResult     = ParentFunction->GetNextAvailableVirtualRegister();
    auto LoadImmResultVReg = MachineOperand::CreateVirtualRegister(LoadImmResult);

    // Replace the immediate operand with the result register
    MI->RemoveOperand(1);
    MI->AddOperand(LoadImmResultVReg);

    MachineInstruction LOAD_IMM;
    LOAD_IMM.SetOpcode(MachineInstruction::LoadImm);
    LOAD_IMM.AddOperand(LoadImmResultVReg);
    LOAD_IMM.AddOperand(Immediate);

    ParentBB->InsertBefore(std::move(LOAD_IMM), MI);

    return true;
}


/// The global address materialization happens in two steps on arm. Example:
///   adrp x0, global_var
///   add  x0, x0, :lo12:global_var
bool AArch64InstructionLegalizer::ExpandGlobalAddress(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "GlobalAddress must have exactly 2 operands.");

    auto ParentBB  = MI->GetParent();
    auto GlobalVar = *MI->GetOperand(1);
    assert(GlobalVar.IsGlobalSymbol() && "Operand #2 must be a symbol");

    auto GlobalVarName = ":lo12:" + GlobalVar.GetGlobalSymbol();

    MI->SetOpcode(ADRP);

    MachineInstruction ADD;
    ADD.SetOpcode(MachineInstruction::Add);

    auto DestReg = *MI->GetOperand(0);

    ADD.AddOperand(DestReg);
    ADD.AddOperand(DestReg);
    ADD.AddGlobalSymbol(GlobalVarName);

    ParentBB->InsertAfter(std::move(ADD), MI);

    return true;
}
