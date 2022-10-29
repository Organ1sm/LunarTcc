#include "BackEnd/TargetArchs/AArch64/AArch64InstructionLegalizer.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64InstructionDefinitions.hpp"
#include <cassert>

using namespace AArch64;

// Mod operation is not legal on ARM, has to be expanded
bool AArch64InstructionLegalizer::Check(MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod: return false;
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
        case MachineInstruction::Store:
        case MachineInstruction::ZExt:
        case MachineInstruction::GlobalAddress: return true;

        default: break;
    }

    return false;
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
