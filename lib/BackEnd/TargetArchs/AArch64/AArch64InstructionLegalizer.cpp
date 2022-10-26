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
            assert(MI->GetOperandsNumber() == 2 && "Must have 2 operands");
            if (MI->GetOperand(1)->IsImmediate())
                return false;
            break;
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
        case MachineInstruction::GlobalAddress: return true;

        default: break;
    }

    return false;
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
