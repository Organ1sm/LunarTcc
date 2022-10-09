#include "BackEnd/TargetArchs/AArch64/AArch64InstructionLegalizer.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include <cassert>

using namespace AArch64;

// Mod operation is not legal on ARM, has to be expanded
bool AArch64InstructionLegalizer::Check(const MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod:
            return false;
        default:
            break;
    }

    return true;
}

bool AArch64InstructionLegalizer::IsExpandable(const MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod:
            return true;
        default:
            break;
    }

    return false;
}
