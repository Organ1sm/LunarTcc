#include <cassert>
#include "BackEnd/TargetMachine.hpp"

bool TargetMachine::SelectInstruction(MachineInstruction *MI)
{
    auto Opcode = MI->GetOpcode();

    switch (Opcode)
    {
        case MachineInstruction::Add:
            return SelectAdd(MI);
        case MachineInstruction::Sub:
            return SelectSub(MI);
        case MachineInstruction::Mul:
            return SelectMul(MI);
        case MachineInstruction::Div:
            return SelectDiv(MI);
        case MachineInstruction::Cmp:
            return SelectCmp(MI);
        case MachineInstruction::Mod:
            return SelectMod(MI);
        case MachineInstruction::Load:
            return SelectLoad(MI);
        case MachineInstruction::Store:
            return SelectStore(MI);
        case MachineInstruction::Branch:
            return SelectBranch(MI);
        case MachineInstruction::Jump:
            return SelectJump(MI);
        case MachineInstruction::Ret:
            return SelectRet(MI);
        default:
            assert(!"Unimplemented.");
            break;
    }

    return false;
}