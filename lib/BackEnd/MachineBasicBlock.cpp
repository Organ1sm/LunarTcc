#include <cassert>
#include "BackEnd/MachineBasicBlock.hpp"


void MachineBasicBlock::InsertInstr(MachineInstruction MI)
{
    if (MI.GetParent() == nullptr)
        MI.SetParent(this);

    Instructions.push_back(MI);
}

auto MachineBasicBlock::InsertInstr(MachineInstruction MI, std::size_t Pos)
    -> MachineBasicBlock::InstructionList::iterator
{
    if (MI.GetParent() == nullptr)
        MI.SetParent(this);

    return Instructions.insert(Instructions.begin() + Pos, MI);
}

auto MachineBasicBlock::InsertInstrToFront(MachineInstruction MI)
    -> MachineBasicBlock::InstructionList::iterator
{
    if (MI.GetParent() == nullptr)
        MI.SetParent(this);

    return Instructions.insert(Instructions.begin(), MI);
}

auto MachineBasicBlock::InsertAfter(MachineInstruction MI, MachineInstruction *AfterMI)
    -> MachineBasicBlock::InstructionList::iterator
{
    std::size_t i = 0;
    for (; i < Instructions.size(); i++)
    {
        if (&Instructions[i] == AfterMI)
            break;
    }

    assert(i < Instructions.size() && "Instruction not found in the list.");

    return InsertInstr(MI, i + 1);
}

auto MachineBasicBlock::ReplaceInstr(MachineInstruction MI,
                                     MachineInstruction *Replacable)
    -> MachineBasicBlock::InstructionList::iterator
{
    std::size_t i = 0;
    for (; i < Instructions.size(); i++)
    {
        auto Ptr = &Instructions[i];
        if (Ptr == Replacable)
        {
            Instructions[i] = std::move(MI);
            return Instructions.begin() + i;
        }
    }

    assert(!"Replacable Instruction was not found.");

    return Instructions.end();
}
