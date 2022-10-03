#include "BackEnd/MachineInstruction.hpp"

void MachineInstruction::RemoveOperand(std::size_t Index)
{
    Operands.erase(Operands.begin() + Index);
}

void MachineInstruction::InsertOperand(std::size_t Index, MachineOperand Operand)
{
    Operands.insert(Operands.begin() + Index, Operand);
}

void MachineInstruction::RemoveMemOperand()
{
    for (std::size_t i = 0; i < Operands.size(); i++)
    {
        if (Operands[i].IsStackAccess())
            Operands.erase(Operands.begin() + i--);
    }
}
