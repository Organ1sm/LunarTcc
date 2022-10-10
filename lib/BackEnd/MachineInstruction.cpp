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

void MachineInstruction::AddRegister(uint64_t Reg)
{
    AddOperand(MachineOperand::CreateRegister(Reg));
}

void MachineInstruction::AddImmediate(uint64_t Num)
{
    AddOperand(MachineOperand::CreateImmediate(Num));
}

void MachineInstruction::AddMemory(uint64_t Id)
{
    AddOperand(MachineOperand::CreateMemory(Id));
}

void MachineInstruction::AddStackAccess(uint64_t Slot)
{
    AddOperand(MachineOperand::CreateStackAccess(Slot));
}

void MachineInstruction::AddLabel(const char *Label)
{
    AddOperand(MachineOperand::CreateLabel(Label));
}
