#include <cassert>
#include <string>
#include <iostream>
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
        if (Operands[i].IsStackAccess() || Operands[i].IsMemory())
        {
            Operands.erase(Operands.begin() + i--);
            return;
        }
    }

    assert(!"Nothing was removed");
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

void MachineInstruction::AddStackAccess(uint64_t Slot, unsigned Size)
{
    AddOperand(MachineOperand::CreateStackAccess(Slot));
}

void MachineInstruction::AddLabel(const char *Label)
{
    AddOperand(MachineOperand::CreateLabel(Label));
}

void MachineInstruction::Print() const
{
    std::string OpcodeStr;

    switch (Opcode)
    {
        case OperationCode::And:
            OpcodeStr = "And ";
            break;
        case OperationCode::Or:
            OpcodeStr = "Or  ";
            break;
        case OperationCode::Add:
            OpcodeStr = "Add ";
            break;
        case OperationCode::Sub:
            OpcodeStr = "Sub ";
            break;
        case OperationCode::Mul:
            OpcodeStr = "Mul ";
            break;
        case OperationCode::Div:
            OpcodeStr = "Div ";
            break;
        case OperationCode::Mod:
            OpcodeStr = "Mod ";
            break;
        case OperationCode::Cmp:
            OpcodeStr = "Cmp ";
            break;
        case OperationCode::SExt:
            OpcodeStr = "SEXT";
            break;
        case OperationCode::ZExt:
            OpcodeStr = "ZExt";
            break;
        case OperationCode::Trunc:
            OpcodeStr = "Trunc";
            break;
        case OperationCode::Store:
            OpcodeStr = "Store";
            break;
        case OperationCode::Load:
            OpcodeStr = "Load";
            break;
        case OperationCode::Jump:
            OpcodeStr = "Jump";
            break;
        case OperationCode::Branch:
            OpcodeStr = "Branch";
            break;
        case OperationCode::Ret:
            OpcodeStr = "Ret ";
            break;

        default:
            break;
    }

    std::cout << OpcodeStr << "\t";
    for (std::size_t i = 0; i < Operands.size(); i++)
    {
        Operands[i].Print();
        if (i < Operands.size() - 1)
            std::cout << ", ";
    }

    std::cout << std::endl;
}
