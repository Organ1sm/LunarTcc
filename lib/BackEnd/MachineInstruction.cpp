#include <cassert>
#include <string>
#include <iostream>
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "fmt/core.h"
MachineOperand *MachineInstruction::GetOperand(std::size_t Index)
{
    assert(Index < Operands.size());
    return &Operands[Index];
}

void MachineInstruction::ReplaceOperand(MachineOperand MO, std::size_t Index)
{
    assert(Index < Operands.size());
    Operands[Index] = MO;
}

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

void MachineInstruction::AddRegister(uint64_t Reg, unsigned BitWidth)
{
    AddOperand(MachineOperand::CreateRegister(Reg, BitWidth));
}

void MachineInstruction::AddVirtualRegister(uint64_t Reg, unsigned BitWidth)
{
    AddOperand(MachineOperand::CreateVirtualRegister(Reg, BitWidth));
}

void MachineInstruction::AddImmediate(uint64_t Num, unsigned BitWidth)
{
    AddOperand(MachineOperand::CreateImmediate(Num, BitWidth));
}

void MachineInstruction::AddMemory(uint64_t Id, unsigned BitWidth)
{
    AddOperand(MachineOperand::CreateMemory(Id, BitWidth));
}


void MachineInstruction::AddMemory(uint64_t Id, int Offset, unsigned BitWidth)
{
    AddOperand(MachineOperand::CreateMemory(Id, Offset, BitWidth));
}

void MachineInstruction::AddStackAccess(uint64_t Slot, unsigned Offset)
{
    AddOperand(MachineOperand::CreateStackAccess(Slot, Offset));
}

void MachineInstruction::AddLabel(const char *Label)
{
    AddOperand(MachineOperand::CreateLabel(Label));
}

void MachineInstruction::AddFunctionName(const char *Name)
{
    AddOperand(MachineOperand::CreateFunctionName(Name));
}

void MachineInstruction::AddGlobalSymbol(std::string Symbol)
{
    AddOperand(MachineOperand::CreateGlobalSymbol(Symbol));
}

void MachineInstruction::Print(TargetMachine *TM) const
{
    std::string OpcodeStr;

    switch (Opcode)
    {
        case OperationCode::And: OpcodeStr = "And"; break;
        case OperationCode::Or: OpcodeStr = "Or"; break;
        case OperationCode::XOr: OpcodeStr = "XOr"; break;
        case OperationCode::LSL: OpcodeStr = "LSL"; break;
        case OperationCode::LSR: OpcodeStr = "LSR"; break;
        case OperationCode::Add: OpcodeStr = "Add"; break;
        case OperationCode::Sub: OpcodeStr = "Sub"; break;
        case OperationCode::Mul: OpcodeStr = "Mul"; break;
        case OperationCode::Div: OpcodeStr = "Div"; break;
        case OperationCode::Mod: OpcodeStr = "Mod"; break;
        case OperationCode::Cmp: OpcodeStr = "Cmp"; break;
        case OperationCode::ModU: OpcodeStr = "ModU"; break;
        case OperationCode::DivU: OpcodeStr = "DivU"; break;
        case OperationCode::SExt: OpcodeStr = "SEXT"; break;
        case OperationCode::ZExt: OpcodeStr = "ZExt"; break;
        case OperationCode::ZExtLoad: OpcodeStr = "ZExtLoad"; break;
        case OperationCode::SExtLoad: OpcodeStr = "SExtLoad"; break;
        case OperationCode::Trunc: OpcodeStr = "Trunc"; break;
        case OperationCode::Store: OpcodeStr = "Store"; break;
        case OperationCode::StackAddress: OpcodeStr = "StackAddress"; break;
        case OperationCode::GlobalAddress: OpcodeStr = "GlobalAddress"; break;
        case OperationCode::Mov: OpcodeStr = "Mov"; break;
        case OperationCode::LoadImm: OpcodeStr = "LoadImm"; break;
        case OperationCode::Load: OpcodeStr = "Load"; break;
        case OperationCode::Call: OpcodeStr = "Call"; break;
        case OperationCode::Jump: OpcodeStr = "Jump"; break;
        case OperationCode::Branch: OpcodeStr = "Branch"; break;
        case OperationCode::Ret: OpcodeStr = "Ret"; break;
        case OperationCode::InvalidOp: OpcodeStr = "InvalidOp"; break;

        default: OpcodeStr = TM->GetInstrDefs()->GetInstrString(Opcode); break;
    }

    fmt::print("{:<{}}", OpcodeStr, 14);

    for (std::size_t i = 0; i < Operands.size(); i++)
    {
        Operands[i].Print(TM);
        if (i < Operands.size() - 1)
            fmt::print(", ");
    }

    fmt::print("\n");
}
