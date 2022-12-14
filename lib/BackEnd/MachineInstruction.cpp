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
    assert(Index < GetOperandsNumber());
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
            assert(i < GetOperandsNumber());
            Operands.erase(Operands.begin() + i--);
            return;
        }
    }

    assert(!"Nothing was removed");
}

void MachineInstruction::UpdateAttributes()
{
    switch (Opcode)
    {
        case SExtLoad:
        case ZExtLoad:
        case Load: AddAttribute(IsLOAD); break;
        case Store: AddAttribute(IsSTORE); break;
        case Ret: AddAttribute(IsRETURN); break;
        case Jump: AddAttribute(IsJUMP); break;
        case Call: AddAttribute(IsCALL); break;

        default: break;
    }
}

void MachineInstruction::SetOpcode(unsigned int Opcode)
{
    this->Opcode = Opcode;

    if (Opcode >= (1 << 16))
    {
        OtherAttributes = 0;
        UpdateAttributes();
    }
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
        case OperationCode::Cmp:
            OpcodeStr = fmt::format("Cmp.{}", GetRelationString());
            break;

        case OperationCode::ModU: OpcodeStr = "ModU"; break;
        case OperationCode::DivU: OpcodeStr = "DivU"; break;
        case OperationCode::AddF: OpcodeStr = "AddF"; break;
        case OperationCode::SubF: OpcodeStr = "SubF"; break;
        case OperationCode::MulF: OpcodeStr = "MulF"; break;
        case OperationCode::DivF: OpcodeStr = "DivF"; break;
        case OperationCode::CmpF:
            OpcodeStr = fmt::format("CmpF.{}", GetRelationString());
            break;

        case OperationCode::SExt: OpcodeStr = "SEXT"; break;
        case OperationCode::ZExt: OpcodeStr = "ZExt"; break;
        case OperationCode::ZExtLoad: OpcodeStr = "ZExtLoad"; break;
        case OperationCode::SExtLoad: OpcodeStr = "SExtLoad"; break;
        case OperationCode::Trunc: OpcodeStr = "Trunc"; break;
        case OperationCode::FloatToInt: OpcodeStr = "FloatToInt"; break;
        case OperationCode::IntToFloat: OpcodeStr = "IntToFloat"; break;
        case OperationCode::BitCast: OpcodeStr = "BitCast"; break;
        case OperationCode::Store: OpcodeStr = "Store"; break;
        case OperationCode::StackAddress: OpcodeStr = "StackAddress"; break;
        case OperationCode::GlobalAddress: OpcodeStr = "GlobalAddress"; break;
        case OperationCode::Mov: OpcodeStr = "Mov"; break;
        case OperationCode::MovF: OpcodeStr = "MovF"; break;
        case OperationCode::LoadImm: OpcodeStr = "LoadImm"; break;
        case OperationCode::Load: OpcodeStr = "Load"; break;
        case OperationCode::Call: OpcodeStr = "Call"; break;
        case OperationCode::Jump: OpcodeStr = "Jump"; break;
        case OperationCode::Branch: OpcodeStr = "Branch"; break;
        case OperationCode::Ret: OpcodeStr = "Ret"; break;
        case OperationCode::AddS: OpcodeStr = "AddS"; break;
        case OperationCode::AddC: OpcodeStr = "AddC"; break;
        case OperationCode::MulHU: OpcodeStr = "MulHU"; break;
        case OperationCode::Merge: OpcodeStr = "Merge"; break;
        case OperationCode::Split: OpcodeStr = "Split"; break;

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

MachineOperand *MachineInstruction::GetDefine()
{
    if (Opcode == Ret || Opcode == Jump || Opcode == Branch || IsStore())
        return nullptr;

    assert(!Operands.empty());

    return GetOperand(0);
}

MachineOperand *MachineInstruction::GetNthUse(std::size_t N)
{
    // Ret only has operands, no define
    if (Opcode != Ret && Opcode != Jump && Opcode != Branch && !IsStore())
        N++;    // Others first operand is usually a define, so skip it

    if (Operands.size() <= N)
        return nullptr;

    return GetOperand(N);
}

void MachineInstruction::SetNthUse(std::size_t N, MachineOperand *Use)
{
    // Ret only has operands, no define
    if (Opcode != Ret && Opcode != Jump && Opcode != Branch && !IsStore())
        N++;    // Others first operand is usually a define, so skip it


    if (Operands.size() <= N)
        return;

    ReplaceOperand(*Use, N);
}

const char *MachineInstruction::GetRelationString() const
{
    switch (Attributes)
    {
        case EQ: return "EQ";
        case NE: return "NE";
        case LT: return "LT";
        case GT: return "GT";
        case LE: return "LE";
        case GE: return "GE";

        default: assert(!"Should not be invalid");
    }
}
