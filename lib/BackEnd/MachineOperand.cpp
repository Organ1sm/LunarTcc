#include <cstdint>
#include <iostream>
#include "BackEnd/LowLevelType.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/TargetMachine.hpp"

MachineOperand MachineOperand::CreateRegister(uint64_t Reg, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToRegister();
    MO.SetReg(Reg);
    MO.SetType(LowLevelType::CreateInt(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateImmediate(uint64_t Val, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToIntImm();
    MO.SetValue(Val);
    MO.SetType(LowLevelType::CreateInt(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateVirtualRegister(uint64_t Reg, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToVirtualRegister();
    MO.SetReg(Reg);
    MO.SetType(LowLevelType::CreateInt(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateMemory(uint64_t Id)
{
    MachineOperand MO;
    MO.SetTypeToMemAddr();
    MO.SetValue(Id);

    return MO;
}

MachineOperand MachineOperand::CreateStackAccess(uint64_t Slot, int Offset)
{
    MachineOperand MO;
    MO.SetTypeToStackAccess();
    MO.SetValue(Slot);
    MO.SetOffset(Offset);

    return MO;
}

MachineOperand MachineOperand::CreateParameter(uint64_t Val)
{
    MachineOperand MO;
    MO.SetTypeToParameter();
    MO.SetReg(Val);

    return MO;
}

MachineOperand MachineOperand::CreateLabel(const char *Label)
{
    MachineOperand MO;
    MO.SetTypeToLabel();
    MO.SetLabel(Label);

    return MO;
}

void MachineOperand::Print(TargetMachine *TM) const
{
    switch (this->Type)
    {
        case MachineOperand::Register:
            if (TM)
            {
                TargetRegister *Reg = TM->GetRegInfo()->GetRegisterByID(GetReg());
                std::string RegStr =
                    Reg->GetAlias() != "" ? Reg->GetAlias() : Reg->GetName();

                std::cout << RegStr;
            }
            else
                std::cout << "%" << Value;
            break;
        case MachineOperand::VirtualRegister:
            std::cout << "%vr-" << Value;
            break;
        case MachineOperand::MemoryAddress:
            std::cout << "%ptr-vr-" << Value;
            break;
        case MachineOperand::IntImmediate:
            std::cout << static_cast<int64_t>(Value);
            break;
        case MachineOperand::StackAccess:
            std::cout << "stack" << Value << "+" << Offset;
            break;
        case MachineOperand::Paramter:
            std::cout << "@" << Value;
            break;
        case MachineOperand::Label:
            std::cout << "<" << BelongToLabel << ">";
            break;
        default:
            break;
    }

    if (LLT.IsValid())
        std::cout << "(" << LLT.ToString() << ")";
}
