#include <cstdint>
#include <iostream>
#include "BackEnd/MachineOperand.hpp"

MachineOperand MachineOperand::CreateRegister(uint64_t Reg)
{
    MachineOperand MO;
    MO.SetTypeToRegister();
    MO.SetReg(Reg);

    return MO;
}

MachineOperand MachineOperand::CreateImmediate(uint64_t Val)
{
    MachineOperand MO;
    MO.SetTypeToIntImm();
    MO.SetValue(Val);

    return MO;
}

MachineOperand MachineOperand::CreateVirtualRegister(uint64_t Reg)
{
    MachineOperand MO;
    MO.SetTypeToVirtualRegister();
    MO.SetReg(Reg);

    return MO;
}

MachineOperand MachineOperand::CreateMemory(uint64_t Id)
{
    MachineOperand MO;
    MO.SetTypeToMemAddr();
    MO.SetValue(Id);

    return MO;
}

MachineOperand MachineOperand::CreateStackAccess(uint64_t Slot)
{
    MachineOperand MO;
    MO.SetTypeToStackAccess();
    MO.SetValue(Slot);

    return MO;
}

MachineOperand MachineOperand::CreateLabel(const char *Label)
{
    MachineOperand MO;
    MO.SetTypeToLabel();
    MO.SetLabel(Label);

    return MO;
}

void MachineOperand::Print() const
{
    switch (this->Type)
    {
        case MachineOperand::Register:
            std::cout << "%" << Value;
            break;
        case MachineOperand::VirtualRegister:
            std::cout << "%vr-" << Value;
            break;
        case MachineOperand::IntImmediate:
            std::cout << static_cast<int64_t>(Value);
            break;
        case MachineOperand::StackAccess:
            std::cout << "stack" << Value;
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
}
