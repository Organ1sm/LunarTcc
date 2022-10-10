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
