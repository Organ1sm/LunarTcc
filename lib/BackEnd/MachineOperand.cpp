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
