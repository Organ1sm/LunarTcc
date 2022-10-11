#include <iostream>
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineBasicBlock.hpp"

unsigned MachineFunction::GetNextAvailableVirtualRegister()
{
    // If the next virtual register was computed already once, then just
    // increment it
    if (NextVirtualReg != 0)
        return NextVirtualReg++;

    // If this function was called the first time then here the highest virtual
    // register ID is searched and NextVReg is set to that.
    for (auto &BB : BasicBlocks)
    {
        for (auto &Instr : BB.GetInstructions())
        {
            for (auto &Operand : Instr.GetOperands())
            {
                if (Operand.IsVirtualReg() && Operand.GetReg() > NextVirtualReg)
                    NextVirtualReg = Operand.GetReg();
            }
        }
    }

    return ++NextVirtualReg;
}

void MachineFunction::Print() const
{
    std::cout << "Function: " << Name << std::endl;

    for (auto &BB : BasicBlocks)
        BB.Print();
}
