#include <iostream>
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "fmt/core.h"

void MachineFunction::InsertStackSlot(unsigned int ID, unsigned int Size)
{
    if (NextVirtualReg < ID)
        NextVirtualReg = ID;

    SF.InsertStackSlot(ID, Size);
}

void MachineFunction::InsertParameter(unsigned int ID, LowLevelType LLT)
{
    Parameters.push_back({ID, LLT});
}

unsigned MachineFunction::GetNextAvailableVirtualRegister()
{
    // If this function was called the first time then here the highest virtual
    // register ID is searched and NextVReg is set to that.
    for (auto &[ParamID, ParamLLT] : Parameters)
    {
        if (ParamID > NextVirtualReg)
            NextVirtualReg = ParamID;
    }

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

    return NextVirtualReg + 1;
}

void MachineFunction::Print(TargetMachine *TM) const
{
    fmt::print("Function: {}\n", Name);
    fmt::print("{:^15}\n", "StackFrame:");
    SF.Print();

    for (auto &BB : BasicBlocks)
        BB.Print(TM);

    fmt::print("\n\n");
}
