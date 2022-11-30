#include "BackEnd/TargetArchs/AArch64/AArch64TargetABI.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64RegisterInfo.hpp"
#include "BackEnd/RegisterInfo.hpp"

using namespace AArch64;

AArch64TargetABI::AArch64TargetABI(RegisterInfo *RI)
{
    this->StackAlignment = 16;
    this->MaxStructSize  = 128;

    /////// ----- Caller/Callee Registers. ----- ///////
    // x0 - x7 regs
    for (int i = X0; i <= X7; i++)
    {
        ArgumentRegisters.push_back(RI->GetRegister(i));
        CallerSavedRegisters.push_back(RI->GetRegister(i));
        ReturnRegisters.push_back(RI->GetRegister(i));
    }

    FirstFPArgRegIdx = ArgumentRegisters.size();
    FirstFPRetRegIdx = ReturnRegisters.size();

    for (unsigned i = D0; i <= D7; i++)
        ArgumentRegisters.push_back(RI->GetRegisterByID(i));

    /////// ----- Caller/Callee Registers. ----- ///////

    // x19 - x28 regs
    for (int i = X19; i <= X28; i++)
        CalleeSavedRegisters.push_back(RI->GetRegister(i));

    // x9 - x15
    for (int i = X9; i <= X15; i++)
        CallerSavedRegisters.push_back(RI->GetRegister(i));

    // d0 - d7
    for (unsigned i = D0; i <= D7; i++)
        CallerSavedRegisters.push_back(RI->GetRegisterByID(i));

    // d9 - d15
    for (unsigned i = D9; i <= D15; i++)
        CallerSavedRegisters.push_back(RI->GetRegisterByID(i));

    /////// -----  Return Registers ----- ///////

    // d0 - d7
    for (unsigned i = D0; i <= D7; i++)
        ReturnRegisters.push_back(RI->GetRegisterByID(i));
}
