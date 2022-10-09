#include "BackEnd/TargetArchs/AArch64/AArch64TargetABI.hpp"
#include "BackEnd/RegisterInfo.hpp"

using namespace AArch64;

AArch64TargetABI::AArch64TargetABI(RegisterInfo *RI)
{
    this->StackAlignment = 16;

    // w0 - w7 regs
    for (int i = 0; i <= 7; i++)
    {
        ArgumentRegisters.push_back(RI->GetRegister(i));
        CallerSavedRegisters.push_back(RI->GetRegister(i));
        ReturnRegisters.push_back(RI->GetRegister(i));
    }
    // sp reg
    CalleeSavedRegisters.push_back(RI->GetRegister(32));
    // fp reg
    CalleeSavedRegisters.push_back(RI->GetRegister(29));

    // w19-w28
    for (int i = 19; i <= 28; i++)
        CalleeSavedRegisters.push_back(RI->GetRegister(i));

    // w9-w15
    for(int i = 9; i <= 15; i++)
        CallerSavedRegisters.push_back(RI->GetRegister(i));

}
