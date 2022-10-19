#include "BackEnd/TargetArchs/AArch64/AArch64TargetABI.hpp"
#include "BackEnd/RegisterInfo.hpp"

using namespace AArch64;

AArch64TargetABI::AArch64TargetABI(RegisterInfo *RI)
{
    this->StackAlignment = 16;

    // x0 - x7 regs
    for (int i = 32; i <= 39; i++)
    {
        ArgumentRegisters.push_back(RI->GetRegister(i));
        CallerSavedRegisters.push_back(RI->GetRegister(i));
        ReturnRegisters.push_back(RI->GetRegister(i));
    }
    // sp reg
    CalleeSavedRegisters.push_back(RI->GetRegister(RI->GetStackRegister()));
    // fp reg
    CalleeSavedRegisters.push_back(RI->GetRegister(RI->GetFrameRegister()));

    // x19 - x28 regs
    for (int i = 51; i <= 60; i++)
        CalleeSavedRegisters.push_back(RI->GetRegister(i));

    // x9 - x15
    for (int i = 41; i <= 47; i++)
        CallerSavedRegisters.push_back(RI->GetRegister(i));
}
