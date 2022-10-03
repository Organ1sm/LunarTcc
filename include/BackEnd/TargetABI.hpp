#ifndef LUNARTCC_TARGETABI_HPP
#define LUNARTCC_TARGETABI_HPP

#include <vector>
#include "BackEnd/TargetRegister.hpp"

class TargetABI
{
    using RegList = std::vector<TargetRegister *>;

  public:
    TargetABI() {}

    unsigned GetStackAlignment() const { return StackAlignment; }
    void SetStackAlignment(unsigned Alignment) { StackAlignment = Alignment; }

    RegList &GetArgumentRegisters() { return ArgumentRegisters; }
    void SetArgumentRegisters(RegList ArgRegs) { ArgumentRegisters = ArgRegs; }

    RegList &GetCallerSavedRegisters() { return CallerSavedRegisters; }
    void SetCallerSavedRegisters(RegList CallerSaved)
    {
        CallerSavedRegisters = CallerSaved;
    }

    RegList &GetCalleeSavedRegisters() { return CalleeSavedRegisters; }
    void SetCalleeSavedRegisters(RegList CalleeSaved)
    {
        CallerSavedRegisters = CalleeSaved;
    }
    

  protected:
    unsigned StackAlignment;
    RegList ArgumentRegisters;
    RegList CalleeSavedRegisters;
    RegList CallerSavedRegisters;
    RegList ReturnRegisters;
};

#endif    // !LUNARTCC_TARGETABI_HPP
