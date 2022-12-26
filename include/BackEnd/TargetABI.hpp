#pragma once

#include <vector>
#include "BackEnd/TargetRegister.hpp"

class TargetABI
{
    using RegList = std::vector<TargetRegister *>;

  public:
    TargetABI() {}

    unsigned GetStackAlignment() const { return StackAlignment; }
    void SetStackAlignment(unsigned Alignment) { StackAlignment = Alignment; }

    unsigned GetMaxStructSizePassedByValue() const { return MaxStructSize; }
    void SetMaxStructSizePassedByValue(unsigned S) { MaxStructSize = S; }

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
        CalleeSavedRegisters = CalleeSaved;
    }

    RegList &GetReturnRegisters() { return ReturnRegisters; }
    void SetReturnRegisters(RegList ReturnRegs) { ReturnRegisters = ReturnRegs; }

    std::size_t GetFirstFPArgRegIdx() const { return FirstFPArgRegIdx; }
    std::size_t GetFirstFPRetRegIdx() const { return FirstFPRetRegIdx; }

    bool HasCLib() const { return CLib; }

  protected:
    unsigned StackAlignment = ~0;
    unsigned MaxStructSize  = ~0;

    /// Targets should fill this by the general registers they use for passing
    /// integer values.
    /// Then fill floating point values with the floating point registers.
    unsigned FirstFPArgRegIdx = 0;
    unsigned FirstFPRetRegIdx = 0;

    bool CLib = false;

    RegList ArgumentRegisters;
    RegList CalleeSavedRegisters;
    RegList CallerSavedRegisters;
    RegList ReturnRegisters;
};
