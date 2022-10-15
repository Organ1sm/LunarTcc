#pragma once

#include <cstdint>
#include "BackEnd/LowLevelType.hpp"

class MachineOperand
{
  public:
    enum MOKind : unsigned {
        None,
        Register,
        VirtualRegister,
        IntImmediate,
        MemoryAddress,
        StackAccess,
        Paramter,
        Label
    };

    MachineOperand() {}

    void SetTypeToLabel() { Type = Label; }
    void SetTypeToIntImm() { Type = IntImmediate; }
    void SetTypeToMemAddr() { Type = MemoryAddress; }
    void SetTypeToRegister() { Type = Register; }
    void SetTypeToParameter() { Type = Paramter; }
    void SetTypeToStackAccess() { Type = StackAccess; }
    void SetTypeToVirtualRegister() { Type = VirtualRegister; }

    int64_t GetReg() const { return Value; }
    uint64_t GetSlot() const { return Value; }
    int64_t GetImmediate() const { return Value; }

    void SetReg(uint64_t V) { SetValue(V); }
    void SetValue(uint64_t V) { Value = V; }

    void SetType(LowLevelType LLT) { this->LLT = LLT; }
    LowLevelType GetType() const { return LLT; }

    const char *GetLabel() { return BelongToLabel; }
    void SetLabel(const char *L) { BelongToLabel = L; }

    bool IsLabel() const { return Type == Label; }
    bool IsMemory() const { return Type == MemoryAddress; }
    bool IsRegister() const { return Type == Register; }
    bool IsImmediate() const { return Type == IntImmediate; }
    bool IsParameter() const { return Type == Paramter; }
    bool IsVirtualReg() const { return Type == VirtualRegister; }
    bool IsStackAccess() const { return Type == StackAccess; }

    static MachineOperand CreateRegister(uint64_t Reg);
    static MachineOperand CreateImmediate(uint64_t Val);
    static MachineOperand CreateVirtualRegister(uint64_t Reg);
    static MachineOperand CreateMemory(uint64_t Id);
    static MachineOperand CreateStackAccess(uint64_t Slot);
    static MachineOperand CreateParameter(uint64_t Val);
    static MachineOperand CreateLabel(const char *Label);

    void Print() const;

  private:
    unsigned Type {None};
    uint64_t Value            = ~0;
    const char *BelongToLabel = nullptr;
    LowLevelType LLT;
};
