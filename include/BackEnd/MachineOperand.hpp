#pragma once

#include <cstdint>
#include "BackEnd/LowLevelType.hpp"

class TargetMachine;

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
        Label,
        FunctionName,
        GlobalSymbol,
    };

    MachineOperand() {}

    void SetTypeToLabel() { Type = Label; }
    void SetTypeToIntImm() { Type = IntImmediate; }
    void SetTypeToMemAddr() { Type = MemoryAddress; }
    void SetTypeToRegister() { Type = Register; }
    void SetTypeToParameter() { Type = Paramter; }
    void SetTypeToStackAccess() { Type = StackAccess; }
    void SetTypeToFunctionName() { Type = FunctionName; }
    void SetTypeToVirtualRegister() { Type = VirtualRegister; }
    void SetTypeToGlobalSymbol() { Type = GlobalSymbol; }

    int64_t GetReg() const { return Value; }
    uint64_t GetSlot() const { return Value; }
    int64_t GetImmediate() const { return Value; }

    void SetOffset(int o) { Offset = o; }
    int GetOffset() const { return Offset; }

    void SetReg(uint64_t V) { SetValue(V); }
    void SetValue(uint64_t V) { Value = V; }

    void SetType(LowLevelType LLT) { this->LLT = LLT; }
    LowLevelType GetType() const { return LLT; }

    const char *GetFunctionName() { return BelongToLabel; }
    const char *GetLabel() { return BelongToLabel; }
    void SetLabel(const char *L) { BelongToLabel = L; }

    std::string &GetGlobalSymbol() { return GlobalSym; }
    void SetGlobalSymbol(const std::string &GS) { GlobalSym = GS; }

    unsigned GetSize() { return LLT.GetBitWidth(); }

    bool IsLabel() const { return Type == Label; }
    bool IsMemory() const { return Type == MemoryAddress; }
    bool IsRegister() const { return Type == Register; }
    bool IsImmediate() const { return Type == IntImmediate; }
    bool IsParameter() const { return Type == Paramter; }
    bool IsVirtualReg() const { return Type == VirtualRegister; }
    bool IsFunctionName() const { return Type == FunctionName; }
    bool IsStackAccess() const { return Type == StackAccess; }
    bool IsGlobalSymbol() const { return Type == GlobalSymbol; }

    static MachineOperand CreateRegister(uint64_t Reg, unsigned BitWidth = 32);
    static MachineOperand CreateImmediate(uint64_t Val, unsigned BitWidth = 32);
    static MachineOperand CreateVirtualRegister(uint64_t Reg, unsigned BitWidth = 32);
    static MachineOperand CreateMemory(uint64_t Id);
    static MachineOperand CreateStackAccess(uint64_t Slot, int Offset = 0);
    static MachineOperand CreateParameter(uint64_t Val);
    static MachineOperand CreateLabel(const char *Label);
    static MachineOperand CreateFunctionName(const char *Label);
    static MachineOperand CreateGlobalSymbol(std::string &Symbol);

    void Print(TargetMachine *TM) const;

  private:
    unsigned Type {None};
    int Offset                = 0;
    uint64_t Value            = ~0;
    const char *BelongToLabel = nullptr;
    LowLevelType LLT;
    std::string GlobalSym;
};
