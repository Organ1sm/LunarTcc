#pragma once

#include <cstdint>
#include <vector>
#include "BackEnd/MachineOperand.hpp"

class MachineBasicBlock;
class TargetMachine;

class MachineInstruction
{
    using OperandList = std::vector<MachineOperand>;

  public:
    enum OperationCode : unsigned {
        // Integer Arithmetic and Logical
        And = 1 << 16,    // 65536
        Or,               // 65537
        XOr,
        LSL,
        LSR,
        Add,
        Sub,
        Mul,
        Div,
        DivU,
        Mod,
        ModU,
        Cmp,    // 65548

        // Floating point Arithmetic and Logical
        AddF,
        SubF,
        MulF,
        DivF,
        CmpF,

        // Conversions
        SExt,     // Sign extension
        ZExt,     // Zero extension
        Trunc,    // Truncating
        FloatToInt,
        IntToFloat,
        BitCast,

        // Control Flow Operations
        Call,
        Jump,
        Branch,
        Ret,

        // Moves and constant materializations
        LoadImm,
        Mov,
        MovF,

        // Memory Access
        Load,
        Store,
        StackAlloc,
        StackAddress,
        GlobalAddress,

        // Combined load and sign/zero extension
        SExtLoad,
        ZExtLoad,
        InvalidOp,
    };

    enum CmpRelation { Invalid, EQ, NE, LT, GT, LE, GE };

    enum Flags : unsigned {
        IsLOAD     = 1,
        IsSTORE    = 1 << 1,
        IsEXPANDED = 1 << 2,
        IsRETURN   = 1 << 3,
        IsJUMP     = 1 << 4,
        IsCALL     = 1 << 5,
    };

    MachineInstruction() {}
    MachineInstruction(unsigned Opcode, MachineBasicBlock *Parent)
        : Opcode(Opcode), Parent(Parent)
    {
        UpdateAttributes();
    }

    void UpdateAttributes();

    unsigned GetOpcode() const { return Opcode; }
    void SetOpcode(unsigned Opcode);

    std::size_t GetOperandsNumber() const { return Operands.size(); }

    MachineOperand *GetOperand(std::size_t Index);
    OperandList &GetOperands() { return Operands; }

    void AddOperand(MachineOperand MO) { Operands.push_back(MO); }
    void ReplaceOperand(MachineOperand MO, std::size_t Index);

    void AddAttribute(unsigned AttributeFlag) { OtherAttributes |= AttributeFlag; }
    void SetAttributes(unsigned A) { Attributes = A; }
    unsigned GetRelation() const { return Attributes; }

    MachineBasicBlock *GetParent() const { return Parent; }
    void SetParent(MachineBasicBlock *BB) { Parent = BB; }

    void RemoveOperand(std::size_t Index);
    void InsertOperand(std::size_t Index, MachineOperand Operand);

    void RemoveMemOperand();
    MachineOperand *GetDefine();

    MachineOperand *GetNthUse(std::size_t N);
    void SetNthUse(std::size_t N, MachineOperand *Use);

    void AddRegister(uint64_t Reg, unsigned BitWidth = 32);
    void AddVirtualRegister(uint64_t Reg, unsigned BitWidth = 32);
    void AddImmediate(uint64_t Num, unsigned BitWidth = 32);
    void AddMemory(uint64_t Id, unsigned BitWidth = 32);
    void AddMemory(uint64_t Id, int Offset, unsigned BitWidth);
    void AddStackAccess(uint64_t Slot, unsigned Offset = 0);
    void AddLabel(const char *Label);
    void AddFunctionName(const char *Name);
    void AddGlobalSymbol(std::string Symbol);

    bool IsFallThroughBranch() const { return Operands.size() == 2; }
    bool IsLoad() const { return Opcode == Load || (OtherAttributes & IsLOAD); }
    bool IsStore() const { return Opcode == Store || (OtherAttributes & IsSTORE); }
    bool IsLoadOrStore() const { return IsLoad() || IsStore(); }
    bool IsInvalid() const { return Opcode == InvalidOp; }
    bool IsReturn() const { return OtherAttributes & IsRETURN; }
    bool IsCall() const { return OtherAttributes & IsCALL; }
    bool IsJump() const { return OtherAttributes & IsJUMP; }
    bool IsDefine() { return GetDefine(); }

    bool IsThreeAddrArithmetic() const { return Opcode >= Add && Opcode <= CmpF; }

    bool IsAlreadySelected() const { return Opcode < 65536; }
    bool IsAlreadyExpanded() const { return OtherAttributes & IsEXPANDED; }

    /// flag the MachineInstruction as expanded, so the legalizer can ignore it
    void FlagAsExpanded() { OtherAttributes |= IsEXPANDED; }

    const char *GetRelationString() const;

    void Print(TargetMachine *TM) const;

  private:
    unsigned Opcode {InvalidOp};

    // Capture things like the relation for compare instructions
    unsigned Attributes      = 0;
    unsigned OtherAttributes = 0;

    OperandList Operands;
    MachineBasicBlock *Parent {nullptr};
};
