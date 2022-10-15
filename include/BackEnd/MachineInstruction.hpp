#pragma once

#include <cstdint>
#include <vector>
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/TargetInstructionLegalizer.hpp"


class MachineBasicBlock;

class MachineInstruction
{
    using OperandList = std::vector<MachineOperand>;

  public:
    enum OperationCode : unsigned {
        // Arithmetic and Logical
        And = 1 << 16,    // 65536
        Or,               // 65537
        Add,              // 65538
        Sub,              // 65539
        Mul,              // 65540
        Div,              // 65541
        Mod,              // 65542
        Cmp,              // 65543

        // Conversions
        SExt,          // Sign extension
        ZExt,          // Zero extension
        Trunc,         // Truncating
        FloatToInt,    // 65544
        IntToFloat,    // 65545

        // Control Flow Operations
        Call,      // 65546
        Jump,      // 65547
        Branch,    // 65548
        Ret,       // 65549

        // Moves and constant materializations
        LoadImm,

        // Memory Access
        Load,
        Store,
        StackAlloc,
    };

    enum CmpRelation {
        Invalid,
        EQ,
        NE,
        LT,
        GT,
        LE,
        GE
    };

    enum Flags : unsigned {
        IsLOAD  = 1,
        IsSTORE = 1 << 1
    };

    MachineInstruction() {}
    MachineInstruction(unsigned Opcode, MachineBasicBlock *Parent)
        : Opcode(Opcode), Parent(Parent)
    {}

    unsigned GetOpcode() const { return Opcode; }
    void SetOpcode(unsigned Opcode) { this->Opcode = Opcode; }

    std::size_t GetOperandsNumber() const { return Operands.size(); }

    MachineOperand *GetOperand(std::size_t Index) { return &Operands[Index]; }
    OperandList &GetOperands() { return Operands; }

    void AddOperand(MachineOperand MO) { Operands.push_back(MO); }

    void AddAttribute(unsigned AttributeFlag) { OtherAttributes |= AttributeFlag; }
    void SetAttributes(unsigned A) { Attributes = A; }
    unsigned GetRelation() const { return Attributes; }

    MachineBasicBlock *GetParent() const { return Parent; }
    void SetParent(MachineBasicBlock *BB) { Parent = BB; }

    void RemoveOperand(std::size_t Index);
    void InsertOperand(std::size_t Index, MachineOperand Operand);

    void RemoveMemOperand();

    void AddRegister(uint64_t Reg);
    void AddImmediate(uint64_t Num);
    void AddMemory(uint64_t Id);
    void AddStackAccess(uint64_t Slot, unsigned Size = 4);
    void AddLabel(const char *Label);

    bool IsFallThroughBranch() const { return Operands.size() == 2; }
    bool IsLoad() const { return Opcode == Load || (OtherAttributes & IsLOAD); }
    bool IsStore() const { return Opcode == Store || (OtherAttributes & IsSTORE); }
    bool IsLoadOrStore() const { return IsLoad() || IsStore(); }

    void Print() const;

  private:
    unsigned Opcode = 0;

    // Capture things like the relation for compare instructions
    unsigned Attributes      = 0;
    unsigned OtherAttributes = 0;

    OperandList Operands;
    MachineBasicBlock *Parent {nullptr};
};
