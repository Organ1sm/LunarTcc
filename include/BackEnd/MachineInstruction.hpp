#pragma once

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
        FloatToInt,    // 65544
        IntToFloat,    // 65545

        // Control Flow Operations
        Call,      // 65546
        Jump,      // 65547
        Branch,    // 65548
        Ret,       // 65549

        // Memory Access
        Load,         // 65550
        Store,        // 65551
        StackAlloc    // 65552
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

    void SetAttributes(unsigned A) { OtherAttributes = A; }
    unsigned GetRelation() const { return OtherAttributes; }


    MachineBasicBlock *GetParent() const { return Parent; }
    void SetParent(MachineBasicBlock *BB) { Parent = BB; }

    void RemoveOperand(std::size_t Index);
    void InsertOperand(std::size_t Index, MachineOperand Operand);

    void RemoveMemOperand();

    bool IsFallThroughBranch() const { return Operands.size() == 2; }
    bool IsLoad() const { return Opcode == Load; }
    bool IsStore() const { return Opcode == Store; }
    bool IsLoadOrStore() const { return Opcode == Load || Opcode == Store; }

  private:
    unsigned Opcode = 0;
    //
    // Capture things like the relation for compare instructions
    unsigned OtherAttributes = 0;

    OperandList Operands;
    MachineBasicBlock *Parent {nullptr};
};
