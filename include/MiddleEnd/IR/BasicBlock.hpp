//
// Created by Organ1sm.
//

#ifndef LUNARTCC_BASICBLOCK_HPP
#define LUNARTCC_BASICBLOCK_HPP

#include <memory>
#include "MiddleEnd/IR/Value.hpp"
#include "MiddleEnd/IR/Instruction.hpp"

class Function;

class BasicBlock : public Value
{
  public:
    using InstructionList = std::vector<std::unique_ptr<Instruction>>;

    BasicBlock(std::string Name, Function *Parent)
        : Name(Name), Parent(Parent), Value(Value::Label)
    {}

    BasicBlock(Function *P) : Parent(P), Value(Value::Label) {}

    BasicBlock(const BasicBlock &) = delete;
    BasicBlock(BasicBlock &&)      = default;

    /// Insert the @Instruction to the back of the Instructions vector.
    Instruction *Insert(std::unique_ptr<Instruction> Ins);

    /// Inserting a StackAllocationInstruction into the entry BasicBlock. It will
    /// be Inserted before the first none SA instruction. Or into the end of the
    /// list if the Instruction list is either empty or contains only SA
    /// instructions.
    Instruction *InsertSA(std::unique_ptr<Instruction> Ins);

    std::string &GetName() { return Name; }
    void SetName(const std::string &N) { Name = N; }

    InstructionList &GetInstructions() { return Instructions; }

    void Print(bool ShowColor = false) const;

  private:
    std::string Name;
    InstructionList Instructions;
    Function *Parent;
};

#endif    // LUNARTCC_BASICBLOCK_HPP
