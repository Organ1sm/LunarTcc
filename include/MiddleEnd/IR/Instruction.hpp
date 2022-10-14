//
// Created by Organ1sm.
//

#ifndef LUNARTCC_INSTRUCTION_HPP
#define LUNARTCC_INSTRUCTION_HPP

#include <cassert>
#include <vector>
#include "MiddleEnd//IR/Value.hpp"

class BasicBlock;

class Instruction : public Value
{
  public:
    enum InstructionKind {
        // Arithmetic and Logical
        And,
        Or,
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        Cmp,

        // Conversions
        FloatToInt,
        IntToFloat,

        // Control flow Operations.
        Call,
        Jump,
        Branch,
        Ret,

        // Memory Operations.
        Load = Ret + 2,
        Store,
        StackAlloc
    };

    static std::string AsString(InstructionKind IK);

    Instruction(InstructionKind K, BasicBlock *P, IRType V)
        : InstKind(K), Parent(P), Value(V)
    {}

    InstructionKind GetInstructionKind() { return InstKind; }

    bool IsStackAllocation() const { return InstKind == StackAlloc; }
    bool IsTerminator() const { return BasicBlockTerminator; }

    virtual void Print() const { assert(!"Cannot print base class."); }

  protected:
    InstructionKind InstKind;
    BasicBlock *Parent {nullptr};
    bool BasicBlockTerminator {false};
};

class BinaryInstruction : public Instruction
{
  public:
    BinaryInstruction(InstructionKind IK, Value *L, Value *R, BasicBlock *P)
        : Instruction(IK, P, L->GetType()), LHS(L), RHS(R)
    {}

    Value *GetLHS() { return LHS; }
    Value *GetRHS() { return RHS; }

    void Print() const override;

  private:
    Value *LHS;
    Value *RHS;
};

class UnaryInstruction : public Instruction
{
  public:
    UnaryInstruction(InstructionKind UO, Value *Operand, BasicBlock *P)
        : Instruction(UO, P, Operand->GetType()), Op(Operand)
    {}

    UnaryInstruction(InstructionKind UO, IRType ResultType, Value *Operand, BasicBlock *P)
        : Instruction(UO, P, ResultType), Op(Operand)
    {}

    void Print() const override;

  private:
    Value *Op;
};

class CompareInstruction : public Instruction
{
  public:
    enum CompareRelation : unsigned {
        Invalid,
        EQ,
        NE,
        LT,
        GT,
        LE,
        GE
    };

    CompareInstruction(Value *L, Value *R, CompareRelation REL, BasicBlock *P)
        : Instruction(InstructionKind::Cmp, P, IRType(IRType::SInt, 1)), LHS(L), RHS(R),
          Relation(REL)
    {}

    const char *GetRelationString() const;
    void InvertRelation();

    Value *GetLHS() { return LHS; }
    Value *GetRHS() { return RHS; }
    unsigned GetRelation() { return Relation; }

    void Print() const override;

  private:
    CompareRelation Relation {Invalid};
    Value *LHS;
    Value *RHS;
};

class CallInstruction : public Instruction
{
  public:
    CallInstruction(const std::string &N, IRType T, BasicBlock *P)
        : Instruction(InstructionKind::Call, P, T), Name(N)
    {}

    CallInstruction(const std::string &N,
                    std::vector<Value *> &A,
                    IRType T,
                    BasicBlock *P)
        : Instruction(InstructionKind::Call, P, T), Name(N), Arguments(A)
    {}

    std::string &GetName() { return Name; }

    std::vector<Value *> &GetArgs() { return Arguments; }

    void Print() const override;

  private:
    std::string Name;
    std::vector<Value *> Arguments;
};

class JumpInstruction : public Instruction
{
  public:
    JumpInstruction(BasicBlock *D, BasicBlock *P)
        : Instruction(InstructionKind::Jump, P, IRType(IRType::None)), Target(D)
    {}

    void SetTargetBB(BasicBlock *T) { Target = T; }

    std::string &GetTargetLabelName();

    void Print() const override;

  private:
    BasicBlock *Target;
};

class BranchInstruction : public Instruction
{
  public:
    BranchInstruction(Value *C, BasicBlock *True, BasicBlock *False, BasicBlock *P)
        : Instruction(InstructionKind::Branch, P, IRType(IRType::None)), Condition(C),
          TrueTarget(True), FalseTarget(False)
    {}

    Value *GetCondition() { return Condition; }
    bool HasFalseLabel() { return FalseTarget != nullptr; }

    std::string &GetTrueLabelName();
    std::string &GetFalseLabelName();

    void Print() const override;

  private:
    Value *Condition;
    BasicBlock *TrueTarget;
    BasicBlock *FalseTarget;
};

class ReturnInstruction : public Instruction
{
  public:
    ReturnInstruction(Value *RV, BasicBlock *P)
        : Instruction(InstructionKind::Ret, P, RV->GetType()), ReturnVal(RV)
    {
        BasicBlockTerminator = true;
    }

    Value *GetRetVal() const { return ReturnVal; }

    void Print() const override;

  private:
    Value *ReturnVal;
};

class StackAllocationInstruction : public Instruction
{
  public:
    StackAllocationInstruction(std::string &S, IRType T, BasicBlock *P)
        : Instruction(InstructionKind::StackAlloc, P, T), VariableName(S)
    {}

    void Print() const override;

  private:
    std::string VariableName;
};

class StoreInstruction : public Instruction
{
  public:
    StoreInstruction(Value *S, Value *D, BasicBlock *P)
        : Instruction(InstructionKind::Store, P, IRType::None), Source(S), Destination(D)
    {}

    Value *GetMemoryLocation() { return Destination; }
    Value *GetSavedValue() { return Source; }

    void Print() const override;

  private:
    Value *Source;
    Value *Destination;
};

class LoadInstruction : public Instruction
{
  public:
    LoadInstruction(IRType T, Value *S, Value *O, BasicBlock *P)
        : Instruction(InstructionKind::Load, P, T), Source(S), Offset(O)
    {}

    LoadInstruction(IRType T, Value *S, BasicBlock *P)
        : Instruction(InstructionKind::Load, P, T), Source(S), Offset(nullptr)
    {}

    Value *GetMemoryLocation() { return Source; }

    void Print() const override;

  private:
    Value *Source;
    Value *Offset;
};

#endif    // LUNARTCC_INSTRUCTION_HPP
