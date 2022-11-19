//
// Created by Organ1sm.
//

#pragma once

#include <cassert>
#include <vector>
#include "MiddleEnd//IR/Value.hpp"

class BasicBlock;
class IRType;

class Instruction : public Value
{
  public:
    enum InstructionKind {
        // Arithmetic and Logical
        And,
        Or,
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
        Cmp,

        // Conversions
        SExt,     // Sign extension
        ZExt,     // Zero extension
        Trunc,    // Truncating
        FloatToInt,
        IntToFloat,

        // Control flow Operations.
        Call,
        Jump,
        Branch,
        Ret,

        Mov,    // 58

        // Memory Operations.
        Load = Ret + 3,
        Store,
        MemCopy,
        StackAlloc,
        GetELemPtr,
    };

    static std::string AsString(InstructionKind IK);

    Instruction(InstructionKind K, BasicBlock *P, IRType V)
        : InstKind(K), Parent(P), Value(V)
    {}

    InstructionKind GetInstructionKind() { return InstKind; }

    bool IsStackAllocation() const { return InstKind == StackAlloc; }
    bool IsTerminator() const { return BasicBlockTerminator; }

    virtual void Print() const { assert(!"Cannot print base class."); }
    virtual void PrintInst(const std::string Format = "") const;

  protected:
    InstructionKind InstKind;
    BasicBlock *Parent {nullptr};
    bool BasicBlockTerminator {false};
    std::string InstFormat {"{:4}{:<10}"};
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

    Value *GetOperand() { return Op; }

    void Print() const override;

  private:
    Value *Op;
};

class CompareInstruction : public Instruction
{
  public:
    enum CompareRelation : unsigned { Invalid, EQ, NE, LT, GT, LE, GE };

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
                    BasicBlock *P,
                    int StructIdx)
        : Instruction(InstructionKind::Call, P, T), Name(N), Arguments(A),
          ImplicitStructArgIndex(StructIdx)
    {}

    std::string &GetName() { return Name; }
    std::vector<Value *> &GetArgs() { return Arguments; }
    int GetImplicitStructArgIndex() const { return ImplicitStructArgIndex; }

    void Print() const override;

  private:
    int ImplicitStructArgIndex = -1;
    std::string Name;
    std::vector<Value *> Arguments;
};

class JumpInstruction : public Instruction
{
  public:
    JumpInstruction(BasicBlock *D, BasicBlock *P)
        : Instruction(InstructionKind::Jump, P, IRType(IRType::None)), Target(D)
    {}

    BasicBlock *GetTargetBB() { return Target; }
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
        : Instruction(InstructionKind::Ret, P, RV ? RV->GetType() : IRType::None),
          ReturnVal(RV)
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
    {
        this->GetTypeRef().SetPointerLevel(this->GetTypeRef().GetPointerLevel() + 1);
    }

    void Print() const override;

  private:
    std::string VariableName;
};

class GetElemPointerInstruction : public Instruction
{
  public:
    GetElemPointerInstruction(IRType T,
                              Value *CompositeObject,
                              Value *AccessIndex,
                              BasicBlock *P)
        : Instruction(Instruction::GetELemPtr, P, T), Source(CompositeObject),
          Index(AccessIndex)
    {}

    Value *GetSource() const { return Source; }
    Value *GetIndex() { return Index; }

    void Print() const override;

  private:
    Value *Source;
    Value *Index;
};

class StoreInstruction : public Instruction
{
  public:
    StoreInstruction(Value *S, Value *D, BasicBlock *P)
        : Instruction(InstructionKind::Store, P, IRType::None), Source(S), Destination(D)
    {
        assert(Source && Destination);
    }

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
    {
        ConstructorHelper();
    }

    LoadInstruction(IRType T, Value *S, BasicBlock *P)
        : Instruction(InstructionKind::Load, P, T), Source(S), Offset(nullptr)
    {
        ConstructorHelper();
    }

    void ConstructorHelper();
    Value *GetMemoryLocation() { return Source; }

    void Print() const override;

  private:
    Value *Source;
    Value *Offset;
};

class MemoryCopyInstruction : public Instruction
{
  public:
    MemoryCopyInstruction(Value *Destination,
                          Value *Source,
                          std::size_t Bytes,
                          BasicBlock *P)
        : Instruction(Instruction::MemCopy, P, IRType()), Dest(Destination),
          Source(Source), N(Bytes)
    {}

    Value *GetDestination() { return Dest; }
    Value *GetSource() { return Source; }
    std::size_t GetSize() const { return N; }

    void Print() const;

  private:
    Value *Dest;
    Value *Source;
    std::size_t N;
};
