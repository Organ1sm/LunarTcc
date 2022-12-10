//
// Created by Organ1sm.
//

#pragma once

#include <algorithm>
#include <cassert>
#include <vector>
#include "MiddleEnd//IR/Value.hpp"

class BasicBlock;
class IRType;

class Instruction : public Value
{
  public:
    enum InstructionKind {
        // Integer Arithmetic and Logical
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

        // Control flow Operations.
        Call,
        Jump,
        Branch,
        Ret,

        // Memory Operations.
        Load = Ret + 4,
        Store,
        MemCopy,
        StackAlloc,
        GetELemPtr,
    };

    static std::string AsString(InstructionKind IK);

    Instruction(InstructionKind K, BasicBlock *P, IRType V)
        : InstKind(K), Parent(P), Value(std::move(V))
    {}

    InstructionKind GetInstructionKind() { return InstKind; }

    bool IsStackAllocation() const { return InstKind == StackAlloc; }
    bool IsTerminator() const { return BasicBlockTerminator; }

    virtual void Print(bool ShowColor = false) const
    {
        assert(!"Cannot print base class.");
    }
    virtual void PrintInst(bool ShowColor, const std::string Format = "") const;

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

    void Print(bool ShowColor = false) const override;

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
        : Instruction(UO, P, std::move(ResultType)), Op(Operand)
    {}

    Value *GetOperand() { return Op; }

    void Print(bool ShowColor = false) const override;

  private:
    Value *Op;
};

class CompareInstruction : public Instruction
{
  public:
    enum CompareRelation : unsigned { Invalid, EQ, NE, LT, GT, LE, GE };

    CompareInstruction(Value *L, Value *R, CompareRelation REL, BasicBlock *P)
        : Instruction(L->IsFPType() && R->IsFPType() ? InstructionKind::CmpF :
                                                       InstructionKind::Cmp,
                      P,
                      IRType(IRType::SInt, 1)),
          LHS(L), RHS(R), Relation(REL)
    {}

    const char *GetRelationString() const;
    void InvertRelation();

    Value *GetLHS() { return LHS; }
    Value *GetRHS() { return RHS; }
    unsigned GetRelation() { return Relation; }

    void Print(bool ShowColor = false) const override;

  private:
    CompareRelation Relation {Invalid};
    Value *LHS;
    Value *RHS;
};

class CallInstruction : public Instruction
{
  public:
    CallInstruction(std::string N, IRType T, BasicBlock *P)
        : Instruction(InstructionKind::Call, P, std::move(T)), Name(std::move(N))
    {}

    // clang-format off
    CallInstruction(std::string N,
                    std::vector<Value *> &A,
                    IRType T,
                    BasicBlock *P,
                    int StructIdx)
        : Instruction(InstructionKind::Call, P, T), Name(std::move(N)), Arguments(A),
          ImplicitStructArgIndex(StructIdx)
    {}
    // clang-format on

    std::string &GetName() { return Name; }
    std::vector<Value *> &GetArgs() { return Arguments; }
    int GetImplicitStructArgIndex() const { return ImplicitStructArgIndex; }

    void Print(bool ShowColor = false) const override;

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

    void Print(bool ShowColor = false) const override;

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

    void Print(bool ShowColor = false) const override;

  private:
    Value *Condition;
    BasicBlock *TrueTarget;
    BasicBlock *FalseTarget;
};

class ReturnInstruction : public Instruction
{
  public:
    ReturnInstruction(Value *RV, BasicBlock *P)
        : Instruction(InstructionKind::Ret, P, RV ? RV->GetType() : IRType(IRType::None)),
          ReturnVal(RV)
    {
        BasicBlockTerminator = true;
    }

    Value *GetRetVal() const { return ReturnVal; }

    void Print(bool ShowColor = false) const override;

  private:
    Value *ReturnVal;
};

class StackAllocationInstruction : public Instruction
{
  public:
    StackAllocationInstruction(std::string &S, IRType T, BasicBlock *P)
        : Instruction(InstructionKind::StackAlloc, P, std::move(T)), VariableName(S)
    {
        this->GetTypeRef().SetPointerLevel(this->GetTypeRef().GetPointerLevel() + 1);
    }

    void Print(bool ShowColor = false) const override;

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
        : Instruction(Instruction::GetELemPtr, P, std::move(T)), Source(CompositeObject),
          Index(AccessIndex)
    {}

    Value *GetSource() const { return Source; }
    Value *GetIndex() { return Index; }

    void Print(bool ShowColor = false) const override;

  private:
    Value *Source;
    Value *Index;
};

class StoreInstruction : public Instruction
{
  public:
    StoreInstruction(Value *S, Value *D, BasicBlock *P)
        : Instruction(InstructionKind::Store, P, IRType(IRType::None)), Source(S),
          Destination(D)
    {
        assert(Source && Destination);
    }

    Value *GetMemoryLocation() { return Destination; }
    Value *GetSavedValue() { return Source; }

    void Print(bool ShowColor = false) const override;

  private:
    Value *Source;
    Value *Destination;
};

class LoadInstruction : public Instruction
{
  public:
    LoadInstruction(IRType T, Value *S, Value *O, BasicBlock *P)
        : Instruction(InstructionKind::Load, P, std::move(T)), Source(S), Offset(O)
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

    void Print(bool ShowColor = false) const override;

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

    void Print(bool ShowColor = false) const;

  private:
    Value *Dest;
    Value *Source;
    std::size_t N;
};
