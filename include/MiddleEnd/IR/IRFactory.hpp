//
// Created by Organ1sm.
//
#pragma once

#include <cstdint>
#include <cstdlib>
#include <memory>
#include <map>
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"

class Value;
class Constant;
class FunctionParameter;

class Module;
class Function;
class BasicBlock;

class TargetMachine;

class IRFactory
{
    using IKind = Instruction::InstructionKind;

  public:
    IRFactory() = delete;
    IRFactory(Module &M, TargetMachine *T) : TM(T), CurrentModule(M), ID(0) {}

    Value *CreateAdd(Value *LHS, Value *RHS);
    Value *CreateSub(Value *LHS, Value *RHS);
    Value *CreateMul(Value *LHS, Value *RHS);
    Value *CreateDiv(Value *LHS, Value *RHS);
    Value *CreateMod(Value *LHS, Value *RHS);

    Value *CreateDivU(Value *LHS, Value *RHS);
    Value *CreateModU(Value *LHS, Value *RHS);

    Value *CreateAddF(Value *LHS, Value *RHS);
    Value *CreateSubF(Value *LHS, Value *RHS);
    Value *CreateMulF(Value *LHS, Value *RHS);
    Value *CreateDivF(Value *LHS, Value *RHS);

    Value *CreateLSL(Value *LHS, Value *RHS);
    Value *CreateLSR(Value *LHS, Value *RHS);

    Value *CreateOr(Value *LHS, Value *RHS);
    Value *CreateXOr(Value *LHS, Value *RHS);
    Value *CreateAnd(Value *LHS, Value *RHS);

    UnaryInstruction *CreateMov(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateMovF(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateSExt(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateZExt(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateTrunc(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateFloatToInt(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateIntToFloat(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateBitCast(Value *Operand, const IRType &To);

    CallInstruction *CreateCall(std::string &FuncName,
                                std::vector<Value *> Args,
                                const IRType &Type,
                                int StructIdx = -1);

    ReturnInstruction *CreateRet(Value *ReturnVal);

    StackAllocationInstruction *CreateSA(std::string Indentifier, const IRType &Type);

    GetElemPointerInstruction *
        CreateGEP(const IRType &ResultType, Value *Source, Value *Index);

    StoreInstruction *CreateStore(Value *Source, Value *Destination);

    LoadInstruction *
        CreateLoad(const IRType &ResultType, Value *Source, Value *Offset = nullptr);

    MemoryCopyInstruction *
        CreateMemCopy(Value *Destination, Value *Source, std::size_t Bytes);

    Value *
        CreateCmp(CompareInstruction::CompareRelation Relation, Value *LHS, Value *RHS);

    JumpInstruction *CreateJump(BasicBlock *Destination);

    BranchInstruction *
        CreateBranch(Value *Condition, BasicBlock *True, BasicBlock *False = nullptr);

    GlobalVariable *CreateGlobalVar(std::string &Identifier, const IRType &Type);
    GlobalVariable *CreateGlobalVar(std::string &Identifier,
                                    const IRType &Type,
                                    std::vector<uint64_t> InitList);
    GlobalVariable *
        CreateGlobalVar(std::string &Identifier, const IRType &Type, Value *Val);
    GlobalVariable *
        CreateGlobalVar(std::string &Identifier, const IRType &Type, std::string Value);

    void CreateNewFunction(std::string &Name, IRType ReturnType);
    void AddGlobalVariable(Value *GlobalValue);
    Value *GetGlobalVar(const std::string &Identifier);

    bool IsGlobalValue(Value *V) const;
    void SetGlobalScope(const bool v = true);
    bool IsGlobalScope() const;

    BasicBlock *CreateBasicBlock();
    Function *GetCurrentFunction();

    void InsertBB(std::unique_ptr<BasicBlock> BB);
    void Insert(std::unique_ptr<FunctionParameter> FP);

    void EraseLastBB();
    void EraseLastInst();

    void EraseInst(Instruction *I);

    void AddToSymbolTable(std::string &Identifier, Value *V);
    Value *GetSymbolValue(const std::string &Identifier);

    Constant *GetConstant(uint64_t C, uint8_t BW = 32);
    Constant *GetConstant(double C, uint8_t BW = 64);

    std::vector<BasicBlock *> &GetLoopIncrementBBsTable();
    std::vector<BasicBlock *> &GetBreakEndBBsTable();

    TargetMachine *GetTargetMachine() { return TM; }

  private:
    Value *CreateBinaryInstruction(IKind K, Value *L, Value *R);

    Value *EvaluateIntegerBinaryConstantExpression(const int64_t LHS,
                                                   const int64_t RHS,
                                                   const uint8_t BitWidth,
                                                   const IKind Operation);

    Value *EvaluateFloatingBinaryConstantExpression(const double LHS,
                                                    const double RHS,
                                                    const uint8_t BitWidth,
                                                    const IKind Operation);

    Value *EvaluateBinaryConstantExpression(const Constant *LHS,
                                            const Constant *RHS,
                                            const IKind Operation);

    BasicBlock *GetCurrentBB();

    Instruction *Insert(std::unique_ptr<Instruction> I);


  private:
    Module &CurrentModule;

    TargetMachine *TM {nullptr};

    /// A counter essentially, which used to give values a unique ID.
    unsigned ID;

    /// Shows whether we are in the global scope or not.
    bool GlobalScope {false};

    /// To store already created integer constants.
    /// <<Size, BitWidth>,...>
    std::map<std::pair<uint64_t, uint8_t>, std::unique_ptr<Constant>> IntConstantPool;

    /// To store already created floating point constants.
    std::map<uint64_t, std::unique_ptr<Constant>> FloatConstantPool;

    /// FixMe: Consider putting these to Function class.

    /// Hold the local symbols for the current function.
    std::map<std::string, Value *> SymbolTable;

    /// To keep track how many times each label were defined.
    /// This number can be used to concatenate it to the label
    /// to make it unique.
    std::map<std::string, unsigned> LabelTable;

    /// For context information for "continue" statements. Containing the pointer
    /// to the basic block which will be the target of the generated jump.
    std::vector<BasicBlock *> LoopIncrementBBsTable;

    /// For context information for "break" statements. Containing the pointer
    /// to the basic block which will be the target of the generated jump.
    std::vector<BasicBlock *> BreaksTargetBBsTable;
};
