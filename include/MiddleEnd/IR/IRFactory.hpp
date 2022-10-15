//
// Created by Organ1sm.
//

#ifndef LUNARTCC_IRFACTORY_HPP
#define LUNARTCC_IRFACTORY_HPP

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

class IRFactory
{
  public:
    IRFactory() = delete;

    IRFactory(Module &M) : CurrentModule(M), ID(0) {}

    BinaryInstruction *CreateAdd(Value *LHS, Value *RHS);
    BinaryInstruction *CreateSub(Value *LHS, Value *RHS);
    BinaryInstruction *CreateMul(Value *LHS, Value *RHS);
    BinaryInstruction *CreateDiv(Value *LHS, Value *RHS);
    BinaryInstruction *CreateMod(Value *LHS, Value *RHS);

    BinaryInstruction *CreateOr(Value *LHS, Value *RHS);
    BinaryInstruction *CreateAnd(Value *LHS, Value *RHS);

    UnaryInstruction *CreateSExt(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateTrunc(Value *Operand, uint8_t BitWidth = 32);
    UnaryInstruction *CreateFloatToInt(Value *Operand, uint8_t FloatBitWidth = 32);
    UnaryInstruction *CreateIntToFloat(Value *Operand, uint8_t IntBitWidth = 32);

    CallInstruction *
        CreateCall(std::string &FuncName, std::vector<Value *> Args, IRType Type);

    ReturnInstruction *CreateRet(Value *ReturnVal);

    StackAllocationInstruction *CreateSA(std::string Indentifier, IRType Type);

    StoreInstruction *CreateStore(Value *Source, Value *Destination);

    LoadInstruction *
        CreateLoad(IRType ResultType, Value *Source, Value *Offset = nullptr);

    CompareInstruction *
        CreateCmp(CompareInstruction::CompareRelation Relation, Value *LHS, Value *RHS);

    JumpInstruction *CreateJump(BasicBlock *Destination);

    BranchInstruction *
        CreateBranch(Value *Condition, BasicBlock *True, BasicBlock *False = nullptr);

    GlobalVariable *CreateGlobalVar(std::string &Identifier, IRType Type);

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

    void AddToSymbolTable(std::string &Identifier, Value *V);
    Value *GetSymbolValue(const std::string &Identifier);

    Constant *GetConstant(uint64_t C);
    Constant *GetConstant(double C);

  private:
    BinaryInstruction *
        CreateBinaryInstruction(Instruction::InstructionKind K, Value *L, Value *R);

    BasicBlock *GetCurrentBB();

    Instruction *Insert(std::unique_ptr<Instruction> I);


  private:
    Module &CurrentModule;

    /// A counter essentially, which used to give values a unique ID.
    unsigned ID;

    /// Shows whether we are in the global scope or not.
    bool GlobalScope {false};

    /// To store already created integer constants.
    std::map<uint64_t, std::unique_ptr<Constant>> IntConstantPool;

    /// To store already created floating point constants.
    std::map<uint64_t, std::unique_ptr<Constant>> FloatConstantPool;

    /// FixMe: Consider putting these to Function class.

    /// Hold the local symbols for the current function.
    std::map<std::string, Value *> SymbolTable;

    /// To keep track how many times each label were defined.
    /// This number can be used to concatenate it to the label
    /// to make it unique.
    std::map<std::string, unsigned> LabelTable;
};


#endif    // LUNARTCC_IRFACTORY_HPP
