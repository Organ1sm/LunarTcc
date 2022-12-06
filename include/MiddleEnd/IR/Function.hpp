//
// Created by Organ1sm.
//
#pragma once

#include <vector>
#include <memory>
#include "MiddleEnd/IR/BasicBlock.hpp"

class IRType;
class Value;
class BasicBlock;
class FunctionParameter;

class Function
{
    using BasicBlockList = std::vector<std::unique_ptr<BasicBlock>>;
    using ParameterList  = std::vector<std::unique_ptr<FunctionParameter>>;

  public:
    Function(const std::string &Name, IRType RT);

    Function(const Function &) = delete;
    Function(Function &&)      = default;

    BasicBlock *GetCurrentBB();
    BasicBlock *GetBB(std::size_t Index);

    std::string &GetName() { return Name; }

    BasicBlockList &GetBasicBlocks() { return BasicBlocks; }
    ParameterList &GetParameters() { return Parameters; }

    bool IsReturnTypeVoid() { return ReturnType.IsVoid(); }

    std::string &GetIgnorableStructVarName() { return IgnorableStructName; }
    void SetIgnorableStructName(std::string &Name) { IgnorableStructName = Name; }

    void SetToDeclarationOnly() { DeclarationOnly = true; }
    bool IsDeclarationOnly() const { return DeclarationOnly; }

    unsigned GetReturnNumber() const { return ReturnNumber; }
    void SetReturnNumber(unsigned N) { ReturnNumber = N; }

    /// If the function had multiple return value, then this field was set
    /// therefore it is also usable as a predicate
    bool HasMultipleReturn() const { return ReturnValue != nullptr; }

    Value *GetReturnValue() const { return ReturnValue; }
    void SetReturnValue(Value *V) { ReturnValue = V; }

    void CreateBasicBlock();

    void Insert(std::unique_ptr<BasicBlock> BB);
    void Insert(std::unique_ptr<FunctionParameter> FP);

    void Print() const;

  private:
    std::string Name;
    IRType ReturnType;
    ParameterList Parameters;
    BasicBlockList BasicBlocks;

    std::string IgnorableStructName {};
    bool DeclarationOnly {false};
    unsigned ReturnNumber {0};
    Value *ReturnValue {nullptr};
};
