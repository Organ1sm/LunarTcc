//
// Created by Organ1sm.
//
#pragma once

#include <vector>
#include <memory>
#include "MiddleEnd/IR/BasicBlock.hpp"

class IRType;
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
    BasicBlock *GetBB(const std::size_t Index);

    std::string &GetName() { return Name; }

    BasicBlockList &GetBasicBlocks() { return BasicBlocks; }
    ParameterList &GetParameters() { return Parameters; }

    bool IsReturnTypeVoid() { return ReturnType.IsVoid(); }

    std::string &GetIgnorableStructVarName() { return IgnorableStructName; }
    void SetIgnorableStructName(std::string &Name) { IgnorableStructName = Name; }

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
};
