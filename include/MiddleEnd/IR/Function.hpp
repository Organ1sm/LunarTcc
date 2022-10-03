//
// Created by Organ1sm.
//

#ifndef LUNARTCC_FUNCTION_HPP
#define LUNARTCC_FUNCTION_HPP

#include <vector>
#include <memory>
#include "MiddleEnd/IR/IRType.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"

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

    BasicBlockList &GetBasicBlocks() { return BasicBlocks; }
    ParameterList &GetParameters() { return Parameters; }

    void CreateBasicBlock();

    void Insert(std::unique_ptr<BasicBlock> BB);
    void Insert(std::unique_ptr<FunctionParameter> FP);

    void Print() const;

  private:
    std::string Name;
    IRType ReturnType;
    ParameterList Parameters;
    BasicBlockList BasicBlocks;
};




#endif    // LUNARTCC_FUNCTION_HPP
