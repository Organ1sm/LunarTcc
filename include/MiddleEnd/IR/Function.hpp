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
  public:
    Function(const std::string &Name, IRType RT);

    Function(const Function &) = delete;
    Function(Function &&)      = default;

    BasicBlock *GetCurrentBB();
    BasicBlock *GetBB(const std::size_t Index);

    void CreateBasicBlock();

    void Insert(std::unique_ptr<BasicBlock> BB);
    void Insert(std::unique_ptr<FunctionParameter> FP);

    void Print() const;

  private:
    std::string Name;
    IRType ReturnType;
    std::vector<std::unique_ptr<FunctionParameter>> Parameters;
    std::vector<std::unique_ptr<BasicBlock>> BasicBlocks;
};




#endif    // LUNARTCC_FUNCTION_HPP
