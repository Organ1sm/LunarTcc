//
// Created by Organ1sm.
//

#ifndef LUNARTCC_MODULE_HPP
#define LUNARTCC_MODULE_HPP

#include <vector>
#include <cassert>
#include <memory>

class BasicBlock;
class Function;
class Value;

class Module
{
  public:
    Module() = default;

    BasicBlock *CurrentBB();
    BasicBlock *GetBB(const std::size_t Index);
    BasicBlock *CreateBasicBlock();

    Function *CurrentFunction();
    void AddFunction(Function F);
    std::vector<Function> &GetFunctions() { return Functions; }

    void AddGlobalVar(std::unique_ptr<Value> GV);
    bool IsGlobalVar(Value *V) const;

    Value *GetGlobalVar(const std::string &Name) const;

    void Print() const;

  private:
    std::vector<std::unique_ptr<Value>> GlobalVars;
    std::vector<Function> Functions;
};

#endif    // LUNARTCC_MODULE_HPP
