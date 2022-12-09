//
// Created by Organ1sm.
//
#pragma once


#include <vector>
#include <cassert>
#include <memory>

class BasicBlock;
class Function;
class IRType;
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

    std::vector<std::unique_ptr<Value>> &GetGlobalVars() { return GlobalVars; }

    void AddGlobalVar(std::unique_ptr<Value> GV);
    bool IsGlobalVar(Value *V) const;

    Value *GetGlobalVar(const std::string &Name) const;

    void Print(bool showColor = false) const;

  private:
    std::vector<IRType> StructTypes;
    std::vector<std::unique_ptr<Value>> GlobalVars;
    std::vector<Function> Functions;
};
