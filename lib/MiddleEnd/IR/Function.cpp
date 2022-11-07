//
// Created by Organ1sm.
//
#include <iostream>
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/IRType.hpp"
#include "fmt/color.h"
#include "fmt/core.h"

Function::Function(const std::string &Name, IRType RT) : Name(Name), ReturnType(RT)
{
    auto FinalName = std::string("entry_") + Name;
    auto BB        = std::make_unique<BasicBlock>(BasicBlock(FinalName, this));

    BasicBlocks.push_back(std::move(BB));
}

BasicBlock *Function::GetCurrentBB()
{
    assert(!BasicBlocks.empty() && "Function must have basic blocks.");
    return BasicBlocks.back().get();
}

BasicBlock *Function::GetBB(const std::size_t Index)
{
    assert(!BasicBlocks.empty() && "Function must have basic blocks.");
    assert(BasicBlocks.size() > Index && " Invalid index.");

    return BasicBlocks[Index].get();
}

void Function::CreateBasicBlock()
{
    auto BB = std::make_unique<BasicBlock>(BasicBlock(this));
    BasicBlocks.push_back(std::move(BB));
}

void Function::Insert(std::unique_ptr<BasicBlock> BB)
{
    BasicBlocks.push_back(std::move(BB));
}

void Function::Insert(std::unique_ptr<FunctionParameter> FP)
{
    Parameters.push_back(std::move(FP));
}

void Function::Print() const
{
    auto size         = Parameters.size();
    bool HasParameter = !((size == 1) && (Parameters[0]->GetType().IsVoid()));

    auto FDStr = "func {n} ({a}) -> {r}";
    std::string ArgsStr {""};
    std::string ReturnTyStr {""};

    if (HasParameter)
    {
        for (auto i = 0; i < size; i++)
        {
            ArgsStr += Parameters[i]->ValueString() + " :";
            ArgsStr += Parameters[i]->GetType().AsString();

            if (i + 1 < size)
                ArgsStr += ", ";
        }
    }

    if (!ReturnType.IsVoid())
        ReturnTyStr = ReturnType.AsString();

    if (DeclarationOnly)
        fmt::print("declare ");


    fmt::print(FDStr,
               fmt::arg("n", Name),
               fmt::arg("a", ArgsStr),
               fmt::arg("r", ReturnTyStr));

    fmt::print("{}\n", DeclarationOnly ? ";\n" : ":");

    if (!DeclarationOnly)
        for (auto &BB : BasicBlocks)
            BB->Print();
}
