//
// Created by Organ1sm.
//

#include <iostream>
#include <fmt/core.h>
#include "MiddleEnd/IR/BasicBlock.hpp"

Instruction *BasicBlock::Insert(std::unique_ptr<Instruction> Ins)
{
    Instructions.push_back(std::move(Ins));
    return Instructions.back().get();
}

Instruction *BasicBlock::InsertSA(std::unique_ptr<Instruction> Ins)
{
    // Edge case: The BB is empty -> Just use Insert
    if (Instructions.empty())
        return Insert(std::move(Ins));

    for (auto i = 0; i < Instructions.size(); i++)
    {
        if (!Instructions[i]->IsStackAllocation())
        {
            auto InstPtr = Ins.get();
            Instructions.insert(Instructions.begin() + i, std::move(Ins));
            return InstPtr;
        }
    }
    // Edge case: Every instruction were stack allocations -> Just use Insert
    return Insert(std::move(Ins));
}

void BasicBlock::Print() const
{
    fmt::print(".{}:\n", Name);

    for (auto &Instruction : Instructions)
        Instruction->Print();

    fmt::print("\n\n");
}
