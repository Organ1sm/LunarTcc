#pragma once

#include "MiddleEnd/Transforms/FunctionPass.hpp"

class Function;

class DeadCodeEliminationPass : public FunctionPass
{
    public:
       bool RunOnFunction(Function &F) override;
};
