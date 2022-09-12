//
// Created by Organ1sm.
//

#include "MiddleEnd/IR/Module.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"


BasicBlock *Module::CurrentBB()
{
    assert(!Functions.empty() && "Module must have functions.");
    return Functions.back().GetCurrentBB();
}

BasicBlock *Module::GetBB(const std::size_t Index)
{
    assert(!Functions.empty() && "Module must have functions.");
    return Functions.back().GetBB(Index);
}

BasicBlock *Module::CreateBasicBlock()
{
    assert(!Functions.empty() && "Module must have functions.");
    Functions.back().CreateBasicBlock();
    return CurrentBB();
}

Function *Module::CurrentFunction()
{
    assert(!Functions.empty() && "Module must have functions.");
    return &Functions.back();
}

void Module::AddFunction(Function F) { Functions.push_back(std::move(F)); }

void Module::AddGlobalVar(std::unique_ptr<Value> GV)
{
    assert(GV && "Cannot be a nullptr");
    GlobalVars.push_back(std::move(GV));
}

bool Module::IsGlobalVar(Value *V) const
{
    for (auto &GV : GlobalVars)
    {
        if (GV.get() == V)
            return true;
    }

    return false;
}

Value *Module::GetGlobalVar(const std::string &Name) const
{
    for (auto &GV : GlobalVars)
    {
        if (GV->ValueString() == Name)
            return GV.get();
    }

    return nullptr;
}

void Module::Print() const
{
    for (auto &GlobalVar : GlobalVars)
        dynamic_cast<GlobalVariable *>(GlobalVar.get())->Print();

    for (auto &Function : Functions)
        Function.Print();
}
