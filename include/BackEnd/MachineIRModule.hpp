#pragma once

#include <vector>
#include "BackEnd/GlobalData.hpp"
#include "BackEnd/MachineFunction.hpp"

class MachineIRModule
{
    using FunctionList = std::vector<MachineFunction>;

  public:
    MachineIRModule() {}

    void AddFunction(MachineFunction &F) { Functions.push_back(F); }
    FunctionList &GetFunctions() { return Functions; }
    MachineFunction *GetCurrentFunction() { return &Functions[Functions.size() - 1]; }

    void Print() const
    {
        for (auto &F : Functions)
            F.Print();
    }

  private:
    std::vector<GlobalData> GlobalVars;
    FunctionList Functions;
};
