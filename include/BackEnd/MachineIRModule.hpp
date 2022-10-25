#pragma once

#include <vector>
#include "BackEnd/GlobalData.hpp"
#include "BackEnd/MachineFunction.hpp"

class MachineIRModule
{
    using FunctionList = std::vector<MachineFunction>;
    using GlobalsList  = std::vector<GlobalData>;

  public:
    MachineIRModule() {}

    void AddFunction(MachineFunction &F) { Functions.push_back(F); }
    FunctionList &GetFunctions() { return Functions; }
    MachineFunction *GetCurrentFunction() { return &Functions[Functions.size() - 1]; }

    GlobalsList &GetGlobalDatas() { return GlobalVars; }
    void AddGlobalData(GlobalData &GD) { GlobalVars.push_back(GD); }

    void Print(TargetMachine *TM) const
    {
        for (auto &F : Functions)
            F.Print(TM);
    }

  private:
    GlobalsList GlobalVars;
    FunctionList Functions;
};
