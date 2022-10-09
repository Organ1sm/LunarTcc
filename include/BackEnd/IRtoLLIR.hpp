#pragma once


#include "MiddleEnd/IR/Module.hpp"
#include "MachineIRModule.hpp"


class IRtoLLIR
{
  public:
    IRtoLLIR(Module &IRModule, MachineIRModule *TranslUnit)
        : IRM(IRModule), TU(TranslUnit)
    {}

    void GenerateLLIRFromIR();

    MachineIRModule *GetMachineIRModule() { return TU; }

  private:
    Module &IRM;
    MachineIRModule *TU;
};
