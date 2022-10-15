#pragma once

class Module;
class MachineIRModule;

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
