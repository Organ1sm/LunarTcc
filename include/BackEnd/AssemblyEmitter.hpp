#pragma once

class TargetMachine;
class MachineIRModule;

class AssemblyEmitter
{
  public:
    AssemblyEmitter(MachineIRModule *Module, TargetMachine *TM) : TM(TM), MIRM(Module) {}

    void GenerateAssembly();

  private:
    TargetMachine *TM;
    MachineIRModule *MIRM;
};
