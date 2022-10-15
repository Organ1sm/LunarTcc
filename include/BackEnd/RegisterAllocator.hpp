#pragma once

class MachineIRModule;
class TargetMachine;

class RegisterAllocator
{
  public:
    RegisterAllocator(MachineIRModule *Module, TargetMachine *TM) : MIRM(Module), TM(TM)
    {}

    void RunRA();

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;
};
