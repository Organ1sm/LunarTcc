#pragma once

#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/TargetMachine.hpp"

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
