#pragma once

class MachineIRModule;
class TargetMachine;

class AArch64MovFixPass
{
  public:
    AArch64MovFixPass(MachineIRModule *MIRM, TargetMachine *TM) : MIRM(MIRM), TM(TM) {}

    void Run();

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;
};
