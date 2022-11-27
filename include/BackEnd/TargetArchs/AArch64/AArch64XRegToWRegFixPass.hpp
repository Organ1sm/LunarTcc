#pragma once

class MachineIRModule;
class TargetMachine;

class AArch64XRegToWRegFixPass
{
  public:
    AArch64XRegToWRegFixPass(MachineIRModule *MIRM, TargetMachine *TM)
        : MIRM(MIRM), TM(TM)
    {}

    void Run();

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;
};
