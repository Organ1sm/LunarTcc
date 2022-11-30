#pragma once

class TargetMachine;
class MachineIRModule;


class RegisterClassSelection
{
  public:
    RegisterClassSelection(MachineIRModule *Input, TargetMachine *Target)
        : MIRM(Input), TM(Target)
    {}

    void Run();

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;
};
