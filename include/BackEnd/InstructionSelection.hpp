#pragma once

class TargetMachine;
class MachineIRModule;

class InstructionSelection
{
  public:
    InstructionSelection(MachineIRModule *Input, TargetMachine *Target)
        : MIRM(Input), TM(Target)
    {}

    void InstrSelect();

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;
};
