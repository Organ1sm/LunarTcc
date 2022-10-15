#pragma once

#include <cstdint>
class TargetMachine;
class MachineIRModule;
class MachineFunction;
class MachineInstruction;

class PrologueEpilogInsertion
{
  public:
    PrologueEpilogInsertion(MachineIRModule *Module, TargetMachine *TM)
        : MIRM(Module), TM(TM)
    {}

    void Run();

    MachineInstruction CreateAddInstruction(int64_t StackAdjustmentSize);
    MachineInstruction CreateLoadInstruction(unsigned Source, int64_t Offset);

    void InsertStackAdjustmentUpward(MachineFunction &Func);
    void InsertStackAdjustmentDownward(MachineFunction &Func);

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;
};
