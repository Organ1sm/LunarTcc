#pragma once

#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/TargetMachine.hpp"

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
