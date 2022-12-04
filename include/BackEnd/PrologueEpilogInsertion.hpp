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

    void InsertLinkRegisterSave(MachineFunction &Func);
    void InsertLinkRegisterReload(MachineFunction &Func);

    void InsertStackAdjustmentUpward(MachineFunction &Func);
    void InsertStackAdjustmentDownward(MachineFunction &Func);

    void SpillClobberedCalleeSavedRegisters(MachineFunction &Func);
    void ReloadClobberedCalleeSavedRegisters(MachineFunction &Func);

  private:
    MachineInstruction CreateStore(MachineFunction &Func, unsigned Register);
    MachineInstruction CreateLoad(MachineFunction &Func, unsigned Register);

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;

    /// The index of the Machine Basic Block, which contains a return instruction.
    unsigned MBBWithRetIdx = ~0;
};
