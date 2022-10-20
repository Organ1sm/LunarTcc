#pragma once

#include <vector>
#include <string>
#include <map>

class Module;
class Function;
class Instruction;

class TargetMachine;
class MachineIRModule;
class MachineFunction;
class MachineBasicBlock;
class MachineInstruction;

class IRtoLLIR
{
  public:
    IRtoLLIR(Module &IRModule, MachineIRModule *TranslUnit, TargetMachine *TM)
        : IRM(IRModule), TU(TranslUnit), TM(TM)
    {}

    void GenerateLLIRFromIR();

    MachineIRModule *GetMachineIRModule() { return TU; }

    void HandleFunctionParams(Function &F, MachineFunction *Func);
    MachineInstruction ConvertToMachineInstr(Instruction *Instr,
                                             MachineBasicBlock *BB,
                                             std::vector<MachineBasicBlock> &BBs);

  private:
    Module &IRM;
    TargetMachine *TM;
    MachineIRModule *TU;
    std::map<std::string, std::vector<unsigned>> StructToRegMap;
};
