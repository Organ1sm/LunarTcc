#pragma once

#include <vector>
#include <string>
#include <map>

class Value;
class Module;
class Function;
class Instruction;

class TargetMachine;
class MachineIRModule;
class MachineFunction;
class MachineBasicBlock;
class MachineOperand;
class MachineInstruction;

class IRtoLLIR
{
  public:
    IRtoLLIR(Module &IRModule, MachineIRModule *TranslUnit, TargetMachine *TM)
        : IRM(IRModule), TU(TranslUnit), TM(TM)
    {}

    void Reset();

    void GenerateLLIRFromIR();
    MachineOperand GetMachineOperandFromValue(Value *Val, MachineFunction *MF);

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

    /// to keep track in which registers the struct is currently living
    std::map<unsigned, std::vector<unsigned>> StructByIDToRegMap;

    /// Keep track what IR virtual registers were mapped to what LLIR virtual
    /// registers. This needed since while translating from IR to LLIR occasionally
    /// new instructions are added with possible new virtual registers.
    std::map<unsigned, unsigned> IRVregToLLIRVreg;
};
