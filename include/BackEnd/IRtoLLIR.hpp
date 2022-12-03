#pragma once

#include <vector>
#include <string>
#include <map>
#include <set>


class Value;
class Module;
class Function;
class Instruction;

class BinaryInstruction;
class UnaryInstruction;
class StoreInstruction;
class LoadInstruction;
class CallInstruction;
class JumpInstruction;
class BranchInstruction;
class CompareInstruction;
class ReturnInstruction;
class MemoryCopyInstruction;
class GetElemPointerInstruction;

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
    MachineOperand GetMachineOperandFromValue(Value *Val, bool IsDef = false);

    MachineIRModule *GetMachineIRModule() { return TU; }

    MachineInstruction HandleBinaryInstruction(BinaryInstruction *I);
    MachineInstruction HandleUnaryInstruction(UnaryInstruction *I);
    MachineInstruction HandleStoreInstruction(StoreInstruction *I);
    MachineInstruction HandleLoadInstruction(LoadInstruction *I);
    MachineInstruction HandleCallInstruction(CallInstruction *I);
    MachineInstruction HandleGetElemPtrInstruction(GetElemPointerInstruction *I);
    MachineInstruction HandleCompareInstruction(CompareInstruction *I);
    MachineInstruction HandleReturnInstruction(ReturnInstruction *I);
    MachineInstruction HandleMemoryCopyInstruction(MemoryCopyInstruction *I);
    MachineInstruction HandleJumpInstruction(JumpInstruction *I,
                                             std::vector<MachineBasicBlock> &BBs);
    MachineInstruction HandleBranchInstruction(BranchInstruction *I,
                                               std::vector<MachineBasicBlock> &BBs);

    /// return the ID of the Value, but checks if it was mapped and if so then
    /// returning the mapped value
    unsigned GetIDFromValue(Value *Val);

    void HandleFunctionParams(Function &F, MachineFunction *Func);
    MachineInstruction ConvertToMachineInstr(Instruction *Instr,
                                             std::vector<MachineBasicBlock> &BBs);

  private:
    Module &IRM;
    TargetMachine *TM;
    MachineIRModule *TU;

    /// Current Processing of BasicBlock
    MachineBasicBlock *CurrentBB;

    /// The Function to which the currently processed basic block belongs
    MachineFunction *ParentFunction;

    std::map<std::string, std::vector<unsigned>> StructToRegMap;

    /// to keep track in which registers the struct is currently living
    std::map<unsigned, std::vector<unsigned>> StructByIDToRegMap;

    /// Keep track what IR virtual registers were mapped to what LLIR virtual
    /// registers. This needed since while translating from IR to LLIR occasionally
    /// new instructions are added with possible new virtual registers.
    std::map<unsigned, unsigned> IRVregToLLIRVreg;

    /// To keep track which stack slots are used for spilling the return values
    /// of functions calls.
    std::set<unsigned> SpilledReturnValuesStackIDs;
};
