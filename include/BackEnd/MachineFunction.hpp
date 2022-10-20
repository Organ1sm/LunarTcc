#pragma once

#include <string>
#include <vector>
#include "BackEnd/StackFrame.hpp"
#include "BackEnd/LowLevelType.hpp"
#include "BackEnd/MachineBasicBlock.hpp"

class MachineFunction
{
    using BasicBlockList = std::vector<MachineBasicBlock>;
    using ParamList      = std::vector<std::pair<unsigned, LowLevelType>>;

  public:
    MachineFunction() {}
    MachineFunction(BasicBlockList BBS) : BasicBlocks(BBS) {}

    void SetBasicBlocks(BasicBlockList BBS) { this->BasicBlocks = BBS; }
    BasicBlockList &GetBasicBlocks() { return BasicBlocks; }

    std::string &GetName() { return Name; }
    void SetName(std::string &Name) { this->Name = Name; }

    void SetToCaller() { HasCall = true; }
    bool IsCaller() const { return HasCall; }

    void InsertStackSlot(unsigned ID, unsigned Size) { SF.InsertStackSlot(ID, Size); }
    void InsertParameter(unsigned ID, LowLevelType LLT)
    {
        Parameters.push_back({ID, LLT});
    }

    ParamList GetParameters() { return Parameters; }
    StackFrame &GetStackFrame() { return SF; }

    unsigned GetStackFrameSize() { return SF.GetSize(); }
    unsigned GetStackObjectPosition(unsigned ID) { return SF.GetPosition(ID); }
    unsigned GetStackObjectSize(unsigned ID) { return SF.GetSize(ID); }

    bool IsStackSlot(unsigned ID) { return SF.IsStackSlot(ID); }

    void Print(TargetMachine *TM) const;

    /// Get the next available virtual register.
    unsigned GetNextAvailableVirtualRegister();

  private:
    std::string Name;
    StackFrame SF;
    ParamList Parameters;
    BasicBlockList BasicBlocks;
    unsigned NextVirtualReg = 0;

    /// Predicate to signal if the function is calling other functions or not
    bool HasCall {false};
};
