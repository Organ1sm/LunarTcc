#pragma once

#include <string>
#include <vector>
#include "BackEnd/StackFrame.hpp"
#include "BackEnd/LowLevelType.hpp"
#include "BackEnd/MachineInstruction.hpp"

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

    void InsertStackSlot(unsigned ID, unsigned Size) { SF.InsertStackSlot(ID, Size); }
    void InsertParameter(unsigned ID, LowLevelType LLT)
    {
        Parameters.push_back({ID, LLT});
    }

    ParamList GetParameters() { return Parameters; }

    unsigned GetStackFrameSize() { return SF.GetSize(); }
    unsigned GetStackObjectPosition(unsigned ID) { return SF.GetPosition(ID); }

    bool IsStackSlot(unsigned ID) { return SF.IsStackSlot(ID); }

    void Print() const;

    /// Get the next available virtual register.
    unsigned GetNextAvailableVirtualRegister();

  private:
    std::string Name;
    StackFrame SF;
    ParamList Parameters;
    BasicBlockList BasicBlocks;
    unsigned NextVirtualReg = 0;
};
