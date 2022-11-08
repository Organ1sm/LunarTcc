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
    using PhysRegList    = std::vector<unsigned>;

  public:
    MachineFunction() {}
    MachineFunction(BasicBlockList BBS) : BasicBlocks(BBS) {}

    void SetBasicBlocks(BasicBlockList BBS) { this->BasicBlocks = BBS; }
    BasicBlockList &GetBasicBlocks() { return BasicBlocks; }

    unsigned GetNextVirtualReg() const { return NextVirtualReg; }
    void SetNextVirtualReg(const unsigned r) { NextVirtualReg = r; }

    std::string &GetName() { return Name; }
    void SetName(std::string &Name) { this->Name = Name; }

    void SetToCaller() { HasCall = true; }
    bool IsCaller() const { return HasCall; }

    void InsertStackSlot(unsigned ID, unsigned Size);
    void InsertParameter(unsigned ID, LowLevelType LLT);

    ParamList GetParameters() { return Parameters; }
    StackFrame &GetStackFrame() { return SF; }
    PhysRegList &GetUsedCalleeSavedRegs() { return UsedCalleSavedRegs; }

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

    /// To keep track what registers were used in the function which must be saved
    /// by the called function if clobbered. This information is used for the
    /// prolog and epilog insertion to restore these registers.
    PhysRegList UsedCalleSavedRegs;
};
