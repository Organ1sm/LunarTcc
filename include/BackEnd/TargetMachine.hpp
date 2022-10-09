#pragma once

#include "BackEnd/TargetABI.hpp"
#include "BackEnd/RegisterInfo.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/InstructionDefinitions.hpp"
#include "BackEnd/TargetInstructionLegalizer.hpp"
#include <memory>

class TargetMachine
{
  public:
    TargetMachine() {}
    TargetMachine(InstructionDefinitions *ID) : InstrDefs(ID) {}
    virtual ~TargetMachine() {}

    TargetABI *GetABI() { return ABI.get(); }
    InstructionDefinitions *GetInstrDefs() { return InstrDefs.get(); }
    RegisterInfo *GetRegInfo() { return RegInfo.get(); }
    TargetInstructionLegalizer *GetLegalizer() { return Legalizer.get(); }

    bool SelectInstruction(MachineInstruction *MI);

    virtual bool SelectAdd(MachineInstruction *MI) { return false; }
    virtual bool SelectSub(MachineInstruction *MI) { return false; }
    virtual bool SelectMul(MachineInstruction *MI) { return false; }
    virtual bool SelectDiv(MachineInstruction *MI) { return false; }
    virtual bool SelectMod(MachineInstruction *MI) { return false; }
    virtual bool SelectCmp(MachineInstruction *MI) { return false; }
    virtual bool SelectLoad(MachineInstruction *MI) { return false; }
    virtual bool SelectStore(MachineInstruction *MI) { return false; }
    virtual bool SelectBranch(MachineInstruction *MI) { return false; }
    virtual bool SelectJump(MachineInstruction *MI) { return false; }
    virtual bool SelectRet(MachineInstruction *MI) { return false; }

  protected:
    std::unique_ptr<TargetABI> ABI                        = nullptr;
    std::unique_ptr<RegisterInfo> RegInfo                 = nullptr;
    std::unique_ptr<InstructionDefinitions> InstrDefs     = nullptr;
    std::unique_ptr<TargetInstructionLegalizer> Legalizer = nullptr;
};