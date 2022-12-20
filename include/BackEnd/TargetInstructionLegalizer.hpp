#pragma once

#include "BackEnd/MachineInstruction.hpp"
#include <set>

class TargetMachine;
class MachineFunction;
class MachineBasicBlock;

/// Instruction legalization interface for targets to implement if. If a target
/// does not support a target independent IR instruction, then it should create
/// a subclass of this class, where it specify which instruction is not legal
/// for the target.
class TargetInstructionLegalizer
{
  public:
    TargetInstructionLegalizer(TargetMachine *TM) : TM(TM) {}
    virtual ~TargetInstructionLegalizer() {}

    /// Predicate to decide which instructions are legal and which not.
    virtual bool Check(MachineInstruction *MI) { return true; }

    /// Predicate to decide whether the instruction is expandable or not.
    virtual bool IsExpandable(const MachineInstruction *MI) { return false; }

    bool IsRelSupported(MachineInstruction::CmpRelation Rel) const;

    virtual bool ExpandAdd(MachineInstruction *MI);
    virtual bool ExpandAddS(MachineInstruction *MI);
    virtual bool ExpandAddC(MachineInstruction *MI);
    virtual bool ExpandSub(MachineInstruction *MI);
    virtual bool ExpandMul(MachineInstruction *MI);
    virtual bool ExpandDiv(MachineInstruction *MI) { return false; }
    virtual bool ExpandDivU(MachineInstruction *MI) { return false; }

    virtual bool ExpandXOR(MachineInstruction *MI);

    virtual bool ExpandLoad(MachineInstruction *MI);
    virtual bool ExpandLoadImm(MachineInstruction *MI);
    virtual bool ExpandStore(MachineInstruction *MI);

    virtual bool ExpandCmp(MachineInstruction *MI);
    virtual bool ExpandMod(MachineInstruction *MI, bool IsUnsigned);

    virtual bool ExpandZExt(MachineInstruction *MI);
    virtual bool ExpandTrunc(MachineInstruction *MI);
    virtual bool ExpandGlobalAddress(MachineInstruction *MI) { return false; }

    /// Expanding the instruction into other ones which are compute the same
    /// value, but usually takes more instructions.
    bool Expand(MachineInstruction *MI);

  private:
    unsigned GetNextAvailVReg();

    void InitCurrProcessMFB(MachineInstruction *MI);

    MachineInstruction CreateThreeAddrMI(MachineInstruction::OperationCode Kind,
                                         MachineOperand First,
                                         MachineOperand Second,
                                         MachineOperand Third);

  protected:
    TargetMachine *TM = nullptr;

    /// Current process machine function
    MachineFunction *PMF;

    /// Current process machine basicblock
    MachineBasicBlock *PMBB;

    std::set<MachineInstruction::CmpRelation> UnSupportedRelations;
};
