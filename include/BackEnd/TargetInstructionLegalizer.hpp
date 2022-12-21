#pragma once

class TargetMachine;
class MachineInstruction;

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

    virtual bool ExpandCmp(MachineInstruction *MI) { return false; }
    virtual bool ExpandMod(MachineInstruction *MI, bool IsUnsigned);
    virtual bool ExpandSub(MachineInstruction *MI) { return false; }
    virtual bool ExpandMul(MachineInstruction *MI) { return false; }
    virtual bool ExpandDiv(MachineInstruction *MI) { return false; }
    virtual bool ExpandDivU(MachineInstruction *MI) { return false; }

    virtual bool ExpandStore(MachineInstruction *MI);
    virtual bool ExpandZExt(MachineInstruction *MI) { return false; };
    virtual bool ExpandGlobalAddress(MachineInstruction *MI) { return false; }

    /// Expandin the instruction into other ones which are compute the same
    /// value, but usually takes more instructions.
    bool Expand(MachineInstruction *MI);

  protected:
    TargetMachine *TM = nullptr;
};
