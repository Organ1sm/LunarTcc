#pragma once

class TargetInstruction;

class InstructionDefinitions
{
  public:
    InstructionDefinitions() {}
    virtual ~InstructionDefinitions() {}
    virtual TargetInstruction *GetTargetInstr(unsigned Opcode) { return nullptr; }
};
