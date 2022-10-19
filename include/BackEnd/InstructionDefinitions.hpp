#pragma once

#include <string>

class TargetInstruction;

class InstructionDefinitions
{
  public:
    InstructionDefinitions() {}
    virtual ~InstructionDefinitions() {}

    virtual TargetInstruction *GetTargetInstr(unsigned Opcode) { return nullptr; }
    virtual std::string GetInstrString(unsigned Index) { return ""; }
};
