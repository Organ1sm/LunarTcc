#pragma once

#include <vector>

class MachineIRModule;
class MachineInstruction;
class MachineOperand;
class TargetMachine;

class LLIROptimizer
{
  public:
    LLIROptimizer(MachineIRModule *Input, TargetMachine *Target) : MIRM(Input), TM(Target)
    {}

    void Run();

  private:
    MachineIRModule *MIRM;
    TargetMachine *TM;
};

struct AliveDefinitions
{
    std::vector<MachineInstruction *> Instructions;

    AliveDefinitions() = default;

    void InvalidateAll() { Instructions.clear(); }

    void InsertDef(MachineInstruction *I) { Instructions.push_back(I); }

    MachineOperand *GetAlreadyComputedExpression(MachineInstruction &I);
};
