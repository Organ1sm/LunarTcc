#pragma once

#include "BackEnd/TargetInstructionLegalizer.hpp"

class TargetMachine;
class MachineInstruction;

namespace AArch64
{
    class AArch64InstructionLegalizer : public TargetInstructionLegalizer
    {
      public:
        AArch64InstructionLegalizer(TargetMachine *TM) : TargetInstructionLegalizer(TM) {}
        ~AArch64InstructionLegalizer() override {}

        bool Check(MachineInstruction *MI) override;
        bool IsExpandable(const MachineInstruction *MI) override;

        bool ExpandCmp(MachineInstruction *MI) override;
        bool ExpandSub(MachineInstruction *MI) override;
        bool ExpandMul(MachineInstruction *MI) override;
        bool ExpandDiv(MachineInstruction *MI) override;
        bool ExpandDivU(MachineInstruction *MI) override;
        bool ExpandStore(MachineInstruction *MI) override;
        bool ExpandGlobalAddress(MachineInstruction *MI) override;
    };
}    // namespace AArch64
