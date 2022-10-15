#pragma once

#include "BackEnd/TargetInstructionLegalizer.hpp"

class MachineInstruction;

namespace AArch64
{
    class AArch64InstructionLegalizer : public TargetInstructionLegalizer
    {
      public:
        AArch64InstructionLegalizer() {}
        ~AArch64InstructionLegalizer() override {}

        bool Check(MachineInstruction *MI) override;
        bool IsExpandable(const MachineInstruction *MI) override;
    };
}    // namespace AArch64
