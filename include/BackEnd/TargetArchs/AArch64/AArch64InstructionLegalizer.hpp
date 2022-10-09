#pragma once

#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetInstructionLegalizer.hpp"


namespace AArch64
{
    class AArch64InstructionLegalizer : public TargetInstructionLegalizer
    {
      public:
        AArch64InstructionLegalizer() {}
        ~AArch64InstructionLegalizer() override {}

        bool Check(const MachineInstruction *MI) override;
        bool IsExpandable(const MachineInstruction *MI) override;
    };
}    // namespace AArch64
