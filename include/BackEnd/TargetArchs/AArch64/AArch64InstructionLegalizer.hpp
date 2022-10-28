#pragma once

#include "BackEnd/TargetInstructionLegalizer.hpp"

class TargetMachine;
class MachineInstruction;

namespace AArch64
{
    class AArch64InstructionLegalizer : public TargetInstructionLegalizer
    {
      public:
        AArch64InstructionLegalizer(TargetMachine *TM) : TM(TM) {}
        ~AArch64InstructionLegalizer() override {}

        bool Check(MachineInstruction *MI) override;
        bool IsExpandable(const MachineInstruction *MI) override;

        bool ExpandZExt(MachineInstruction *MI) override;
        bool ExpandGlobalAddress(MachineInstruction *MI) override;

      private:
        TargetMachine *TM {nullptr};
    };
}    // namespace AArch64
