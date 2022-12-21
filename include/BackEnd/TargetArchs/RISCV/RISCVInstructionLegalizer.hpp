#pragma once

#include "BackEnd/TargetInstructionLegalizer.hpp"
#include "BackEnd/TargetMachine.hpp"

namespace RISCV
{

    class RISCVInstructionLegalizer : public TargetInstructionLegalizer
    {
      public:
        explicit RISCVInstructionLegalizer(TargetMachine *TM)
            : TargetInstructionLegalizer(TM)
        {
            UnSupportedRelations.insert(MachineInstruction::EQ);
            UnSupportedRelations.insert(MachineInstruction::NE);
            UnSupportedRelations.insert(MachineInstruction::GT);
            UnSupportedRelations.insert(MachineInstruction::LE);
            UnSupportedRelations.insert(MachineInstruction::GE);
        }

        ~RISCVInstructionLegalizer() override {}


        bool Check(MachineInstruction *MI) override;
        bool IsExpandable(const MachineInstruction *MI) override;

        /// Since RISCV does not support for immediate operand as last source
        /// operand for some arithmetic instruction, therefore it has to be
        /// materialized first into a register
        bool ExpandDiv(MachineInstruction *MI) override;
        bool ExpandDivU(MachineInstruction *MI) override;
        bool ExpandMod(MachineInstruction *MI, bool IsUnsigned) override;
    };

}    // namespace RISCV
