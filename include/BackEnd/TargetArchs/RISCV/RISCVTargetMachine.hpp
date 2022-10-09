#pragma once

#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "RISCVTargetABI.hpp"
#include "RISCVRegisterInfo.hpp"
#include "RISCVInstructionDefinitions.hpp"


namespace RISCV
{
    class RISCVTargetMachine : public TargetMachine
    {
      public:
        RISCVTargetMachine();

        ~RISCVTargetMachine() override {}

        bool SelectAdd(MachineInstruction *MI) override;
        bool SelectMod(MachineInstruction *MI) override;
        bool SelectCmp(MachineInstruction *MI) override;
        bool SelectLoad(MachineInstruction *MI) override;
        bool SelectStore(MachineInstruction *MI) override;
        bool SelectBranch(MachineInstruction *MI) override;
        bool SelectJump(MachineInstruction *MI) override;
        bool SelectRet(MachineInstruction *MI) override;
    };
}    // namespace RISCV
