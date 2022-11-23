#pragma once

#include "BackEnd/TargetMachine.hpp"
#include <cassert>

namespace AArch64
{
    class AArch64TargetMachine : public TargetMachine
    {
      public:
        AArch64TargetMachine();
        ~AArch64TargetMachine() override {}

        uint8_t GetPointerSize() override { return 64; }

        bool SelectAnd(MachineInstruction *MI) override;
        bool SelectXOR(MachineInstruction *MI) override;
        bool SelectLSL(MachineInstruction *MI) override;
        bool SelectLSR(MachineInstruction *MI) override;
        bool SelectAdd(MachineInstruction *MI) override;
        bool SelectSub(MachineInstruction *MI) override;
        bool SelectMul(MachineInstruction *MI) override;
        bool SelectDiv(MachineInstruction *MI) override;
        bool SelectMod(MachineInstruction *MI) override;
        bool SelectCmp(MachineInstruction *MI) override;
        bool SelectDivU(MachineInstruction *MI) override;
        bool SelectModU(MachineInstruction *MI) override;
        bool SelectZExt(MachineInstruction *MI) override;
        bool SelectSExt(MachineInstruction *MI) override;
        bool SelectZExtLoad(MachineInstruction *MI) override;
        bool SelectTrunc(MachineInstruction *MI) override;
        bool SelectMov(MachineInstruction *MI) override;
        bool SelectLoadImm(MachineInstruction *MI) override;
        bool SelectLoad(MachineInstruction *MI) override;
        bool SelectStore(MachineInstruction *MI) override;
        bool SelectStackAddress(MachineInstruction *MI) override;
        bool SelectCall(MachineInstruction *MI) override;
        bool SelectBranch(MachineInstruction *MI) override;
        bool SelectJump(MachineInstruction *MI) override;
        bool SelectRet(MachineInstruction *MI) override;
    };

}    // namespace AArch64
