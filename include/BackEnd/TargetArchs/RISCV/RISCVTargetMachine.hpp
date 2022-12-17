#pragma once

#include "BackEnd/TargetMachine.hpp"

class MachineInstruction;

namespace RISCV
{
    enum Opcodes : unsigned;
    class RISCVTargetMachine : public TargetMachine
    {
      public:
        RISCVTargetMachine();
        ~RISCVTargetMachine() override {}

        uint8_t GetPointerSize() override { return 32; }
        uint8_t GetIntSize() override { return 32; }
        uint8_t GetLongSize() override { return 32; }

        bool SelectThreeAddressInstruction(MachineInstruction *MI,
                                           const Opcodes rrr,
                                           const Opcodes rri,
                                           unsigned ImmSize = 12);

        bool SelectThreeAddressInstruction(MachineInstruction *MI, const Opcodes rrr);

        bool SelectAnd(MachineInstruction *MI) override;
        bool SelectOr(MachineInstruction *MI) override;
        bool SelectXOR(MachineInstruction *MI) override;
        bool SelectLSL(MachineInstruction *MI) override;
        bool SelectLSR(MachineInstruction *MI) override;
        bool SelectAdd(MachineInstruction *MI) override;
        bool SelectSub(MachineInstruction *MI) override;
        bool SelectMul(MachineInstruction *MI) override;
        bool SelectMulHU(MachineInstruction *MI) override;
        bool SelectDiv(MachineInstruction *MI) override;
        bool SelectDivU(MachineInstruction *MI) override;
        bool SelectMod(MachineInstruction *MI) override;
        bool SelectModU(MachineInstruction *MI) override;
        bool SelectCmp(MachineInstruction *MI) override;

        bool SelectSExt(MachineInstruction *MI) override;
        bool SelectZExt(MachineInstruction *MI) override;
        bool SelectTrunc(MachineInstruction *MI) override;
        bool SelectLoad(MachineInstruction *MI) override;
        bool SelectLoadImm(MachineInstruction *MI) override;
        bool SelectMov(MachineInstruction *MI) override;
        bool SelectStore(MachineInstruction *MI) override;
        bool SelectStackAddress(MachineInstruction *MI) override;
        bool SelectGlobalAddress(MachineInstruction *MI) override;
        bool SelectBranch(MachineInstruction *MI) override;
        bool SelectJump(MachineInstruction *MI) override;
        bool SelectCall(MachineInstruction *MI) override;
        bool SelectRet(MachineInstruction *MI) override;
    };
}    // namespace RISCV
