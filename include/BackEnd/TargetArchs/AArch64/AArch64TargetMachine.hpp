#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "AArch64InstructionDefinitions.hpp"
#include "AArch64InstructionLegalizer.hpp"
#include "AArch64RegisterInfo.hpp"
#include "AArch64TargetABI.hpp"
#include <cassert>


namespace AArch64
{
    class AArch64TargetMachine : public TargetMachine
    {
      public:
        AArch64TargetMachine();
        ~AArch64TargetMachine() override {}

        bool SelectAdd(MachineInstruction *MI) override;
        bool SelectSub(MachineInstruction *MI) override;
        bool SelectMul(MachineInstruction *MI) override;
        bool SelectDiv(MachineInstruction *MI) override;
        bool SelectMod(MachineInstruction *MI) override;
        bool SelectCmp(MachineInstruction *MI) override;
        bool SelectLoadImm(MachineInstruction *MI) override;
        bool SelectLoad(MachineInstruction *MI) override;
        bool SelectStore(MachineInstruction *MI) override;
        bool SelectBranch(MachineInstruction *MI) override;
        bool SelectJump(MachineInstruction *MI) override;
        bool SelectRet(MachineInstruction *MI) override;
    };

}    // namespace AArch64
