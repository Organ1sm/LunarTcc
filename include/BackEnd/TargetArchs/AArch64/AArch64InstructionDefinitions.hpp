#pragma once

#include "BackEnd/InstructionDefinitions.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetInstruction.hpp"
#include <map>

namespace AArch64
{
    enum Opcodes : unsigned {
        ADD_rrr,
        ADD_rri,
        SUB_rrr,
        SUB_rri,
        SUBS,
        MUL_rri,
        MUL_rrr,
        SDIV_rri,
        SDIV_rrr,
        CMP_ri,
        CMP_rr,
        CSET,
        LDR,
        STR,
        BEQ,
        BLE,
        B,
        RET,
    };

    enum OperandTypes : unsigned {
        GPR,
        SIMM12,
        UIMM12,
        SIMM13_LSB0,
        SIMM21_LSB0,
    };

    class AArch64InstructionDefinitions : public InstructionDefinitions
    {
        using IRToTargetInstrMap = std::map<unsigned, TargetInstruction>;

      public:
        AArch64InstructionDefinitions() {}
        ~AArch64InstructionDefinitions() override {}

        TargetInstruction *GetTargetInstr(unsigned Opcode) override;

      private:
        static IRToTargetInstrMap Instructions;
    };

}    // namespace AArch64
