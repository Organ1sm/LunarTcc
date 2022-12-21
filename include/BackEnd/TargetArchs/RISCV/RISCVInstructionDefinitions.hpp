#pragma once

#include "BackEnd/InstructionDefinitions.hpp"
#include "BackEnd/TargetInstruction.hpp"
#include <map>

namespace RISCV
{
    enum Opcodes : unsigned {
        ADD,
        ADDI,
        REM,
        SLT,
        SLTI,
        LW,
        SW,
        BEQ,
        BLT,
        BNEZ,
        J,
        RET,
    };

    enum OperandTypes : unsigned {
        GPR,
        SIMM12,
        SIMM13_LSB0,
        SIMM21_LSB0
    };


    class RISCVInstructionDefinitions : public InstructionDefinitions
    {
        using IRToTargetInstrMap = std::map<unsigned, TargetInstruction>;

      public:
        RISCVInstructionDefinitions() {}
        ~RISCVInstructionDefinitions() override {}
        TargetInstruction *GetTargetInstr(unsigned Opcode) override;

      private:
        static IRToTargetInstrMap Instructions;
    };
}    // namespace RISCV
