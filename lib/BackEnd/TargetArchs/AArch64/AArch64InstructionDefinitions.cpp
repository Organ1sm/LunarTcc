#include "BackEnd/TargetArchs/AArch64/AArch64InstructionDefinitions.hpp"

using namespace AArch64;
AArch64InstructionDefinitions::AArch64InstructionDefinitions()
{
    InstrEnumStrings = {"ADD_rrr", "ADD_rri", "AND_rri",  "SUB_rrr",  "SUB_rri", "SUBS",
                        "MUL_rri", "MUL_rrr", "SDIV_rri", "SDIV_rrr", "CMP_ri",  "CMP_rr",
                        "CSET",    "SXTB",    "MOV_rc",   "LDR",      "LDRB",    "STR",
                        "STRB",    "BEQ",     "BNE",      "BGE",      "BGT",     "BLE",
                        "BLT",     "B",       "RET"};
}

AArch64InstructionDefinitions::IRToTargetInstrMap
    AArch64InstructionDefinitions::Instructions = [] {
        IRToTargetInstrMap ret;

        ret[ADD_rrr] = {
            ADD_rrr,
            32,
            "add\t$1, $2, $3",
            {GPR, GPR, GPR}
        };
        ret[ADD_rri] = {
            ADD_rri,
            32,
            "add\t$1, $2, #$3",
            {GPR, GPR, UIMM12}
        };
        ret[SUB_rrr] = {
            SUB_rrr,
            32,
            "sub\t$1, $2, $3",
            {GPR, GPR, GPR}
        };
        ret[SUB_rri] = {
            SUB_rri,
            32,
            "sub\t$1, $2, #$3",
            {GPR, GPR, UIMM12}
        };
        ret[SUBS] = {
            SUBS,
            32,
            "subs\t$1, $2, $3",
            {GPR, GPR, GPR}
        };
        ret[SDIV_rrr] = {
            SDIV_rrr,
            32,
            "sdiv\t$1, $2, $3",
            {GPR, GPR, GPR}
        };
        ret[SDIV_rri] = {
            SDIV_rri,
            32,
            "sdiv\t$1, $2, #$3",
            {GPR, GPR, UIMM12}
        };
        ret[MUL_rrr] = {
            MUL_rri,
            32,
            "mul\t$1, $2, $3",
            {GPR, GPR, GPR}
        };
        ret[MUL_rri] = {
            MUL_rrr,
            32,
            "mul\t$1, $2, #$3",
            {GPR, GPR, UIMM12}
        };
        ret[AND_rri] = {
            ADD_rri,
            32,
            "and\t$1, $2, #$3",
            {GPR, GPR, UIMM12}
        };
        ret[CMP_rr] = {
            CMP_rr,
            32,
            "cmp\t$1, $2",
            {GPR, GPR}
        };
        ret[CMP_ri] = {
            CMP_ri,
            32,
            "cmp\t$1, #$2",
            {GPR, UIMM12}
        };
        ret[CSET] = {
            CSET,
            32,
            "cset\t$1, $2, $3",
            {GPR, GPR, GPR}
        };
        ret[SXTB] = {
            SXTB,
            32,
            "sxtb\t$1, $2",
            {GPR, GPR}
        };
        ret[MOV_ri] = {
            MOV_ri,
            32,
            "mov\t$1, #$2",
            {GPR, UIMM16}
        };
        ret[LDR] = {
            LDR,
            32,
            "ldr\t$1, [$2, #$3]",
            {GPR, GPR, SIMM12},
            TargetInstruction::Load
        };
        ret[LDRB] = {
            LDRB,
            32,
            "ldrb\t$1, [$2, #$3]",
            {GPR, GPR, SIMM12},
            TargetInstruction::Load
        };
        ret[STR] = {
            STR,
            32,
            "str\t$1, [$2, #$3]",
            {GPR, GPR, SIMM12},
            TargetInstruction::Store
        };
        ret[STRB] = {
            STRB,
            32,
            "strb\t$1, [$2, #$3]",
            {GPR, GPR, SIMM12},
            TargetInstruction::Store
        };
        ret[BGE] = {BGE, 32, "b.ge\t$1", {SIMM21_LSB0}};
        ret[BGT] = {BGT, 32, "b.gt\t$1", {SIMM21_LSB0}};
        ret[BLE] = {BLE, 32, "b.le\t$1", {SIMM21_LSB0}};
        ret[BLT] = {BLT, 32, "b.lt\t$1", {SIMM21_LSB0}};
        ret[BEQ] = {BEQ, 32, "b.eq\t$1", {SIMM21_LSB0}};
        ret[BNE] = {BNE, 32, "b.ne\t$1", {SIMM21_LSB0}};
        ret[B]   = {B, 32, "b\t$1", {SIMM21_LSB0}};
        ret[RET] = {RET, 32, "ret", {}, TargetInstruction::Return};

        return ret;
    }();

TargetInstruction *AArch64InstructionDefinitions::GetTargetInstr(unsigned int Opcode)
{
    if (Instructions.count(Opcode) == 0)
        return nullptr;

    return &Instructions[Opcode];
}

std::string AArch64InstructionDefinitions::GetInstrString(unsigned int index)
{
    return InstrEnumStrings[index];
}
