#include "BackEnd/AssemblyEmitter.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/TargetRegister.hpp"
#include "BackEnd/TargetInstruction.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "fmt/color.h"
#include "fmt/core.h"
#include <iostream>
#include <cassert>
#include <string>


void AssemblyEmitter::GenerateAssembly()
{
    for (auto &Func : MIRM->GetFunctions())
    {
        fmt::print(".globl\t{}\n", Func.GetName());
        fmt::print("{}:\n", Func.GetName());

        bool IsFirstBB = true;

        for (auto &BB : Func.GetBasicBlocks())
        {
            if (!IsFirstBB)
                fmt::print(".L{}:\n", BB.GetName());
            else
                IsFirstBB = false;

            for (auto &Instr : BB.GetInstructions())
            {
                fmt::print("\t");

                auto TargetInstr = TM->GetInstrDefs()->GetTargetInstr(Instr.GetOpcode());
                assert(TargetInstr != nullptr && "Something went wrong here.");

                std::string AssemblyTemplateStr = TargetInstr->GetAsmString();
                const auto OperandNumber        = TargetInstr->GetOperandNumber();

                // if the target instruction has no operands, then just print it and
                // continue.
                if (OperandNumber == 0)
                {
                    fmt::print("{}\n", AssemblyTemplateStr);
                    continue;
                }

                // Substitute the stringified operands to their appropriate places
                // example:
                // add $1, $2, $3 --> add a0, a1, a2
                for (std::size_t i = 0; i < OperandNumber; i++)
                {
                    std::size_t DollarPos = AssemblyTemplateStr.find('$');

                    if (DollarPos == std::string::npos)
                        assert(!"The number of template operands are "
                                "not match the number of operands");

                    unsigned NthOperand = AssemblyTemplateStr[DollarPos + 1] - '0';
                    auto CurrentOperand = Instr.GetOperand(NthOperand - 1);


                    // Register case
                    if (CurrentOperand->IsRegister())
                    {
                        TargetRegister *Reg =
                            TM->GetRegInfo()->GetRegisterByID(CurrentOperand->GetReg());
                        std::string RegStr;

                        if (Reg->GetAlias() != "")
                            RegStr = Reg->GetAlias();
                        else
                            RegStr = Reg->GetName();

                        AssemblyTemplateStr.replace(DollarPos, 2, RegStr);
                    }
                    // Immediate case
                    else if (CurrentOperand->IsImmediate())
                    {
                        std::string ImmStr =
                            std::to_string(CurrentOperand->GetImmediate());
                        AssemblyTemplateStr.replace(DollarPos, 2, ImmStr);
                    }
                    // Label and FunctionName (func call) case
                    else if (CurrentOperand->IsLabel() || CurrentOperand->IsGlobalSymbol()
                             || CurrentOperand->IsFunctionName())
                    {
                        std::string Str = "";
                        if (CurrentOperand->IsLabel())
                            Str += ".L";

                        if (!CurrentOperand->IsGlobalSymbol())
                            Str.append(CurrentOperand->GetLabel());
                        else
                            Str.append(CurrentOperand->GetGlobalSymbol());

                        AssemblyTemplateStr.replace(DollarPos, 2, Str);
                    }
                    else
                    {
                        assert(!"Invalid Machine Operand type");
                    }
                }

                fmt::print("{}\n", AssemblyTemplateStr);
            }
        }
        fmt::print("\n");
    }

    for (auto &GlobalData : MIRM->GetGlobalDatas())
        GlobalData.Print();
}
