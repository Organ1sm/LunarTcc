#include "BackEnd/AssemblyEmitter.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/TargetRegister.hpp"
#include "BackEnd/TargetInstruction.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include <iostream>
#include <cassert>
#include <string>


void AssemblyEmitter::GenerateAssembly()
{
    for (auto &Func : MIRM->GetFunctions())
    {
        std::cout << ".globl\t" << Func.GetName() << std::endl;
        std::cout << Func.GetName() << ":" << std::endl;

        bool IsFirstBB = true;

        for (auto &BB : Func.GetBasicBlocks())
        {
            if (!IsFirstBB)
                std::cout << ".L" << BB.GetName() << ":" << std::endl;
            else
                IsFirstBB = false;

            for (auto &Instr : BB.GetInstructions())
            {
                std::cout << "\t";

                auto TargetInstr = TM->GetInstrDefs()->GetTargetInstr(Instr.GetOpcode());
                assert(TargetInstr != nullptr && "Something went wrong here.");

                std::string AssemblyTemplateStr = TargetInstr->GetAsmString();
                const auto OperandNumber        = TargetInstr->GetOperandNumber();

                // if the target instruction has no operands, then just print it and
                // continue.
                if (OperandNumber == 0)
                {
                    std::cout << AssemblyTemplateStr << std::endl;
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
                    else if (CurrentOperand->IsLabel()
                             || CurrentOperand->IsFunctionName())
                    {
                        std::string Str = "";
                        if (CurrentOperand->IsLabel())
                            Str += ".L";

                        Str.append(CurrentOperand->GetLabel());
                        AssemblyTemplateStr.replace(DollarPos, 2, Str);
                    }
                    else
                    {
                        assert(!"Invalid Machine Operand type");
                    }
                }

                std::cout << AssemblyTemplateStr << std::endl;
            }
        }
    }
    std::cout << std::endl;
}
