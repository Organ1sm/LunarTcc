#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64MovFixPass.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64InstructionDefinitions.hpp"


void AArch64MovFixPass::Run()
{
    for (auto &MFunc : MIRM->GetFunctions())
    {
        for (auto &MBB : MFunc.GetBasicBlocks())
        {
            for (auto &Instr : MBB.GetInstructions())
            {
                if (Instr.GetOpcode() == AArch64::MOV_rr &&
                    Instr.GetOperand(0)->GetSize() == 32 &&
                    Instr.GetOperand(1)->GetSize() == 64)
                {
                    auto SrcXReg = Instr.GetOperand(1)->GetReg();
                    auto WReg =
                        TM->GetRegInfo()->GetRegisterByID(SrcXReg)->GetSubRegs()[0];

                    Instr.GetOperand(1)->SetReg(WReg);
                }
            }
        }
    }
}
