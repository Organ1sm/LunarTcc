#include "BackEnd/MachineInstructionLegalizer.hpp"
#include "BackEnd/MachineBasicBlock.hpp"


void MachineInstructionLegalizer::Run()
{
    auto Legalizer = TM->GetLegalizer();
    if (Legalizer == nullptr)
        return;

    for (auto &Func : MIRM->GetFunctions())
    {
        auto BBS = Func.GetBasicBlocks();
        for (std::size_t BBIndex = 0; BBIndex < BBS.size(); BBIndex++)
        {
            auto Instrs = BBS[BBIndex].GetInstructions();
            for (std::size_t InstrIndex = 0; InstrIndex < Instrs.size(); InstrIndex++)
            {
                auto *MI = &Instrs[InstrIndex];

                // if the instruction is not legal on the target
                if (!Legalizer->Check(MI))
                {
                    if (Legalizer->IsExpandable(MI))
                    {
                        Legalizer->Expand(MI);
                        InstrIndex--;
                        continue;
                    }
                    else
                    {
                        assert(!"Machine Instruction is not legal neither expandable");
                    }
                }
            }
        }
    }
}
