#include "BackEnd/MachineInstructionLegalizer.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/TargetMachine.hpp"

void MachineInstructionLegalizer::Run()
{
    auto Legalizer = TM->GetLegalizer();

    if (Legalizer == nullptr)
        return;

    for (auto &Func : MIRM->GetFunctions())
    {
        for (size_t BBIndex = 0; BBIndex < Func.GetBasicBlocks().size(); BBIndex++)
            for (size_t InstrIndex = 0;
                 InstrIndex < Func.GetBasicBlocks()[BBIndex].GetInstructions().size();
                 InstrIndex++)
            {
                auto *MI = &Func.GetBasicBlocks()[BBIndex].GetInstructions()[InstrIndex];

                // If the instruction is not legal on the target and not selected, not has
                // been expanded.
                if (!Legalizer->Check(MI) && !MI->IsAlreadySelected() &&
                    !MI->IsAlreadyExpanded())
                {
                    // but if it is expandable to hopefully legal ones, then do it
                    if (Legalizer->IsExpandable(MI))
                    {
                        if (Legalizer->Expand(MI))
                            InstrIndex--;
                        else
                            assert(!"Expandable instruction should be expandable");

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
