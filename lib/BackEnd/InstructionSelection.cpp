#include "BackEnd/InstructionSelection.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/TargetMachine.hpp"
#include <cassert>

void InstructionSelection::InstrSelect()
{
    for (auto &MFunc : MIRM->GetFunctions())
    {
        for (auto &MBB : MFunc.GetBasicBlocks())
        {
            for (size_t i = 0; i < MBB.GetInstructions().size(); i++)
            {
                // Skip selection if already selected
                if (!MBB.GetInstructions()[i].IsAlreadySelected())
                {
                    TM->SelectInstruction(&MBB.GetInstructions()[i]);

                    assert(MBB.GetInstructions()[i].IsAlreadySelected());
                }
            }
        }
    }
}
