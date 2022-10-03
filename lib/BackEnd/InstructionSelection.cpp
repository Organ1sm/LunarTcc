#include "BackEnd/InstructionSelection.hpp"
#include "BackEnd/MachineBasicBlock.hpp"

void InstructionSelection::InstrSelect()
{
    for (auto &MFunc : MIRM->GetFunctions())
    {
        for (auto &MBB : MFunc.GetBasicBlocks())
        {
            for (auto &Instr : MBB.GetInstructions())
                // todo: finish
                ;
        }
    }
}
