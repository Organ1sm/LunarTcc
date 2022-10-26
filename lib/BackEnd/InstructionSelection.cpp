#include "BackEnd/InstructionSelection.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/TargetMachine.hpp"

void InstructionSelection::InstrSelect()
{
    for (auto &MFunc : MIRM->GetFunctions())
    {
        for (auto &MBB : MFunc.GetBasicBlocks())
            for (auto &Instr : MBB.GetInstructions())
                // Skip selection if already selected
                if (!Instr.IsAlreadySelected())
                    this->TM->SelectInstruction(&Instr);
    }
}
