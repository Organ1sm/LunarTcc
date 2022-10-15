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
                this->TM->SelectInstruction(&Instr);
    }
}
