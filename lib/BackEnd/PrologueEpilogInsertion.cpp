#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/PrologueEpilogInsertion.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/Support.hpp"
#include <cassert>


MachineInstruction
    PrologueEpilogInsertion::CreateAddInstruction(int64_t StackAdjustmentSize)
{
    MachineInstruction Add(MachineInstruction::Add, nullptr);

    auto SPReg     = TM->GetRegInfo()->GetStackRegister();
    auto SPRegSize = TM->GetRegInfo()->GetRegisterByID(SPReg)->GetBitWidth();

    Add.AddRegister(SPReg, SPRegSize);
    Add.AddRegister(SPReg, SPRegSize);
    Add.AddImmediate(StackAdjustmentSize);

    if (!TM->SelectInstruction(&Add))
    {
        assert(!"Unable to select instruction");
        return MachineInstruction();
    }

    return Add;
}

void PrologueEpilogInsertion::InsertStackAdjustmentUpward(MachineFunction &Func)
{
    unsigned StackAlignment = TM->GetABI()->GetStackAlignment();
    int64_t StackAdjustmentSize =
        GetNextAlignedValue(Func.GetStackFrameSize(), StackAlignment) * -1;

    MachineInstruction AddToSP = CreateAddInstruction(StackAdjustmentSize);

    Func.GetBasicBlocks().front().InsertInstrToFront(AddToSP);
}

void PrologueEpilogInsertion::InsertStackAdjustmentDownward(MachineFunction &Func)
{
    unsigned StackAlignment = TM->GetABI()->GetStackAlignment();
    int64_t StackAdjustmentSize =
        GetNextAlignedValue(Func.GetStackFrameSize(), StackAlignment);

    MachineInstruction AddToSP = CreateAddInstruction(StackAdjustmentSize);

    // NOTE: for now assuming that there is only one ret instruction and its
    // the last one of the last basic block, so we want to insert above it
    auto &LastBB = Func.GetBasicBlocks().back();

    LastBB.InsertInstr(AddToSP, LastBB.GetInstructions().size() - 1);
}

void PrologueEpilogInsertion::Run()
{
    for (auto &Func : MIRM->GetFunctions())
    {
        /// TODO: handle spilled objects when spilling is implemented
        InsertStackAdjustmentUpward(Func);
        InsertStackAdjustmentDownward(Func);
    }
}
