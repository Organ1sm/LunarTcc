#include "MiddleEnd/Transforms/CSEPass.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "MiddleEnd/Transforms/Utils.hpp"
#include <map>
#include <memory>



Instruction *AliveDefinitions::GetAlreadyComputedExpression(Instruction *I)
{
    for (auto Instr : Instructions)
    {
        // If there is already an instruction with the same type, which used the
        // same operands
        if (I->GetInstructionKind() == Instr->GetInstructionKind() &&
            I->Get1stUse() == Instr->Get1stUse() && I->Get2ndUse() == Instr->Get2ndUse())
        {
            // then the value computed by I is already computed, return it's
            // defining instruction
            return Instr;
        }
    }

    return nullptr;
}

static void ProcessBB(std::unique_ptr<BasicBlock> &BB)
{
    std::map<Value *, Value *> Renamables;
    AliveDefinitions AliveDefs;

    auto &InstList = BB->GetInstructions();


    for (auto &Instr : InstList)
    {
        // Nothing to do with stack allocations or jumps.
        if (Instr->IsStackAllocation() || Instr->IsJump())
            continue;

        // call -s might clobber registers at the target level, so anything defined
        // before a call will be invalid. Although this is IR level and should not
        // care about this here, but for now it is just easier to do it now.
        if (Instr->IsCall())
        {
            AliveDefs.InvalidateAll();
            continue;
        }

        // If the current instruction computation is already done by previous
        // ones then register it as renamable
        if (auto ACE = AliveDefs.GetAlreadyComputedExpression(Instr.get()))
        {
            Renamables[Instr.get()] = ACE;
        }
        else
        {
            // Otherwise it is a newly defined expression/value so register it
            AliveDefs.InsertDefine(Instr.get());
        }
    }

    if (!Renamables.empty())
        RenameRegisters(Renamables, InstList);
}


bool CSEPass::RunOnFunction(Function &F)
{
    for (auto &BB : F.GetBasicBlocks())
        ProcessBB(BB);

    return true;
}
