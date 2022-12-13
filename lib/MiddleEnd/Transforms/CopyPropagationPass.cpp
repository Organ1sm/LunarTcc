#include "MiddleEnd/Transforms/CopyPropagationPass.hpp"
#include "FrontEnd/Support.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/Value.hpp"
#include "MiddleEnd/Transforms/Utils.hpp"
#include "fmt/core.h"
#include "fmt/format.h"
#include <memory>
#include <map>


static bool ProcessBB(std::unique_ptr<BasicBlock> &BB)
{
    std::map<Value *, Value *> Renameables;
    std::map<Value *, Value *> KnownMemoryValues;

    auto &InstrList = BB->GetInstructions();

    for (auto &Instr : InstrList)
    {
        auto InstrPtr = Instr.get();
        if (InstrPtr->IsCall())
        {
            KnownMemoryValues.clear();
            continue;
        }

        // If it is a load from a stack allocation or global variable, then register
        // the source as a know value, since it's value is now held in the
        // destination of the load
        if (InstrPtr->IsLoad())
        {
            auto Source = InstrPtr->Get1stUse();

            // If the source is null or the load instruction use an offset, then skip it
            if (Source == nullptr || InstrPtr->Get2ndUse())
                continue;

            // clang-format off
            // only checking global variables and stack-allocate, GEPs
            if (!Source->IsGlobalVar() && 
                !instanceof <StackAllocationInstruction>(Source) && 
                !instanceof <GetElemPointerInstruction>(Source))
                continue;
            // clang-format on

            // If the value is not known, the it is now
            if (KnownMemoryValues.count(Source) == 0)
                KnownMemoryValues[Source] = InstrPtr;

            // Otherwise there is already a load or store which defined this
            // stack allocation or global variable, therefore it is known and this
            // load is superflous. Register it's definition as renamable.
            else
                Renameables[InstrPtr] = KnownMemoryValues[Source];
        }

        else if (InstrPtr->IsStore())
        {
            auto Source = InstrPtr->Get2ndUse();

            // If the source is null or the store instruction is not a register, then skip
            // it
            if (Source == nullptr || !InstrPtr->Get1stUse()->IsRegister())
                continue;

            // clang-format off
            // only checking global variables and stack-allocate, GEPs
            if (!Source->IsGlobalVar() && 
                !instanceof <StackAllocationInstruction>(Source) && 
                !instanceof <GetElemPointerInstruction>(Source))
                continue;
            // clang-format on

            KnownMemoryValues[Source] =
                dynamic_cast<Instruction *>(InstrPtr->Get1stUse());
        }
    }

    if (!Renameables.empty())
    {
        RenameRegisters(Renameables, InstrList);
        return true;
    }

    return false;
}

bool CopyPropagationPass::RunOnFunction(Function &F)
{
    bool IsChanged = false;
    for (auto &BB : F.GetBasicBlocks())
    {
        IsChanged = ProcessBB(BB);
    }

    if (IsChanged)
    {
        fmt::print(FMT_STRING("{:*^60}\n\n"), " After Copy Prop Pass ");
        F.Print(true);
    }

    return true;
}
