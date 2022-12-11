#include "MiddleEnd/IR/Module.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/Transforms/DeadCodeEliminationPass.hpp"
#include "MiddleEnd/Transforms/PassManager.hpp"
#include "MiddleEnd/Transforms/CSEPass.hpp"
#include <memory>

bool PassManager::RunAll()
{
    auto CSE = std::make_unique<CSEPass>();
    auto DCE = std::make_unique<DeadCodeEliminationPass>();

    for (auto &F : IRModule->GetFunctions())
    {
        if (Optimizations.count(Optimization::CSE) != 0)
        {
            std::size_t InstNumAtStart;

            // It is an iterative process, since after CSE and DCE
            // now opportunities for CSE could arise, so running it until
            // there is no change in the instructions size between the
            // start and end of iteration.
            do
            {
                InstNumAtStart = F.GetNumOfInstructions();
                CSE->RunOnFunction(F);
                DCE->RunOnFunction(F);
            }
            while (InstNumAtStart != F.GetNumOfInstructions());
        }

        DCE->RunOnFunction(F);
    }


    return true;
}
