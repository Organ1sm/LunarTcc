#pragma once

#include "MiddleEnd/Transforms/FunctionPass.hpp"
#include <vector>

class Instruction;

/// For now do only local optimization (only optimize at basic block level).
/// The idea is to find already computed values which could be reused.
/// Below is a snippet of a part of IR basic block.
///
///  ...
/// 	loadd	$5<i32>, [$0<i32*>]
/// 	load	$6<i32>, [$2<i32*>]
/// 	add	  $7<i32>, $5<i32>, $6<i32>
/// 	store	[$4<i32*>], $7<i32>
/// 	add	  $11<i32>, $5<i32>, $6<i32>
/// 	store	[$8<i32*>], $11<i32>
///   ...
///
/// It can bee seen, that the 2nd add instruction using the same source
/// operands as the 1st. Since the IR is in SSA, therefore these source
/// operands will never change. Which means that the second add will surely
/// calculate the same value as the first one. This means where the 2nd add
/// result register $11 would be used, the first add result register $7 could be
/// used instead. This will make the 2nd add dead, since it's defined value will
/// never be used anywhere, so a subsequent dead code elimination pass can
/// delete it.
///
/// The goal of Common Subexpression Elimination is to find similar scenarios
/// as shown above and try to eliminate redundant calculations.
class CSEPass : public FunctionPass
{
  public:
    bool RunOnFunction(Function &F) override;
};

struct AliveDefinitions
{
    AliveDefinitions() = default;

    void InvalidateAll() { Instructions.clear(); }
    void InsertDefine(Instruction *I) { Instructions.push_back(I); }

    Instruction *GetAlreadyComputedExpression(Instruction *I);

    std::vector<Instruction *> Instructions;
};
