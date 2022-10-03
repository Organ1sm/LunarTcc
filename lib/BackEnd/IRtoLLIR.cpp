#include <cassert>
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/IRtoLLIR.hpp"
#include "BackEnd/LowLevelType.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/Value.hpp"


MachineOperand GetMachineOperandFromValue(Value *Val)
{
    MachineOperand Result;

    if (Val->IsRegister())
    {
        Result.SetTypeToVirtualRegister();
        Result.SetValue(Val->GetID());
    }
    else if (Val->IsParameter())
    {
        // FIXME: Only handling int params now, handle others too
        // And add type to registers and others too
        Result.SetTypeToParameter();
        Result.SetType(LowLevelType::CreateInt(32));
        Result.SetValue(Val->GetID());
    }
    else if (Val->IsConstant())
    {
        auto C = dynamic_cast<Constant *>(Val);
        assert(!C->IsFPConst() && "TODO");
        Result.SetTypeToIntImm();
        Result.SetValue(C->GetIntValue());
    }
    else
    {
        assert(!"Unhandled MachineOperand case.");
    }

    return Result;
}

