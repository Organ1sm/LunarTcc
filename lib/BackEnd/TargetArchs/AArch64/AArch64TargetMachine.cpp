#include "BackEnd/Support.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64TargetMachine.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64InstructionDefinitions.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64InstructionLegalizer.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64RegisterInfo.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64TargetABI.hpp"
#include <cassert>
#include <cstdint>
#include <memory>

using namespace AArch64;

AArch64TargetMachine::AArch64TargetMachine()
{
    this->RegInfo   = std::make_unique<AArch64RegisterInfo>();
    this->ABI       = std::make_unique<AArch64TargetABI>(this->RegInfo.get());
    this->InstrDefs = std::make_unique<AArch64InstructionDefinitions>();
    this->Legalizer = std::make_unique<AArch64InstructionLegalizer>();
}

bool AArch64TargetMachine::SelectAdd(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "Add must have 3 operands");

    // If last operand is an immediate then selec "addi";
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        // FIXME: Since currently ADD used for adjusting the stack in the prolog,
        // therefore its possible that the immediate is negative. In that case for
        // now we just convert the ADD into a SUB and call select on that.
        auto Value = static_cast<int64_t>(ImmMO->GetImmediate());
        if (Value < 0)
        {
            MI->SetOpcode(SUB_rri);
            MI->GetOperand(2)->SetValue(Value * -1);

            return SelectSub(MI);
        }

        assert(IsUInt<12>(Value) && "Immediate must be 12 bit wide.");

        // TODO: check if the register operands are valid, like i32 and not f32
        MI->SetOpcode(ADD_rri);

        return true;
    }
    else
    {
        MI->SetOpcode(ADD_rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectSub(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "SUB must have 3 operands");

    // If last operand is an immediate then select "SUB_rri"
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        auto Value = static_cast<int64_t>(ImmMO->GetImmediate());
        assert(IsUInt<12>(Value) && "Immediate must be 12 bit wide");

        // TODO: see ADD comment
        MI->SetOpcode(SUB_rri);
        return true;
    }
    // else try to select "SUB_rrr"
    else
    {
        MI->SetOpcode(SUB_rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectMul(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "MUL must have 3 operands");

    // If last operand is an immediate then select "MUL_rri"
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        auto Value = static_cast<int64_t>(ImmMO->GetImmediate());
        assert(IsUInt<12>(Value) && "Immediate must be 12 bit wide");

        // TODO: see ADD comment
        MI->SetOpcode(MUL_rri);
        return true;
    }
    else
    {
        MI->SetOpcode(MUL_rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectDiv(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "DIV must have 3 operands");

    // If last operand is an immediate then select "addi"
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        auto Value = static_cast<int64_t>(ImmMO->GetImmediate());
        assert(IsUInt<12>(Value) && "Immediate must be 12 bit wide");

        // TODO: see ADD comment
        MI->SetOpcode(SDIV_rri);
        return true;
    }
    else
    {
        MI->SetOpcode(SDIV_rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectMod(MachineInstruction *MI)
{
    assert(!"MOD not supported");
    return false;
}

bool AArch64TargetMachine::SelectCmp(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "CMP must have 3 operands");

    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        MI->SetOpcode(CMP_ri);
        // remove the destination hence the implicit condition register is
        // overwritten
        MI->RemoveOperand(0);
        return true;
    }
    else
    {
        MI->SetOpcode(CMP_rr);
        // remove the destination hence the implicit condition register is
        // overwritten
        MI->RemoveOperand(0);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectLoad(MachineInstruction *MI)
{
    assert((MI->GetOperandsNumber() == 2 || MI->GetOperandsNumber() == 3)
           && "LOAD must have 2 or 3 operands");

    MI->SetOpcode(LDR);
    return true;
}

bool AArch64TargetMachine::SelectStore(MachineInstruction *MI)
{
    assert((MI->GetOperandsNumber() == 2 || MI->GetOperandsNumber() == 3)
           && "STORE must have 2 or 3 operands");

    MI->SetOpcode(STR);
    return true;
}

bool AArch64TargetMachine::SelectBranch(MachineInstruction *MI)
{
    // 1) Get the preceding instruction if exists
    // 2) If a compare then use its condition to determine the condition code
    //    for this branch
    // FIXME: not sure if for a branch it is REQUIRED to have a compare before
    //        it or its just optional (likely its optional)
    auto &BBInstructions            = MI->GetParent()->GetInstructions();
    MachineInstruction *PrecedingMI = nullptr;

    for (size_t i = 0; i < BBInstructions.size(); i++)
        // find the current instruction index
        if (&BBInstructions[i] == MI && i > 0)
        {
            PrecedingMI = &BBInstructions[i - 1];
            break;
        }

    if (MI->IsFallThroughBranch())
    {
        assert(PrecedingMI && "For now assume a preceding cmp instruction");

        // choose the appropriate conditional branch based on the cmp type
        switch (PrecedingMI->GetRelation())
        {
            case MachineInstruction::LE:
                MI->SetOpcode(BLE);
                MI->RemoveOperand(0);
                return true;
            default:
                assert(!"Unimplemented");
        }
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectJump(MachineInstruction *MI)
{
    MI->SetOpcode(B);
    return true;
}

bool AArch64TargetMachine::SelectRet(MachineInstruction *MI)
{
    MI->SetOpcode(RET);
    return true;
}