#include "BackEnd/Support.hpp"
#include "BackEnd/MachineFunction.hpp"
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
    this->Legalizer = std::make_unique<AArch64InstructionLegalizer>(this);
}

bool AArch64TargetMachine::SelectLSL(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "SLL must have 3 operands");

    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        assert(IsUInt<12>((int64_t)ImmMO->GetImmediate()) &&
               "Immediate must 12 bit wide");

        MI->SetOpcode(LSL_rri);
        return true;
    }
    else
    {
        MI->SetOpcode(LSL_rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectLSR(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "SLR must have 3 operands");

    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        assert(IsUInt<12>((int64_t)ImmMO->GetImmediate()) &&
               "Immediate must 12 bit wide");

        MI->SetOpcode(LSR_rri);
        return true;
    }
    else
    {
        MI->SetOpcode(LSR_rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectAdd(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "Add must have 3 operands");

    if (auto Symbol = MI->GetOperand(2); Symbol->IsGlobalSymbol())
    {
        MI->SetOpcode(ADD_rri);
        return true;
    }

    // If last operand is an immediate then selec "addi";
    else if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
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

bool AArch64TargetMachine::SelectDivU(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "DIVU must have 3 operands");

    // If last operand is an immediate then select "addi"
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        assert(!"Immediate not supported");
    }
    else
    {
        MI->SetOpcode(UDIV_rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectMod(MachineInstruction *MI)
{
    assert(!"MOD not supported");
    return false;
}

bool AArch64TargetMachine::SelectModU(MachineInstruction *MI)
{
    assert(!"MODU not supported");
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

bool AArch64TargetMachine::SelectZExt(MachineInstruction *MI) { return SelectSExt(MI); }

bool AArch64TargetMachine::SelectSExt(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "SEXT must have 2 operands");

    if (MI->GetOperand(1)->IsImmediate())
    {
        MI->SetOpcode(MOV_ri);
        return true;
    }
    else if (MI->GetOperand(1)->GetType().GetBitWidth() == 8)
    {
        MI->SetOpcode(SXTB);
        return true;
    }
    else if (MI->GetOperand(1)->GetType().GetBitWidth() == 32)
    {
        MI->SetOpcode(SXTW);
        return true;
    }

    assert(!"Unimplemented!");
    return false;
}

bool AArch64TargetMachine::SelectZExtLoad(MachineInstruction *MI)
{
    assert((MI->GetOperandsNumber() == 3) && "ZEXT_LOAD must have 3 operands");

    auto SourceSize = MI->GetOperand(1)->GetType().GetBitWidth();
    MI->RemoveOperand(1);

    if (SourceSize == 8)
    {
        MI->SetOpcode(LDRB);
        return true;
    }

    MI->SetOpcode(LDR);
    return true;
}

bool AArch64TargetMachine::SelectTrunc(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "SEXT must have 2 operands");

    if (MI->GetOperand(0)->GetType().GetBitWidth() == 8)
    {
        // if the operand i san immediate
        if (MI->GetOperand(1)->IsImmediate())
        {
            // then calculate the truncated immediate value and emit mov
            int64_t ResultIMM = MI->GetOperand(1)->GetImmediate() & 0xFFu;
            MI->GetOperand(1)->SetValue(ResultIMM);
            MI->SetOpcode(MOV_ri);
        }
        else
        {
            MI->SetOpcode(AND_rri);
            MI->AddImmediate(0xFFu);
        }

        if (MI->GetOperand(0)->GetSize() < 32)
            MI->GetOperand(0)->GetTypeRef().SetBitWidth(32);
        return true;
    }

    // in cases like
    //      TRUNC  %dst(s32), %src(s64)
    // for arm only a "mov" instruction is needed, but for $src the W subregister
    // of the X register should be used, this will be enforced in a later pass
    if (MI->GetOperand(0)->GetType().GetBitWidth() == 32 &&
        MI->GetOperand(1)->GetType().GetBitWidth() == 64)
    {
        if (!MI->GetOperand(1)->IsImmediate())
        {
            MI->SetOpcode(MOV_rr);
            return true;
        }
    }

    assert(!"Unimplemented");
    return false;
}

bool AArch64TargetMachine::SelectMov(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "MOV must have exactly 2 operands");

    if (MI->GetOperand(1)->IsImmediate())
    {
        assert(IsInt<16>(MI->GetOperand(1)->GetImmediate()) && "Invalid immediate value");
        MI->SetOpcode(MOV_ri);
    }
    else
        MI->SetOpcode(MOV_rr);

    return true;
}

bool AArch64TargetMachine::SelectLoadImm(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "LOAD_IMM must have exactly 2 operands");

    assert(MI->GetOperand(1)->IsImmediate() && "Operand #2 must be an immediate");
    assert(IsInt<16>(MI->GetOperand(1)->GetImmediate()) && "Ivalid immediate value");

    MI->SetOpcode(MOV_ri);
    return true;
}

bool AArch64TargetMachine::SelectLoad(MachineInstruction *MI)
{
    assert((MI->GetOperandsNumber() == 2 || MI->GetOperandsNumber() == 3) &&
           "LOAD must have 2 or 3 operands");

    auto OprType = MI->GetOperand(0)->GetType();
    if (OprType.GetBitWidth() == 8 && !OprType.IsPointer())
    {
        MI->SetOpcode(LDRB);
        if (MI->GetOperand(0)->GetSize() < 32)
            MI->GetOperand(0)->GetTypeRef().SetBitWidth(32);

        return true;
    }

    if (MI->GetOperand(1)->IsStackAccess())
    {
        auto StackSlotID = MI->GetOperand(1)->GetSlot();
        auto ParentFunc  = MI->GetParent()->GetParent();
        auto Size        = ParentFunc->GetStackObjectSize(StackSlotID);

        if (Size == 1)
        {
            MI->SetOpcode(LDRB);
            if (MI->GetOperand(0)->GetSize() < 32)
                MI->GetOperand(0)->GetTypeRef().SetBitWidth(32);
            return true;
        }
        else if (Size == 4)
        {
            MI->SetOpcode(LDR);
            return true;
        }
    }

    MI->SetOpcode(LDR);
    return true;
}

bool AArch64TargetMachine::SelectStore(MachineInstruction *MI)
{
    assert((MI->GetOperandsNumber() == 2 || MI->GetOperandsNumber() == 3) &&
           "STORE must have 2 or 3 operands");

    MachineFunction *ParentMF {nullptr};

    if (MI->GetOperandsNumber() == 2)
        ParentMF = MI->GetParent()->GetParent();

    auto BeginOP = MI->GetOperand(0);
    auto EndOp   = *MI->GetOperand(MI->GetOperandsNumber() - 1);

    if (EndOp.GetType().GetBitWidth() == 8 ||
        (MI->GetOperandsNumber() == 2 && ParentMF->IsStackSlot(BeginOP->GetSlot()) &&
         ParentMF->GetStackObjectSize(BeginOP->GetSlot()) == 1))
    {
        MI->SetOpcode(STRB);
        return true;
    }

    MI->SetOpcode(STR);
    return true;
}

bool AArch64TargetMachine::SelectStackAddress(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "StackAddress must have 2 operands");

    MI->SetOpcode(ADD_rri);

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
            case MachineInstruction::EQ: MI->SetOpcode(BEQ); break;
            case MachineInstruction::NE: MI->SetOpcode(BNE); break;
            case MachineInstruction::LE: MI->SetOpcode(BLE); break;
            case MachineInstruction::LT: MI->SetOpcode(BLT); break;
            case MachineInstruction::GE: MI->SetOpcode(BGE); break;
            case MachineInstruction::GT: MI->SetOpcode(BGT); break;
            default: assert(!"Unimplemented");
        }
        MI->RemoveOperand(0);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectJump(MachineInstruction *MI)
{
    MI->SetOpcode(B);
    return true;
}

bool AArch64TargetMachine::SelectCall(MachineInstruction *MI)
{
    MI->SetOpcode(BL);
    return true;
}

bool AArch64TargetMachine::SelectRet(MachineInstruction *MI)
{
    MI->SetOpcode(RET);
    return true;
}
