#include "BackEnd/MachineOperand.hpp"
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

void ExtendRegSize(MachineOperand *MO, uint8_t BitWidth = 32)
{
    if (MO->GetSize() < 32)
        MO->GetTypeRef().SetBitWidth(BitWidth);
}

bool SelectThreeAddressFPInstuction(MachineInstruction *MI, Opcodes rrr)
{
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        return false;
    }
    else
    {
        MI->SetOpcode(rrr);
        return true;
    }
}

/// Materialize the given constant before the MI instruction.
MachineInstruction *AArch64TargetMachine::MaterializeConstant(MachineInstruction *MI,
                                                              const uint64_t Constant,
                                                              MachineOperand &VReg)
{
    auto MBB = MI->GetParent();
    auto Reg = MBB->GetParent()->GetNextAvailableVirtualRegister();


    // define its size by the size of the destination register of the MI for now
    // and assume an integer constant
    VReg = MachineOperand::CreateVirtualRegister(Reg);
    VReg.SetRegClass(RegInfo->GetRegisterClass(MI->GetOperand(0)->GetSize(), false));

    std::vector<MachineInstruction> MIs;

    MachineInstruction MOV;
    MOV.SetOpcode(MOV_ri);
    MOV.AddOperand(VReg);
    MOV.AddImmediate(Constant & 0xffffu);
    MIs.push_back(MOV);

    if (!IsInt<16>(Constant) && IsInt<32>(Constant))
    {
        MachineInstruction MOVK;
        MOVK.SetOpcode(MOVK_ri);
        MOVK.AddOperand(VReg);
        MOVK.AddImmediate((uint32_t)Constant >> 16u);    // upper 16 bit
        MOVK.AddImmediate(16);                           // left shift amount

        MIs.push_back(MOVK);
    }

    return &(*MBB->InsertBefore(std::move(MIs), MI));
}

bool AArch64TargetMachine::SelectThreeAddressInstruction(MachineInstruction *MI,
                                                         Opcodes rrr,
                                                         Opcodes rri,
                                                         unsigned ImmSize)
{
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        if (IsInt(ImmMO->GetImmediate(), ImmSize))
        {
            MI->SetOpcode(rri);
            return true;
        }

        MachineOperand VReg;
        MI = MaterializeConstant(MI, ImmMO->GetImmediate(), VReg);
        MI->SetOpcode(rrr);
        MI->RemoveOperand(2);
        MI->AddOperand(VReg);

        return true;
    }
    else if (MI->GetOperand(2)->IsRegister() || MI->GetOperand(2)->IsVirtualReg())
    {
        MI->SetOpcode(rrr);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectAnd(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "And must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressInstruction(MI, AND_rrr, AND_rri))
        assert(!"Cannot select And instruction.");

    return true;
}

bool AArch64TargetMachine::SelectOr(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "Or must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressInstruction(MI, ORR_rrr, ORR_rri))
        assert(!"Cannot select And instruction.");

    return true;
}

bool AArch64TargetMachine::SelectXOR(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "XOR must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));


    // in case of bitwise not
    if (MI->GetOperand(2)->IsImmediate() && MI->GetOperand(2)->GetImmediate() == -1)
    {
        MI->RemoveOperand(2);
        MI->SetOpcode(MVN_rr);

        return true;
    }

    if (!SelectThreeAddressInstruction(MI, EOR_rrr, EOR_rri))
        assert(!"Cannot select Xor instruction.");

    return true;
}

bool AArch64TargetMachine::SelectLSL(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "SLL must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressInstruction(MI, LSL_rrr, LSL_rri))
        assert(!"Cannot select LSL instruction.");

    return true;
}

bool AArch64TargetMachine::SelectLSR(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "SLR must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressInstruction(MI, LSR_rrr, LSR_rri))
        assert(!"Cannot select LSR");

    return true;
}

bool AArch64TargetMachine::SelectAdd(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "Add must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

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

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

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

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

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

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

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

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

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

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        if (IsInt<12>(ImmMO->GetImmediate()))
            MI->SetOpcode(CMP_ri);
        else
        {
            MachineOperand Reg;
            MI = MaterializeConstant(MI, ImmMO->GetImmediate(), Reg);

            MI->SetOpcode(CMP_rr);
            MI->RemoveOperand(2);
            MI->AddOperand(Reg);
        }

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

bool AArch64TargetMachine::SelectCmpF(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "CMP must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        MI->SetOpcode(FCMP_ri);
        MI->RemoveOperand(0);
        return true;
    }
    else
    {
        MI->SetOpcode(FCMP_rr);
        MI->RemoveOperand(0);
        return true;
    }

    return false;
}

bool AArch64TargetMachine::SelectAddF(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "ADDF must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressFPInstuction(MI, FADD_rrr))
        assert(!"Immedaite operand is not allowed for FADD");

    return true;
}

bool AArch64TargetMachine::SelectSubF(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "SUBF must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressFPInstuction(MI, FSUB_rrr))
        assert(!"Immedaite operand is not allowed for FSUB");

    return true;
}

bool AArch64TargetMachine::SelectMulF(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "MULF must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressFPInstuction(MI, FMUL_rrr))
        assert(!"Immedaite operand is not allowed for FMUL");

    return true;
}

bool AArch64TargetMachine::SelectDivF(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "DIVF must have 3 operands");

    ExtendRegSize(MI->GetOperand(0));
    ExtendRegSize(MI->GetOperand(1));

    if (!SelectThreeAddressFPInstuction(MI, FDIV_rrr))
        assert(!"Immedaite operand is not allowed for FDIV");

    return true;
}

bool AArch64TargetMachine::SelectIntToFloat(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "ITOF must have 2 operands");

    ExtendRegSize(MI->GetOperand(0));

    MI->SetOpcode(SCVTF_rr);
    return true;
}

bool AArch64TargetMachine::SelectFloatToInt(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "FTOI must have 2 operands");

    ExtendRegSize(MI->GetOperand(0));

    MI->SetOpcode(FCVTZS_rr);
    return true;
}


bool AArch64TargetMachine::SelectZExt(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "ZEXT must have 2 operands");

    ExtendRegSize(MI->GetOperand(0));

    if (MI->GetOperand(1)->IsImmediate())
    {
        MI->SetOpcode(MOV_ri);
        return true;
    }
    else if (MI->GetOperand(1)->GetType().GetBitWidth() == 8)
    {
        MI->SetOpcode(UXTB);
        return true;
    }
    else if (MI->GetOperand(1)->GetType().GetBitWidth() == 16)
    {
        MI->SetOpcode(UXTH);
        return true;
    }
    else if (MI->GetOperand(1)->GetType().GetBitWidth() == 32)
    {
        MI->SetOpcode(UXTW);
        return true;
    }
    else if (MI->GetOperand(1)->GetType().GetBitWidth() == 64)
    {
        MI->SetOpcode(MOV_rr);
        return true;
    }

    assert(!"Unimplemented!");
    return false;
}


bool AArch64TargetMachine::SelectSExt(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "SEXT must have 2 operands");

    ExtendRegSize(MI->GetOperand(0));

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
    else if (MI->GetOperand(1)->GetType().GetBitWidth() == 16)
    {
        MI->SetOpcode(SXTH);
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

        ExtendRegSize(MI->GetOperand(0));

        return true;
    }
    else if (MI->GetOperand(0)->GetType().GetBitWidth() == 16)
    {
        // if the operand i san immediate
        if (MI->GetOperand(1)->IsImmediate())
        {
            // then calculate the truncated immediate value and emit mov
            int64_t ResultIMM = MI->GetOperand(1)->GetImmediate() & 0xFFFFu;
            MI->GetOperand(1)->SetValue(ResultIMM);
            MI->SetOpcode(MOV_ri);
        }
        else
        {
            MI->SetOpcode(AND_rri);
            MI->AddImmediate(0xFFFFu);
        }
        // For now set the result's bitwidth to 32 if its less than that, otherwise
        // no register could be selected for it.
        // FIXME: Enforce this in the legalizer maybe (check LLVM for clues)
        ExtendRegSize(MI->GetOperand(0));

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

bool AArch64TargetMachine::SelectMovF(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "MOVF must have exactly 2 operands");

    if (MI->GetOperand(1)->IsImmediate())
    {
        MI->SetOpcode(FMOV_ri);
    }
    else
        MI->SetOpcode(FMOV_rr);

    return true;
}

bool AArch64TargetMachine::SelectLoadImm(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "LOAD_IMM must have exactly 2 operands");
    assert(MI->GetOperand(1)->IsImmediate() && "Operand #2 must be an immediate");

    auto Imm = MI->GetOperand(1)->GetImmediate();

    ExtendRegSize(MI->GetOperand(0));

    if (IsInt<16>(Imm))
        MI->SetOpcode(MOV_ri);
    else if (IsInt<32>(Imm))
    {
        MachineBasicBlock *MBB = MI->GetParent();

        MI->SetOpcode(MOV_ri);

        // keep lower 16 bit
        MI->GetOperand(1)->SetValue(Imm & 0xffffu);

        MachineInstruction MOVK;
        MOVK.SetOpcode(MOVK_ri);
        MOVK.AddOperand(*MI->GetOperand(0));

        // upper 16 bit
        MOVK.AddImmediate(((uint64_t)Imm) >> 16u);

        /// left shift amount
        MOVK.AddImmediate(16);

        MBB->InsertAfter(MOVK, MI);
    }
    else
    {
        assert(!"Invalid immediate value");
    }

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
        ExtendRegSize(MI->GetOperand(0));

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
            ExtendRegSize(MI->GetOperand(0));

            return true;
        }
        else if (Size == 2)
        {
            MI->SetOpcode(LDRH);
            ExtendRegSize(MI->GetOperand(0));

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
    }
    else if (EndOp.GetType().GetBitWidth() == 16 ||
             (MI->GetOperandsNumber() == 2 && ParentMF->IsStackSlot(BeginOP->GetSlot()) &&
              ParentMF->GetStackObjectSize(BeginOP->GetSlot()) == 2))
    {
        MI->SetOpcode(STRH);
    }
    else
    {
        MI->SetOpcode(STR);
    }

    ExtendRegSize(MI->GetOperand(1));
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

            default: MI->SetOpcode(BEQ);
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
