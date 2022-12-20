#include "BackEnd/TargetInstructionLegalizer.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/Support.hpp"
#include <cassert>

/// Materialize @MI instruction @Index-th operand - which must be an immediate -
/// into a virtual register by issuing a LOAD_IMM
static bool MaterializeImmOperand(MachineInstruction *MI, const size_t Index)
{
    assert(Index < MI->GetOperandsNumber());
    assert(MI->GetOperand(Index)->IsImmediate());

    auto ParentBB = MI->GetParent();

    auto LI      = MachineInstruction(MachineInstruction::LoadImm, ParentBB);
    auto DestReg = ParentBB->GetParent()->GetNextAvailableVirtualRegister();

    LI.AddVirtualRegister(DestReg, MI->GetOperand(Index)->GetSize());
    LI.AddOperand(*MI->GetOperand(Index));

    // replace the immediate operand with the destination of the immediate load
    MI->RemoveOperand(Index);
    MI->InsertOperand(
        Index,
        MachineOperand::CreateVirtualRegister(DestReg, MI->GetOperand(0)->GetSize()));

    // insert after modifying MI, otherwise MI would became invalid
    ParentBB->InsertBefore(std::move(LI), MI);

    return true;
}

bool TargetInstructionLegalizer::IsRelSupported(MachineInstruction::CmpRelation Rel) const
{
    return UnSupportedRelations.count(Rel) == 0;
}

MachineInstruction
    TargetInstructionLegalizer::CreateThreeAddrMI(MachineInstruction::OperationCode Kind,
                                                  MachineOperand First,
                                                  MachineOperand Second,
                                                  MachineOperand Third)
{
    auto MI = MachineInstruction(Kind, PMBB);

    MI.AddOperand(First);
    MI.AddOperand(Second);
    MI.AddOperand(Third);

    return MI;
}

static auto InsertAfterMI(MachineBasicBlock *MBB,
                          MachineInstruction MI,
                          MachineBasicBlock::InstructionList::iterator InsertTo)
    -> MachineBasicBlock::InstructionList::iterator
{
    return MBB->InsertAfter(std::move(MI), &*InsertTo);
}

static auto InsertAfterMI(MachineBasicBlock *MBB,
                          std::vector<MachineInstruction> MIS,
                          MachineBasicBlock::InstructionList::iterator InsertTo)
{
    for (auto MI : MIS)
    {
        InsertTo = InsertAfterMI(MBB, std::move(MI), InsertTo);
    }

    return InsertTo;
}

unsigned TargetInstructionLegalizer::GetNextAvailVReg()
{
    return this->PMF->GetNextAvailableVirtualRegister();
}

void TargetInstructionLegalizer::InitCurrProcessMFB(MachineInstruction *MI)
{
    PMBB = MI->GetParent();
    PMF  = PMBB->GetParent();
}

/// The following
///     ADDS    %dst, %carry, %src1, %src2
///
///  is replaced with
///     ADD     %dst, %src1, %src2
///     CMP.LT  %carry, %dst, %src2
bool TargetInstructionLegalizer::ExpandAddS(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 4 && "ADDS must have exactly 4 operands");
    InitCurrProcessMFB(MI);

    auto Dest  = *MI->GetOperand(0);
    auto Carry = *MI->GetOperand(1);
    auto Src1  = *MI->GetOperand(2);
    auto Src2  = *MI->GetOperand(3);

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Carry.IsVirtualReg() && "Carry must be a virtual register");
    assert(Src1.IsVirtualReg() && "Source #1 must be a virtual register");
    assert((Src2.IsVirtualReg() || Src2.IsImmediate()) &&
           "Source #2 must be a virtual register or an immediate");

    auto Add = CreateThreeAddrMI(MachineInstruction::Add, Dest, Src1, Src2);
    auto Cmp = CreateThreeAddrMI(MachineInstruction::Cmp, Carry, Dest, Src2);
    Cmp.SetAttributes(MachineInstruction::LT);

    auto InsertTo = PMBB->ReplaceInstr(std::move(Add), &*MI);
    InsertAfterMI(PMBB, std::move(Cmp), InsertTo);

    return true;
}

/// The following
///     ADDC    %dst, %src1, %src2, %carry
///
///  is replaced with
///     ADD     %temp, %src1, %src2
///     ADD     %dst, %temp, %carry
bool TargetInstructionLegalizer::ExpandAddC(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 4 && "ADDC must have exactly 4 operands");
    InitCurrProcessMFB(MI);

    auto Dest  = *MI->GetOperand(0);
    auto Src1  = *MI->GetOperand(1);
    auto Src2  = *MI->GetOperand(2);
    auto Carry = *MI->GetOperand(3);

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Carry.IsVirtualReg() && "Carry must be a virtual register");
    assert(Src1.IsVirtualReg() && "Source #1 must be a virtual register");
    assert((Src2.IsVirtualReg() || Src2.IsImmediate()) &&
           "Source #2 must be a virtual register or an immediate");

    auto Temp = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto Add  = CreateThreeAddrMI(MachineInstruction::Add, Temp, Src1, Src2);
    auto Addc = CreateThreeAddrMI(MachineInstruction::Add, Dest, Temp, Carry);

    auto InsertTo = PMBB->ReplaceInstr(std::move(Add), &*MI);
    InsertAfterMI(PMBB, std::move(Addc), InsertTo);

    return true;
}

/// Expand 64 bit ADD to equivalent calculation using 32 bit additions
///     ADD     %dst(s64), %src1(s64), %src2(s64)
///
///  is replaced with
///     SPLIT   %src1_lo(s32), %src1_hi(s32), %src1(s64)
///     SPLIT   %src2_lo(s32), %src2_hi(s32), %src2(s64)
///     ADDS    %adds, %carry, %src1_lo, %src2_lo
///     ADDC    %addc, %src1_hi, %src2_hi, %carry
///     MERGE   %dst(s64), %adds(s32), %addc(s32)
bool TargetInstructionLegalizer::ExpandAdd(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "ADD must have exactly 3 operands");
    InitCurrProcessMFB(MI);

    auto Dest = *MI->GetOperand(0);
    auto Src1 = *MI->GetOperand(1);
    auto Src2 = *MI->GetOperand(2);

    // Better to materialize the constant in this case
    // TODO: If the immediate fits into s32 then the generated code could
    // certainly be smaller. Its pointless to treat its as s64. Improve it.
    if (Src2.IsImmediate())
    {
        // Make sure that the operand is also has the same size as
        // the destination
        // TODO: This should be already the case
        MI->GetOperand(2)->SetSize(Dest.GetSize());
        Src2.SetSize(Dest.GetSize());
        return MaterializeImmOperand(MI, 2);
    }

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Src1.IsVirtualReg() && "Operand #1 must be a virtual register");
    assert(Src2.IsVirtualReg() && "Operand #2 must be a virtual register");

    auto Src1Lo = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src1Hi = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Lo = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Hi = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto SplitSrc1 = CreateThreeAddrMI(MachineInstruction::Split, Src1Lo, Src1Hi, Src1);
    auto SplitSrc2 = CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2);

    auto AddLoDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Carry    = MachineOperand::CreateVirtualRegister(GetNextAvailVReg(), 1);
    auto AddHiDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto Adds = CreateThreeAddrMI(MachineInstruction::AddS, AddLoDst, Carry, Src1Lo);
    Adds.AddOperand(Src2Lo);

    auto Addc = CreateThreeAddrMI(MachineInstruction::AddC, AddHiDst, Src1Hi, Src2Hi);
    Addc.AddOperand(Carry);

    auto InsertTo = PMBB->ReplaceInstr(std::move(SplitSrc1), MI);
    InsertTo      = InsertAfterMI(PMBB, SplitSrc2, InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Adds), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Addc), InsertTo);

    auto Merge = CreateThreeAddrMI(MachineInstruction::Merge, Dest, AddLoDst, AddHiDst);
    InsertAfterMI(PMBB, Merge, InsertTo);

    return true;
}

/// Expand 64 bit MUL to equivalent calculation using 32 bit muls and adds
///     MUL %dst(s64), %src1(s64), %src2(s64)
///
///  is replaced with
///     SPLIT   %src1_lo(s32), %src1_hi(s32), %src1(s64)
///     SPLIT   %src2_lo(s32), %src2_hi(s32), %src2(s64)
///     MUL     %mul1, %src1_hi, %src2_lo
///     MUL     %mul2, %src2_hi, %src1_lo
///     ADD     %add1, %mul1, %mul2
///     MULHU   %mulhu, %src1_lo, %src2_lo
///     MUL     %dst_lo, %src1_lo, %src2_lo
///     ADD     %dst_hi, %add1, %mulhu
///     MERGE   %dst(s64), %dst_lo(s32), %dst_hi(s32)
bool TargetInstructionLegalizer::ExpandMul(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "MUL must have exactly 3 operands");
    InitCurrProcessMFB(MI);

    auto Dest = *MI->GetOperand(0);
    auto Src1 = *MI->GetOperand(1);
    auto Src2 = *MI->GetOperand(2);

    // If the expansion was requested because the last operand is immediate
    // and the target is not supporting that
    if (Src2.IsImmediate() && Dest.GetSize() <= TM->GetPointerSize())
        return MaterializeImmOperand(MI, 2);

    if (Src2.IsImmediate())
        return MaterializeImmOperand(MI, 2);

    assert(Dest.GetSize() == 64 && Src1.GetSize() == Src2.GetSize() &&
           Src1.GetSize() == Dest.GetSize());

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Src1.IsVirtualReg() && "Operand #1 must be a virtual register");
    assert(Src2.IsVirtualReg() && "Operand #2 must be a virtual register");

    auto Src1Lo   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src1Hi   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Lo   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Hi   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Mul1Dst  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Mul2Dst  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Add1Dst  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto MulhuDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto DstLo    = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto DstHi    = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto SplitSrc1 = CreateThreeAddrMI(MachineInstruction::Split, Src1Lo, Src1Hi, Src1);
    auto SplitSrc2 = CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2);
    auto Mul1      = CreateThreeAddrMI(MachineInstruction::Mul, Mul1Dst, Src1Hi, Src2Lo);
    auto Mul       = CreateThreeAddrMI(MachineInstruction::Mul, Mul2Dst, Src2Hi, Src1Lo);
    auto Add1  = CreateThreeAddrMI(MachineInstruction::Add, Add1Dst, Mul1Dst, Mul2Dst);
    auto Mulhu = CreateThreeAddrMI(MachineInstruction::MulHU, MulhuDst, Src1Lo, Src2Lo);
    auto Mul3  = CreateThreeAddrMI(MachineInstruction::Mul, DstLo, Src1Lo, Src2Lo);
    auto Add2  = CreateThreeAddrMI(MachineInstruction::Add, DstHi, Add1Dst, MulhuDst);
    auto Merge = CreateThreeAddrMI(MachineInstruction::Merge, Dest, DstLo, DstHi);

    auto InsertTo = PMBB->ReplaceInstr(std::move(SplitSrc1), MI);
    InsertTo      = InsertAfterMI(PMBB, std::move(SplitSrc2), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Mul1), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Mul), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Add1), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Mulhu), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Mul), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Add2), InsertTo);


    InsertAfterMI(PMBB, std::move(Merge), InsertTo);

    return true;
}


/// Expand 64 bit SUB to equivalent calculation using 32 bit substractions
/// The following
///     SUB     %dst(s64), %src1(s64), %src1(s64)
///
/// is replaced with
///     SPLIT   %src1_lo(s32), %src1_hi(s32), %src1(s64)
///     SPLIT   %src2_lo(s32), %src2_hi(s32), %src2(s64)
///     CMP.LT  %cmp_lo, %src1_lo, %src2_lo
///     SUB     %sub1, %src1_hi, %src2_hi
///     SUB     %sub_hi, %sub1, %cmp_lo
///     SUB     %sub_lo, %src1_lo, %src2_lo
///     MERGE   %dest(s64), %sub_lo(s32), %sub_hi(s32)
bool TargetInstructionLegalizer::ExpandSub(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "SUB must have exactly 3 operands");
    InitCurrProcessMFB(MI);

    auto Dest = *MI->GetOperand(0);
    auto Src1 = *MI->GetOperand(1);
    auto Src2 = *MI->GetOperand(2);

    if (Src1.IsImmediate())
        return MaterializeImmOperand(MI, 1);

    // Better to materialize the constant in this case
    // TODO: If the immediate fits into s32 then the generated code could
    // certainly be smaller. Its pointless to treat its as s64. Improve it.
    if (Src2.IsImmediate())
    {
        // Make sure that the operand is also has the same size as
        // the destination
        // TODO: This should be already the case
        MI->GetOperand(2)->SetSize(Dest.GetSize());
        Src2.SetSize(Dest.GetSize());
        return MaterializeImmOperand(MI, 2);
    }

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Src1.IsVirtualReg() && "Operand #1 must be a virtual register");
    assert(Src2.IsVirtualReg() && "Operand #2 must be a virtual register");

    auto Src1Lo   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src1Hi   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Lo   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Hi   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto CmpLoDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Sub1Dst  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto SubLoDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto SubHiDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto SplitSrc1 = CreateThreeAddrMI(MachineInstruction::Split, Src1Lo, Src1Hi, Src1);
    auto SplitSrc2 = CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2);
    auto CmpLo     = CreateThreeAddrMI(MachineInstruction::Cmp, CmpLoDst, Src1Lo, Src2Lo);
    auto Sub1      = CreateThreeAddrMI(MachineInstruction::Sub, Sub1Dst, Src1Hi, Src2Hi);
    auto SubLo = CreateThreeAddrMI(MachineInstruction::Sub, SubLoDst, Sub1Dst, CmpLoDst);
    auto SubHi = CreateThreeAddrMI(MachineInstruction::Sub, SubHiDst, Src1Hi, Src2Hi);
    auto Merge = CreateThreeAddrMI(MachineInstruction::Merge, Dest, SubLoDst, SubHiDst);

    CmpLo.SetAttributes(MachineInstruction::LT);

    auto InsertTo = PMBB->ReplaceInstr(std::move(SplitSrc1), MI);
    InsertTo      = InsertAfterMI(PMBB, std::move(SplitSrc2), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(CmpLo), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(Sub1), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(SubLo), InsertTo);
    InsertTo      = InsertAfterMI(PMBB, std::move(SubHi), InsertTo);

    InsertAfterMI(PMBB, std::move(Merge), InsertTo);

    return true;
}



/// The following
///     MOD %res, %num, %mod
///
/// is replaced with
///     DIV %div_res, %num, %mod
///     MUL %mul_res, %div_res, %mod
///     SUB %res, %num, %mul_res
bool TargetInstructionLegalizer::ExpandMod(MachineInstruction *MI, bool IsUnsigned)
{
    assert(MI->GetOperandsNumber() == 3 && "MOD{U} must have exactly 3 operands");
    auto ParentBB   = MI->GetParent();
    auto ParentFunc = ParentBB->GetParent();

    auto ResVReg = *MI->GetOperand(0);
    auto NumVReg = *MI->GetOperand(1);
    auto ModVReg = *MI->GetOperand(2);

    assert(ResVReg.IsVirtualReg() && "Result must be a virtual register");
    assert(NumVReg.IsVirtualReg() && "Operand #1 must be a virtual register");
    assert((ModVReg.IsVirtualReg() || ModVReg.IsImmediate()) &&
           "Operand #2 must be a virtual register or an immediate");

    auto DIVResult = ParentFunc->GetNextAvailableVirtualRegister();
    auto Opcode    = IsUnsigned ? MachineInstruction::DivU : MachineInstruction::Div;
    auto DIV       = MachineInstruction(Opcode, ParentBB);

    DIV.AddOperand(MachineOperand::CreateVirtualRegister(DIVResult));
    DIV.AddOperand(NumVReg);
    DIV.AddOperand(ModVReg);
    auto DIVIter = ParentBB->ReplaceInstr(std::move(DIV), MI);

    auto MULResult = ParentFunc->GetNextAvailableVirtualRegister();
    MachineInstruction MUL(MachineInstruction::Mul, ParentBB);

    MUL.AddOperand(MachineOperand::CreateVirtualRegister(MULResult));
    MUL.AddOperand(MachineOperand::CreateVirtualRegister(DIVResult));
    MUL.AddOperand(ModVReg);
    auto MULIter = ParentBB->InsertAfter(std::move(MUL), &*DIVIter);

    auto SUB = MachineInstruction(MachineInstruction::Sub, ParentBB);

    SUB.AddOperand(ResVReg);
    SUB.AddOperand(NumVReg);
    SUB.AddOperand(MachineOperand::CreateVirtualRegister(MULResult));
    ParentBB->InsertAfter(std::move(SUB), &*MULIter);

    return true;
}



bool TargetInstructionLegalizer::Expand(MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Cmp: return ExpandCmp(MI);

        case MachineInstruction::Add: return ExpandAdd(MI);
        case MachineInstruction::AddS: return ExpandAddS(MI);
        case MachineInstruction::AddC: return ExpandAddC(MI);
        case MachineInstruction::XOr: return ExpandXOR(MI);

        case MachineInstruction::ModU:
        case MachineInstruction::Mod:
            return ExpandMod(MI, MI->GetOpcode() == MachineInstruction::ModU);
        case MachineInstruction::Sub: return ExpandSub(MI);
        case MachineInstruction::Mul: return ExpandMul(MI);
        case MachineInstruction::Div: return ExpandDiv(MI);
        case MachineInstruction::DivU: return ExpandDivU(MI);
        case MachineInstruction::Load: return ExpandLoad(MI);
        case MachineInstruction::LoadImm: return ExpandLoadImm(MI);
        case MachineInstruction::Store: return ExpandStore(MI);
        case MachineInstruction::ZExt: return ExpandZExt(MI);
        case MachineInstruction::Trunc: return ExpandTrunc(MI);
        case MachineInstruction::GlobalAddress: return ExpandGlobalAddress(MI);

        default: break;
    }

    return false;
}
