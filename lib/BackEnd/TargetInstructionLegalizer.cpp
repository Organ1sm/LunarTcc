#include "BackEnd/TargetInstructionLegalizer.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/Support.hpp"
#include <cassert>
#include <vector>

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
    for (auto &MI : MIS)
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

    auto AddLoDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Carry    = MachineOperand::CreateVirtualRegister(GetNextAvailVReg(), 1);
    auto AddHiDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto SplitSrc1 = CreateThreeAddrMI(MachineInstruction::Split, Src1Lo, Src1Hi, Src1);
    auto InsertTo  = PMBB->ReplaceInstr(std::move(SplitSrc1), MI);

    auto Adds = CreateThreeAddrMI(MachineInstruction::AddS, AddLoDst, Carry, Src1Lo);
    Adds.AddOperand(Src2Lo);

    auto Addc = CreateThreeAddrMI(MachineInstruction::AddC, AddHiDst, Src1Hi, Src2Hi);
    Addc.AddOperand(Carry);

    std::vector<MachineInstruction> MIS {
        CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2),
        std::move(Adds),
        std::move(Addc),
        CreateThreeAddrMI(MachineInstruction::Merge, Dest, AddLoDst, AddHiDst),
    };

    InsertAfterMI(PMBB, std::move(MIS), InsertTo);

    return true;
}

// Expand 64 bit XOR to equivalent calculation using 32 bit additions
///     XOR %dst(s64), %src1(s64), %src2(s64)
///
///  is replaced with
///     SPLIT   %src1_lo(s32), %src1_hi(s32), %src1(s64)
///     SPLIT   %src2_lo(s32), %src2_hi(s32), %src2(s64)
///     XOR     %xor_lo, %src1_lo, %src2_lo
///     XOR     %xor_hi, %src1_hi, %src2_hi
///     MERGE   %dst(s64), %xor_lo, %xor_hi
bool TargetInstructionLegalizer::ExpandXOR(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "XOR must have exactly 3 operands");
    InitCurrProcessMFB(MI);

    auto Dest = *MI->GetOperand(0);
    auto Src1 = *MI->GetOperand(1);
    auto Src2 = *MI->GetOperand(2);

    // Assume expansion was requested because the last operand is immediate
    // and the target is not supporting that
    if (Src2.IsImmediate())
        return MaterializeImmOperand(MI, 2);

    assert(Dest.GetSize() == 64 && Src1.GetSize() == Src2.GetSize() &&
           Src1.GetSize() == Dest.GetSize());

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Src1.IsVirtualReg() && "Operand #1 must be a virtual register");
    assert((Src2.IsVirtualReg() || Src2.IsImmediate()) &&
           "Operand #2 must be a virtual register or an immediate");

    auto Src1Lo  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src1Hi  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Lo  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Src2Hi  = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Xor1Dst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Xor2Dst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto Split1   = CreateThreeAddrMI(MachineInstruction::Split, Src1Lo, Src1Hi, Src1);
    auto InsertTo = PMBB->ReplaceInstr(std::move(Split1), MI);

    std::vector<MachineInstruction> MIS {
        CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2),
        CreateThreeAddrMI(MachineInstruction::XOr, Xor1Dst, Src1Lo, Src2Lo),
        CreateThreeAddrMI(MachineInstruction::XOr, Xor2Dst, Src1Hi, Src2Hi),
        CreateThreeAddrMI(MachineInstruction::Merge, Dest, Xor1Dst, Xor2Dst),
    };

    InsertAfterMI(PMBB, std::move(MIS), InsertTo);

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
    auto InsertTo  = PMBB->ReplaceInstr(std::move(SplitSrc1), MI);

    std::vector<MachineInstruction> MIS {
        CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2),
        CreateThreeAddrMI(MachineInstruction::Mul, Mul1Dst, Src1Hi, Src2Lo),
        CreateThreeAddrMI(MachineInstruction::Mul, Mul2Dst, Src2Hi, Src1Lo),
        CreateThreeAddrMI(MachineInstruction::Add, Add1Dst, Mul1Dst, Mul2Dst),
        CreateThreeAddrMI(MachineInstruction::MulHU, MulhuDst, Src1Lo, Src2Lo),
        CreateThreeAddrMI(MachineInstruction::Mul, DstLo, Src1Lo, Src2Lo),
        CreateThreeAddrMI(MachineInstruction::Add, DstHi, Add1Dst, MulhuDst),
        CreateThreeAddrMI(MachineInstruction::Merge, Dest, DstLo, DstHi),
    };

    InsertAfterMI(PMBB, std::move(MIS), InsertTo);

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
    auto InsertTo  = PMBB->ReplaceInstr(std::move(SplitSrc1), MI);

    auto CmpLo = CreateThreeAddrMI(MachineInstruction::Cmp, CmpLoDst, Src1Lo, Src2Lo);
    CmpLo.SetAttributes(MachineInstruction::LT);

    std::vector<MachineInstruction> MIS {
        CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2),
        std::move(CmpLo),
        CreateThreeAddrMI(MachineInstruction::Sub, Sub1Dst, Src1Hi, Src2Hi),
        CreateThreeAddrMI(MachineInstruction::Sub, SubLoDst, Sub1Dst, CmpLoDst),
        CreateThreeAddrMI(MachineInstruction::Sub, SubHiDst, Src1Hi, Src2Hi),
        CreateThreeAddrMI(MachineInstruction::Merge, Dest, SubLoDst, SubHiDst),
    };

    InsertAfterMI(PMBB, std::move(MIS), InsertTo);

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
    InitCurrProcessMFB(MI);

    auto ResVReg = *MI->GetOperand(0);
    auto NumVReg = *MI->GetOperand(1);
    auto ModVReg = *MI->GetOperand(2);

    assert(ResVReg.IsVirtualReg() && "Result must be a virtual register");
    assert(NumVReg.IsVirtualReg() && "Operand #1 must be a virtual register");
    assert((ModVReg.IsVirtualReg() || ModVReg.IsImmediate()) &&
           "Operand #2 must be a virtual register or an immediate");

    auto DIVResult = PMF->GetNextAvailableVirtualRegister();
    auto Opcode    = IsUnsigned ? MachineInstruction::DivU : MachineInstruction::Div;
    auto DIV       = MachineInstruction(Opcode, PMBB);

    DIV.AddOperand(MachineOperand::CreateVirtualRegister(DIVResult));
    DIV.AddOperand(NumVReg);
    DIV.AddOperand(ModVReg);
    auto DIVIter = PMBB->ReplaceInstr(std::move(DIV), MI);

    auto MULResult = PMF->GetNextAvailableVirtualRegister();
    MachineInstruction MUL(MachineInstruction::Mul, PMBB);

    MUL.AddOperand(MachineOperand::CreateVirtualRegister(MULResult));
    MUL.AddOperand(MachineOperand::CreateVirtualRegister(DIVResult));
    MUL.AddOperand(ModVReg);
    auto MULIter = PMBB->InsertAfter(std::move(MUL), &*DIVIter);

    auto SUB = MachineInstruction(MachineInstruction::Sub, PMBB);

    SUB.AddOperand(ResVReg);
    SUB.AddOperand(NumVReg);
    SUB.AddOperand(MachineOperand::CreateVirtualRegister(MULResult));
    PMBB->InsertAfter(std::move(SUB), &*MULIter);

    return true;
}

bool TargetInstructionLegalizer::ExpandCmp(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "CMP must have exactly 3 operands");
    InitCurrProcessMFB(MI);

    const bool IsEQ                  = MI->GetRelation() == MachineInstruction::EQ;
    const bool IsNE                  = MI->GetRelation() == MachineInstruction::NE;
    [[maybe_unused]] const bool IsLT = MI->GetRelation() == MachineInstruction::LT;
    const bool IsGT                  = MI->GetRelation() == MachineInstruction::GT;
    const bool IsLE                  = MI->GetRelation() == MachineInstruction::LE;
    const bool IsGE                  = MI->GetRelation() == MachineInstruction::GE;

    const bool HasEQ = IsRelSupported(MachineInstruction::EQ);
    const bool HasNE = IsRelSupported(MachineInstruction::NE);
    const bool HasLT = IsRelSupported(MachineInstruction::LT);
    const bool HasGT = IsRelSupported(MachineInstruction::GT);
    const bool HasLE = IsRelSupported(MachineInstruction::LE);
    const bool HasGE = IsRelSupported(MachineInstruction::GE);

    // case when the sources are s64 sized, but it is not supported
    if (MI->GetOperand(1)->GetSize() == 64 || MI->GetOperand(2)->GetSize() == 64)
    {
        auto PMBB  = MI->GetParent();
        auto PFunc = PMBB->GetParent();

        auto Dest = *MI->GetOperand(0);
        auto Src1 = *MI->GetOperand(1);
        auto Src2 = *MI->GetOperand(2);

        // Better to materialize the constant in this case
        if (Src2.IsImmediate())
            return MaterializeImmOperand(MI, 2);

        assert(Dest.IsVirtualReg() && "Result must be a virtual register");
        assert(Src1.IsVirtualReg() && "Operand #1 must be a virtual register");
        assert(Src2.IsVirtualReg() && "Operand #2 must be a virtual register");

        // EQ and NE case
        //    CMP.[EQ|NE] %res, %src1(s64), %src2(s64)
        //
        // is replaced with
        //    SPLIT       %src1_lo, %src1_hi, %src1
        //    SPLIT       %src2_lo, %src2_hi, %src2
        //    XOR         %xor_lo, %src1_lo, %src2_lo
        //    XOR         %xor_hi, %src1_hi, %src2_hi
        //    OR          %or, %xor_lo, %xor_hi
        //    CMP.[EQ|NE] %res, %or, 0
        if (IsEQ || IsNE)
        {
            auto Src1Lo   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto Src1Hi   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto Src2Lo   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto Src2Hi   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto XorLoDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto XorHiDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto OrDst    = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

            auto Spl = CreateThreeAddrMI(MachineInstruction::Split, Src1Lo, Src1Hi, Src1);
            auto InsertTo = PMBB->ReplaceInstr(std::move(Spl), MI);

            auto Cmp = MachineInstruction(MachineInstruction::Cmp, PMBB);
            Cmp.SetAttributes(IsEQ ? MachineInstruction::EQ : MachineInstruction::NE);
            Cmp.AddOperand(Dest);
            Cmp.AddOperand(OrDst);
            Cmp.AddImmediate(0);

            std::vector<MachineInstruction> MIS {
                CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2),
                CreateThreeAddrMI(MachineInstruction::XOr, XorLoDst, Src1Lo, Src2Lo),
                CreateThreeAddrMI(MachineInstruction::XOr, XorHiDst, Src1Hi, Src2Hi),
                CreateThreeAddrMI(MachineInstruction::Or, OrDst, XorLoDst, XorHiDst),
                std::move(Cmp),
            };

            InsertAfterMI(PMBB, std::move(MIS), InsertTo);

            return true;
        }

        // LT, LE, GT, GE
        else
        {
            // This could be done with 5 instructions, but have to introduce
            // new basic blocks, so instead branchless programming principles used to
            // solve it without branches, but this results in a sequence of 7
            // instructions. And also with branches some unnecessary calculation can
            // be skipped.
            // TODO: Chose this branchless way, because not sure if currently
            // inserting a BB would not screw up something (reallocation of the vector
            // of basic block may cause problems or simply the way the basic blocks
            // iterated in the passes etc). Investigate this and improve the
            // implementation

            // EQ and NE case
            //    CMP.<REL> %res, %src1(s64), %src2(s64)
            //
            // is replaced with
            //    SPLIT       %src1_lo, %src1_hi, %src1
            //    SPLIT       %src2_lo, %src2_hi, %src2
            //    CMP.EQ      %hi_eq, %src1_hi, %src2_hi
            //    CMP.NE      %hi_ne, %src1_hi, %src2_hi
            //    CMP.<REL>   %cmp_lo, %src1_lo, %src2_lo
            //    CMP.<REL>   %cmp_hi, %src1_hi, %src2_hi
            //    MUL         %mul_eq, %hi_eq, %cmp_lo
            //    MUL         %mul_ne, %hi_ne, %cmp_hi
            //    ADD         %add, %mul_eq, %mul_ne
            auto Src1Lo     = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto Src1Hi     = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto Src2Lo     = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto Src2Hi     = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto CmpHiEqDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto CmpHiNeDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto CmpLoDst   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto CmpHiDst   = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto MulHiEqDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
            auto MulHiNeDst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

            const auto Relation = MI->GetRelation();

            auto Spl = CreateThreeAddrMI(MachineInstruction::Split, Src1Lo, Src1Hi, Src1);
            auto InsertTo = PMBB->ReplaceInstr(std::move(Spl), MI);

            auto CmpHiEq =
                CreateThreeAddrMI(MachineInstruction::Cmp, CmpHiEqDst, Src1Hi, Src2Hi);
            CmpHiEq.SetAttributes(MachineInstruction::EQ);

            auto CmpHiNe =
                CreateThreeAddrMI(MachineInstruction::Cmp, CmpHiNeDst, Src1Hi, Src2Hi);
            CmpHiNe.SetAttributes(MachineInstruction::NE);

            auto CmpLo =
                CreateThreeAddrMI(MachineInstruction::Cmp, CmpLoDst, Src1Lo, Src2Lo);
            CmpLo.SetAttributes(Relation);

            auto CmpHi =
                CreateThreeAddrMI(MachineInstruction::Cmp, CmpHiDst, Src1Hi, Src2Hi);
            CmpHi.SetAttributes(Relation);

            auto MulHiEq = MachineInstruction(MachineInstruction::Mul, PMBB);
            MulHiEq.AddOperand(MulHiEqDst);
            MulHiEq.AddOperand(CmpHiEqDst);
            MulHiEq.AddOperand(CmpLoDst);


            auto MulHiNe = MachineInstruction(MachineInstruction::Mul, PMBB);
            MulHiNe.AddOperand(MulHiNeDst);
            MulHiNe.AddOperand(CmpHiNeDst);
            MulHiNe.AddOperand(CmpHiDst);

            auto Add = MachineInstruction(MachineInstruction::Add, PMBB);
            Add.AddOperand(Dest);
            Add.AddOperand(MulHiEqDst);
            Add.AddOperand(MulHiNeDst);

            std::vector<MachineInstruction> MIS {
                CreateThreeAddrMI(MachineInstruction::Split, Src2Lo, Src2Hi, Src2),
                std::move(CmpHiEq),
                std::move(CmpHiNe),
                std::move(CmpLo),
                std::move(CmpHi),
                std::move(MulHiEq),
                std::move(MulHiNe),
                std::move(Add),
            };

            InsertAfterMI(PMBB, std::move(MIS), InsertTo);

            return true;
        }
    }

    // EQ and NE case
    //    CMP.EQ %res, %src1, %src2
    //
    // is replaced with
    //    XOR     %dst, $scr1, $src2
    //    CMP.LT  %res, %dst, 1
    //
    // and
    //    CMP.NE %res, %src1, %src2
    //
    // is replaced with
    //    XOR     %dst, $scr1, $src2
    //    CMP.LT  %res, 0, %dst
    //
    // given that LT is supported
    if (((IsNE && !HasNE) || (IsEQ && !HasEQ)) && HasLT)
    {
        // change the relation of the compare to less than
        MI->SetAttributes(MachineInstruction::LT);

        auto XorDst = GetNextAvailVReg();
        auto XorDstMO =
            MachineOperand::CreateVirtualRegister(XorDst, MI->GetOperand(1)->GetSize());

        MachineInstruction Xor;
        Xor.SetOpcode(MachineInstruction::XOr);
        Xor.AddOperand(XorDstMO);
        Xor.AddOperand(*MI->GetOperand(1));
        Xor.AddOperand(*MI->GetOperand(2));

        MI = &*PMBB->InsertBefore(std::move(Xor), MI);
        MI++;

        // Replace the immediate operand with the result register
        MI->RemoveOperand(2);
        MI->RemoveOperand(1);

        if (IsEQ)
        {
            MI->AddOperand(XorDstMO);
            MI->AddImmediate(1);
        }
        else
        {
            // If the target has zero register, then use that
            if (auto ZeroReg = TM->GetRegInfo()->GetZeroRegister(XorDstMO.GetSize());
                ZeroReg != ~0u)
                MI->AddRegister(ZeroReg);
            else
                // TODO: load 0 into a register first
                MI->AddImmediate(0);

            MI->AddOperand(XorDstMO);
        }
        return true;
    }
    // change CMP.GT %dst, %src1, %src2 to CMP.LT %dst, %src2, %src1
    else if (IsGT && !HasGT && HasLT)
    {
        MI->SetAttributes(MachineInstruction::LT);

        auto src2 = *MI->GetOperand(2);

        // If the 2nd source operand is an immediate then load it
        // in first to a register since it will become the first
        // source operand which cannot be an immediate
        if (src2.IsImmediate())
        {
            // If the immediate is zero and the target has zero register,
            // then use that instead
            if (src2.GetImmediate() == 0 &&
                TM->GetRegInfo()->GetZeroRegister(src2.GetSize()) != ~0u)
            {
                auto ZeroReg = TM->GetRegInfo()->GetZeroRegister(src2.GetSize());
                src2         = MachineOperand::CreateRegister(ZeroReg, src2.GetSize());
            }
            // Else the immediate is non-zero or the target does not have
            // zero register, so issue a LOAD_IMM first for src2
            else
            {
                auto dst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

                MachineInstruction LI;
                LI.SetOpcode(MachineInstruction::LoadImm);
                LI.AddOperand(dst);
                LI.AddOperand(src2);

                MI   = &*PMBB->InsertBefore(std::move(LI), MI);
                src2 = dst;
                MI++;
            }
        }

        MI->RemoveOperand(2);
        MI->InsertOperand(1, src2);
        return true;
    }
    // GE
    //    CMP.GE %res, %src1, %src2
    //
    // is replaced with
    //    CMP.LT  %dst, %src1, %src2
    //    XOR     %res, $dst, 1
    //
    // The XOR is doing negation, so turning x >= y to !(x < y)
    else if (IsGE && !HasGE && HasLT)
    {
        MI->SetAttributes(MachineInstruction::LT);

        auto dst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg(),
                                                         MI->GetOperand(0)->GetSize());

        auto res = *MI->GetOperand(0);

        // replace CMP def operand to the new virtual register
        MI->RemoveOperand(0);
        MI->InsertOperand(0, dst);

        // building XOR %res, $dst, 1, which is basically res = !dst
        MachineInstruction Xor;
        Xor.SetOpcode(MachineInstruction::XOr);
        Xor.AddOperand(res);
        Xor.AddOperand(dst);
        Xor.AddImmediate(1);

        PMBB->InsertAfter(std::move(Xor), MI);

        return true;
    }
    // LE case
    //    CMP.LE %res, %src1, %src2
    //
    // is replaced with
    //    CMP.GE  %res, %src2, %src1
    // Just turning it into GE and that could be expanded as it can be seen above
    else if (IsLE && !HasLE && HasLT)
    {
        MI->SetAttributes(MachineInstruction::GE);

        auto src2 = *MI->GetOperand(2);

        // If the 2nd source operand is an immediate then load it
        // in first to a register since it will become the first
        // source operand which cannot be an immediate
        if (src2.IsImmediate())
        {
            // If the immediate is zero and the target has zero register,
            // then use that instead
            if (src2.GetImmediate() == 0 &&
                TM->GetRegInfo()->GetZeroRegister(src2.GetSize()) != ~0u)
            {
                auto ZeroReg = TM->GetRegInfo()->GetZeroRegister(src2.GetSize());
                src2         = MachineOperand::CreateRegister(ZeroReg, src2.GetSize());
            }
            // Else the immediate is non-zero or the target does not have
            // zero register, so issue a LOAD_IMM first for src2
            else
            {
                auto dst = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

                MachineInstruction LI;
                LI.SetOpcode(MachineInstruction::LoadImm);
                LI.AddOperand(dst);
                LI.AddOperand(src2);

                MI   = &*PMBB->InsertBefore(std::move(LI), MI);
                src2 = dst;
                MI++;
            }
        }

        MI->RemoveOperand(2);
        MI->InsertOperand(1, src2);
        return true;
    }

    return false;
}

/// If the target does not support s64, then
/// the following
///     LOAD %dest(s64), [address]
///
/// is replaced with
///     LOAD %lo32(s32), [address]
///     LOAD %hi32(s32), [address+4]
///     MERGE %dest(s64), %lo32(s32), %hi32(s32)
///
/// where immediate is a constant number
bool TargetInstructionLegalizer::ExpandLoad(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "LOAD must have exactly 2 operands");
    InitCurrProcessMFB(MI);

    auto Dest    = *MI->GetOperand(0);
    auto Address = *MI->GetOperand(1);

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert((Address.IsMemory() || Address.IsStackAccess()) &&
           "Result must be a memory access");

    auto Lo32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Hi32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    MI->RemoveOperand(0);
    MI->InsertOperand(0, Lo32);

    auto LoadHI = MachineInstruction(MachineInstruction::Load, PMBB);
    LoadHI.AddOperand(Hi32);
    LoadHI.AddOperand(Address);
    LoadHI.GetOperand(1)->SetOffset(Address.GetOffset() + 4);
    auto InsertTo = PMBB->InsertAfter(std::move(LoadHI), MI);

    auto Merge = CreateThreeAddrMI(MachineInstruction::Merge, Dest, Lo32, Hi32);
    PMBB->InsertAfter(std::move(Merge), &*InsertTo);

    return true;
}

/// Expand 64 bit LOAD_IMM to equivalent calculation using 32 bit instructions
///     LOAD_IMM   %dst(s64), const
///
///  is replaced with
///     LOAD_IMM   %lo(s32), const & 0xffffffff
///     LOAD_IMM   %hi(s32), (const >> 32) & 0xffffffff
///     MERGE      %dst(s64), %lo(s32), %hi(s32)
bool TargetInstructionLegalizer::ExpandLoadImm(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "LOAD_IMM must have exactly 2 operands");
    auto PMBB = MI->GetParent();
    auto PMF  = PMBB->GetParent();

    auto Dest = *MI->GetOperand(0);
    auto Src  = *MI->GetOperand(1);

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Src.IsImmediate() && "Source must be an immediate");

    auto Lo32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Hi32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    MachineInstruction LI1;
    LI1.SetOpcode(MachineInstruction::LoadImm);
    LI1.AddOperand(Lo32);
    LI1.AddImmediate(Src.GetImmediate() & 0xffffffff);
    auto InsertTo = PMBB->ReplaceInstr(std::move(LI1), MI);

    MachineInstruction LI2;
    LI2.SetOpcode(MachineInstruction::LoadImm);
    LI2.AddOperand(Hi32);
    LI2.AddImmediate((Src.GetImmediate() >> 32u) & 0xffffffff);
    InsertTo = PMBB->InsertAfter(std::move(LI2), &*InsertTo);

    auto Merge = CreateThreeAddrMI(MachineInstruction::Merge, Dest, Lo32, Hi32);
    PMBB->InsertAfter(std::move(Merge), &*InsertTo);

    return true;
}

/// If the target does not support storing directly an immediate, then
/// the following
///     STORE [address], immediate
///
/// is replaced with
///     LOAD_IMM %reg, immediate
///     STORE [address], %reg
///
/// where immediate is a constans number
bool TargetInstructionLegalizer::ExpandStore(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "STORE must have exactly 2 operands");
    InitCurrProcessMFB(MI);

    auto Address = *MI->GetOperand(0);
    auto Src     = *MI->GetOperand(1);

    if (Src.IsImmediate())
    {
        // Create the result register where the immediate will be loaded
        auto LOAD_IMMResult = GetNextAvailVReg();
        auto LOAD_IMMResultVReg =
            MachineOperand::CreateVirtualRegister(LOAD_IMMResult, Src.GetSize());

        // Replace the immediate operand with the result register
        MI->RemoveOperand(1);
        MI->AddOperand(LOAD_IMMResultVReg);

        MachineInstruction LI;
        LI.SetOpcode(MachineInstruction::LoadImm);
        LI.AddOperand(LOAD_IMMResultVReg);
        LI.AddOperand(Src);
        PMBB->InsertBefore(std::move(LI), MI);

        return true;
    }

    /// If the target does not support s64, then
    /// the following
    ///     STORE [address], %src(s64)
    ///
    /// is replaced with
    ///     SPLIT %lo32(s32), %hi32(s32), %src(s64)
    ///     STORE [address], %lo32(s32)
    ///     STORE [address+4], %hi32(s32)
    ///     MERGE %dest(s64), %lo32(s32), %hi32(s32)
    auto Lo32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Hi32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto Split = CreateThreeAddrMI(MachineInstruction::Split, Lo32, Hi32, Src);

    auto StoreLo = MachineInstruction(MachineInstruction::Store, PMBB);
    StoreLo.AddOperand(Address);
    StoreLo.AddOperand(Lo32);
    StoreLo.GetOperand(0)->SetOffset(Address.GetOffset());

    auto StoreHi = MachineInstruction(MachineInstruction::Store, PMBB);
    StoreHi.AddOperand(Address);
    StoreHi.AddOperand(Hi32);
    StoreHi.GetOperand(0)->SetOffset(Address.GetOffset() + 4);

    auto InsertTo = PMBB->ReplaceInstr(std::move(Split), MI);
    InsertTo      = PMBB->InsertAfter(std::move(StoreLo), &*InsertTo);
    PMBB->InsertAfter(std::move(StoreHi), &*InsertTo);

    return true;
}

/// Expand 64 bit ZEXT to equivalent calculation using 32 bit instructions
///     ZEXT %dst(s64), %src(s32)
///  is replaced with
///     LOAD_IMM   %zero, 0
///     MERGE      %dst(s64), %src(s32), %zero(s32)
bool TargetInstructionLegalizer::ExpandZExt(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "ZEXT must have exactly 2 operands");
    InitCurrProcessMFB(MI);

    auto Dest = *MI->GetOperand(0);
    auto Src  = *MI->GetOperand(1);

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Src.IsVirtualReg() && "Source must be a virtual register");

    auto Zero = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    MachineInstruction LI;
    LI.SetOpcode(MachineInstruction::LoadImm);
    LI.AddOperand(Zero);
    LI.AddImmediate(0);

    auto Merge = CreateThreeAddrMI(MachineInstruction::Merge, Dest, Src, Zero);

    auto InsertTo = PMBB->ReplaceInstr(std::move(LI), MI);
    PMBB->InsertAfter(std::move(Merge), &*InsertTo);

    return true;
}

/// Expand 64 bit TRUNC to equivalent calculation using 32 bit instructions
///     TRUNC %dst(s32), %src(s64)
///
///  is replaced with
///     SPLIT      %src(s64), %lo(s32), %hi(s32)
///     TRUNC      %dst, %lo
bool TargetInstructionLegalizer::ExpandTrunc(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "ZEXT must have exactly 2 operands");
    InitCurrProcessMFB(MI);

    auto Dest = *MI->GetOperand(0);
    auto Src  = *MI->GetOperand(1);

    assert(Dest.IsVirtualReg() && "Result must be a virtual register");
    assert(Src.IsVirtualReg() && "Source must be a virtual register");

    auto Lo32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());
    auto Hi32 = MachineOperand::CreateVirtualRegister(GetNextAvailVReg());

    auto Split = CreateThreeAddrMI(MachineInstruction::Split, Lo32, Hi32, Src);
    auto Trunc = MachineInstruction(MachineInstruction::Trunc, PMBB);
    Trunc.AddOperand(Dest);
    Trunc.AddOperand(Lo32);

    auto InsertTo = PMBB->ReplaceInstr(std::move(Split), MI);
    PMBB->InsertAfter(std::move(Trunc), &*InsertTo);

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
