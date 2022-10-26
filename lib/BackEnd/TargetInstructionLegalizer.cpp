#include "BackEnd/TargetInstructionLegalizer.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/Support.hpp"
#include <cassert>

/// The following
///     MOD %res, %num, %mod
///
/// is replaced with
///     DIV %div_res, %num, %mod
///     MUL %mul_res, %div_res, %mod
///     SUB %res, %num, %mul_res
bool TargetInstructionLegalizer::ExpandMod(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "MOD must have exactly 3 operands");
    auto ParentBB   = MI->GetParent();
    auto ParentFunc = ParentBB->GetParent();

    auto ResVReg = *MI->GetOperand(0);
    auto NumVReg = *MI->GetOperand(1);
    auto ModVReg = *MI->GetOperand(2);

    assert(ResVReg.IsVirtualReg() && "Result must be a virtual register");
    assert(NumVReg.IsVirtualReg() && "Operand #1 must be a virtual register");
    assert((ModVReg.IsVirtualReg() || ModVReg.IsImmediate())
           && "Operand #2 must be a virtual register or an immediate");

    auto DIVResult = ParentFunc->GetNextAvailableVirtualRegister();
    MachineInstruction DIV;
    DIV.SetOpcode(MachineInstruction::Div);
    DIV.AddOperand(MachineOperand::CreateVirtualRegister(DIVResult));
    DIV.AddOperand(NumVReg);
    DIV.AddOperand(ModVReg);
    auto DIVIter = ParentBB->ReplaceInstr(std::move(DIV), MI);

    auto MULResult = ParentFunc->GetNextAvailableVirtualRegister();
    MachineInstruction MUL;
    MUL.SetOpcode(MachineInstruction::Mul);
    MUL.AddOperand(MachineOperand::CreateVirtualRegister(MULResult));
    MUL.AddOperand(MachineOperand::CreateVirtualRegister(DIVResult));
    MUL.AddOperand(ModVReg);
    auto MULIter = ParentBB->InsertAfter(std::move(MUL), &*DIVIter);

    MachineInstruction SUB;
    SUB.SetOpcode(MachineInstruction::Sub);
    SUB.AddOperand(ResVReg);
    SUB.AddOperand(NumVReg);
    SUB.AddOperand(MachineOperand::CreateVirtualRegister(MULResult));
    ParentBB->InsertAfter(std::move(SUB), &*MULIter);

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
    auto ParentBB   = MI->GetParent();
    auto ParentFunc = ParentBB->GetParent();

    auto Immediate = *MI->GetOperand(1);

    assert(Immediate.IsImmediate() && "Operand #2 must be an immediate");

    // Create the result register where the immediate will be loaded
    auto LOAD_IMMResult     = ParentFunc->GetNextAvailableVirtualRegister();
    auto LOAD_IMMResultVReg = MachineOperand::CreateVirtualRegister(LOAD_IMMResult);

    // Replace the immediate operand with the result register
    MI->RemoveOperand(1);
    MI->AddOperand(LOAD_IMMResultVReg);

    MachineInstruction LOAD_IMM;
    LOAD_IMM.SetOpcode(MachineInstruction::LoadImm);
    LOAD_IMM.AddOperand(LOAD_IMMResultVReg);
    LOAD_IMM.AddOperand(Immediate);
    ParentBB->InsertBefore(std::move(LOAD_IMM), MI);

    return true;
}

bool TargetInstructionLegalizer::Expand(MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod: return ExpandMod(MI);
        case MachineInstruction::Store: return ExpandStore(MI);
        case MachineInstruction::GlobalAddress: return ExpandGlobalAddress(MI);

        default: break;
    }

    return false;
}
