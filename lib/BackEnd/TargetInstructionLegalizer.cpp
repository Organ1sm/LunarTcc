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
bool ExpandMOD(MachineInstruction *MI)
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

bool TargetInstructionLegalizer::Expand(MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod:
            return ExpandMOD(MI);

        default:
            break;
    }

    return false;
}