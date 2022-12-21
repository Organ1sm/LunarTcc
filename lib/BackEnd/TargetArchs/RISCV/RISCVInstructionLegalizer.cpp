#include "BackEnd/TargetArchs/RISCV/RISCVInstructionLegalizer.hpp"
#include "BackEnd/TargetArchs/RISCV/RISCVInstructionDefinitions.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineBasicBlock.hpp"

#include <cassert>

using namespace RISCV;

bool RISCVInstructionLegalizer::Check(MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Add:
            if (MI->GetDefine()->GetSize() > TM->GetPointerSize())
                return false;
            break;

        case MachineInstruction::AddS:
        case MachineInstruction::AddC: return false;

        case MachineInstruction::XOr:
            if (MI->GetDefine()->GetSize() > TM->GetPointerSize())
                return false;
            break;

        case MachineInstruction::Cmp: {
            if (MI->GetOperand(1)->GetSize() > TM->GetPointerSize() ||
                MI->GetOperand(2)->GetSize() > TM->GetPointerSize() ||
                MI->GetRelation() != MachineInstruction::LT)
                return false;
            break;
        }
        case MachineInstruction::Load:
            if (MI->GetDefine()->GetSize() > TM->GetPointerSize())
                return false;
            break;

        case MachineInstruction::LoadImm:
            if (MI->GetDefine()->GetSize() > TM->GetPointerSize())
                return false;
            break;

        case MachineInstruction::Store:
            if (MI->GetOperands().back().IsImmediate() ||
                MI->GetOperand(1)->GetSize() > TM->GetPointerSize())
                return false;
            break;

        case MachineInstruction::Mul:
            if (MI->GetDefine()->GetSize() > TM->GetPointerSize())
                return false;

        case MachineInstruction::Div:
        case MachineInstruction::DivU:
        case MachineInstruction::Mod:
        case MachineInstruction::ModU:
            if (MI->GetOperand(2)->IsImmediate())
                return false;

        case MachineInstruction::Sub: {
            if (MI->GetOperand(1)->GetSize() > TM->GetPointerSize() ||
                MI->GetOperand(2)->GetSize() > TM->GetPointerSize() ||
                MI->GetOperand(1)->IsImmediate())
                return false;
            break;
        }

        case MachineInstruction::ZExt:
            if (MI->GetDefine()->GetSize() > TM->GetPointerSize())
                return false;
            break;

        case MachineInstruction::Trunc:
            if (MI->GetOperand(1)->GetSize() > TM->GetPointerSize())
                return false;
            break;

        default: break;
    }

    return true;
}

bool RISCVInstructionLegalizer::IsExpandable(const MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Add:
        case MachineInstruction::AddS:
        case MachineInstruction::AddC:
        case MachineInstruction::XOr:
        case MachineInstruction::Cmp:
        case MachineInstruction::Load:
        case MachineInstruction::LoadImm:
        case MachineInstruction::Store:
        case MachineInstruction::Sub:
        case MachineInstruction::Mul:
        case MachineInstruction::Div:
        case MachineInstruction::DivU:
        case MachineInstruction::Mod:
        case MachineInstruction::ModU:
        case MachineInstruction::ZExt:
        case MachineInstruction::Trunc: return true;

        default: break;
    }

    return false;
}

static bool ExpandArithmeticInstWithImm(MachineInstruction *MI, size_t Index)
{
    assert(MI->GetOperandsNumber() == 3 && "MI must have exactly 3 operands");
    assert(Index < MI->GetOperandsNumber());
    auto ParentBB = MI->GetParent();

    auto DestReg = ParentBB->GetParent()->GetNextAvailableVirtualRegister();

    auto MOV = MachineInstruction(MachineInstruction::LoadImm, nullptr);
    MOV.AddVirtualRegister(DestReg, MI->GetOperand(Index)->GetSize());
    MOV.AddOperand(*MI->GetOperand(Index));

    // replace the immediate operand with the destination of the immediate load
    MI->RemoveOperand(Index);
    MI->InsertOperand(
        Index,
        MachineOperand::CreateVirtualRegister(DestReg, MI->GetOperand(0)->GetSize()));

    // insert after modifying MI, otherwise MI would became invalid
    ParentBB->InsertBefore(std::move(MOV), MI);

    return true;
}

bool RISCVInstructionLegalizer::ExpandDiv(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "DIV must have exactly 3 operands");
    return ExpandArithmeticInstWithImm(MI, 2);
}

bool RISCVInstructionLegalizer::ExpandDivU(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "DIVU must have exactly 3 operands");
    return ExpandArithmeticInstWithImm(MI, 2);
}

bool RISCVInstructionLegalizer::ExpandMod(MachineInstruction *MI, bool IsUnsigned)
{
    assert(MI->GetOperandsNumber() == 3 && "MOD must have exactly 3 operands");
    return ExpandArithmeticInstWithImm(MI, 2);
}
