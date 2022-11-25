#include "BackEnd/TargetArchs/AArch64/AArch64InstructionLegalizer.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64InstructionDefinitions.hpp"
#include "BackEnd/TargetMachine.hpp"
#include <cassert>

using namespace AArch64;

// Mod operation is not legal on ARM, has to be expanded
bool AArch64InstructionLegalizer::Check(MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::ModU:
        case MachineInstruction::Mod: return false;

        case MachineInstruction::Sub:
            if (MI->GetOperand(1)->IsImmediate())
                return false;
            break;
        case MachineInstruction::Mul:
        case MachineInstruction::Div:
        case MachineInstruction::DivU: {
            if (MI->GetOperand(2)->IsImmediate())
                return false;
            break;
        }
        case MachineInstruction::Store:
            if (MI->GetOperands().back().IsImmediate())
                return false;
            break;
        case MachineInstruction::GlobalAddress: return false;

        default: break;
    }

    return true;
}

bool AArch64InstructionLegalizer::IsExpandable(const MachineInstruction *MI)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::Mod:
        case MachineInstruction::ModU:
        case MachineInstruction::Mul:
        case MachineInstruction::Div:
        case MachineInstruction::DivU:
        case MachineInstruction::Sub:
        case MachineInstruction::Store:
        case MachineInstruction::GlobalAddress: return true;

        default: break;
    }

    return false;
}

/// Since AArch64 does not support for immediate operand as 1st source operand
/// for SUB (and for all arithmetic instruction as well), there for it has to
/// be materialized first into a register
bool ExpandArithmeticInstWithImm(MachineInstruction *MI, std::size_t Index)
{
    assert(MI->GetOperandsNumber() == 3 && "Inst must have exactly 3 operands");
    assert(Index < MI->GetOperandsNumber());

    auto ParentBB = MI->GetParent();

    auto Mov     = MachineInstruction(MachineInstruction::LoadImm, nullptr);
    auto DestReg = ParentBB->GetParent()->GetNextAvailableVirtualRegister();

    // replace the immediate operand with the destination of the immediate load
    Mov.AddVirtualRegister(DestReg, MI->GetOperand(Index)->GetSize());
    Mov.AddOperand(*MI->GetOperand(Index));

    MI->RemoveOperand(Index);
    MI->InsertOperand(
        Index,
        MachineOperand::CreateVirtualRegister(DestReg, MI->GetOperand(0)->GetSize()));

    ParentBB->InsertBefore(std::move(Mov), MI);

    return true;
}

bool AArch64InstructionLegalizer::ExpandSub(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "Sub must have exactly 3 operands");
    return ExpandArithmeticInstWithImm(MI, 1);
}

bool AArch64InstructionLegalizer::ExpandMul(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "MUL must have exactly 3 operands");
    return ExpandArithmeticInstWithImm(MI, 2);
}

bool AArch64InstructionLegalizer::ExpandDiv(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "DIV must have exactly 3 operands");
    return ExpandArithmeticInstWithImm(MI, 2);
}

bool AArch64InstructionLegalizer::ExpandDivU(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "DIVU must have exactly 3 operands");
    return ExpandArithmeticInstWithImm(MI, 2);
}

/// Use wzr register if the stored immediate is 0
bool AArch64InstructionLegalizer::ExpandStore(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "Store must have exactly 2 operands");

    auto ParentBB       = MI->GetParent();
    auto ParentFunction = ParentBB->GetParent();
    auto Immediate      = *MI->GetOperand(1);

    assert(Immediate.IsImmediate() && "Operand #2 must be an immediate");

    if (Immediate.GetImmediate() == 0)
    {
        auto Size = Immediate.GetSize();
        auto WZR  = TM->GetRegInfo()->GetZeroRegister();

        // TODO: it might be not a good idea to set different bitwidth to this
        // physical register from its actual bitwidth, for now ExtendRegSize
        // handle this issue, but need to think this through
        MI->RemoveOperand(1);
        MI->AddRegister(WZR, Size);

        return true;
    }

    auto LoadImmResult = ParentFunction->GetNextAvailableVirtualRegister();
    auto LoadImmResultVReg =
        MachineOperand::CreateVirtualRegister(LoadImmResult, Immediate.GetSize());

    // Replace the immediate operand with the result register
    MI->RemoveOperand(1);
    MI->AddOperand(LoadImmResultVReg);

    MachineInstruction LOAD_IMM;
    LOAD_IMM.SetOpcode(MachineInstruction::LoadImm);
    LOAD_IMM.AddOperand(LoadImmResultVReg);
    LOAD_IMM.AddOperand(Immediate);

    ParentBB->InsertBefore(std::move(LOAD_IMM), MI);

    return true;
}


/// The global address materialization happens in two steps on arm. Example:
///   adrp x0, global_var
///   add  x0, x0, :lo12:global_var
bool AArch64InstructionLegalizer::ExpandGlobalAddress(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 2 && "GlobalAddress must have exactly 2 operands.");

    auto ParentBB  = MI->GetParent();
    auto GlobalVar = *MI->GetOperand(1);
    assert(GlobalVar.IsGlobalSymbol() && "Operand #2 must be a symbol");

    auto GlobalVarName = ":lo12:" + GlobalVar.GetGlobalSymbol();

    MI->SetOpcode(ADRP);

    MachineInstruction ADD;
    ADD.SetOpcode(MachineInstruction::Add);

    auto DestReg = *MI->GetOperand(0);

    ADD.AddOperand(DestReg);
    ADD.AddOperand(DestReg);
    ADD.AddGlobalSymbol(GlobalVarName);

    ParentBB->InsertAfter(std::move(ADD), MI);

    return true;
}
