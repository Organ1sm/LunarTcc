#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/Support.hpp"
#include "BackEnd/TargetArchs/RISCV/RISCVTargetMachine.hpp"
#include "BackEnd/TargetArchs/RISCV/RISCVRegisterInfo.hpp"
#include "BackEnd/TargetArchs/RISCV/RISCVTargetABI.hpp"
#include "BackEnd/TargetArchs/RISCV/RISCVInstructionDefinitions.hpp"

using namespace RISCV;

RISCVTargetMachine::RISCVTargetMachine()
{
    this->RegInfo   = std::make_unique<RISCVRegisterInfo>();
    this->ABI       = std::make_unique<RISCVTargetABI>(this->RegInfo.get());
    this->InstrDefs = std::make_unique<RISCVInstructionDefinitions>();
}

bool RISCVTargetMachine::SelectAdd(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "ADD must have 3 operands");

    // If last operand is an immediate then select "addi"
    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        assert(IsInt<12>((int64_t)ImmMO->GetImmediate())
               && "Immediate must be 12 bit wide");

        // TODO: check if the register operands are valid, like i32 and not f32
        // NOTE: maybe we should not really check here, although then how we know
        // that it is a floating point addition or not?
        MI->SetOpcode(ADDI);
        return true;
    }
    // Try to select "add"
    else
    {
        MI->SetOpcode(ADD);
        return true;
    }

    return false;
}

bool RISCVTargetMachine::SelectMod(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "MOD must have 3 operands");

    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        assert(!"Unimplemented");
    }
    else
    {
        MI->SetOpcode(REM);
        return true;
    }

    return false;
}

bool RISCVTargetMachine::SelectCmp(MachineInstruction *MI)
{
    assert(MI->GetOperandsNumber() == 3 && "CMP must have 3 operands");

    if (auto ImmMO = MI->GetOperand(2); ImmMO->IsImmediate())
    {
        switch (MI->GetRelation())
        {
            case MachineInstruction::LE:
                MI->SetOpcode(SLTI);
                return true;
            default:
                assert(!"Unimplemented");
        }
    }
    else
    {
        switch (MI->GetRelation())
        {
            case MachineInstruction::LE:
                MI->SetOpcode(SLT);
                return true;
            default:
                assert(!"Unimplemented");
        }
    }

    return false;
}

bool RISCVTargetMachine::SelectLoad(MachineInstruction *MI)
{
    assert((MI->GetOperandsNumber() == 2 || MI->GetOperandsNumber() == 3)
           && "LOAD must have 2 or 3 operands");

    MI->SetOpcode(LW);
    return true;
}

bool RISCVTargetMachine::SelectStore(MachineInstruction *MI)
{
    assert((MI->GetOperandsNumber() == 2 || MI->GetOperandsNumber() == 3)
           && "STORE must have 2 or 3 operands");

    MI->SetOpcode(SW);
    return true;
}

bool RISCVTargetMachine::SelectBranch(MachineInstruction *MI)
{
    if (MI->IsFallThroughBranch())
    {
        MI->SetOpcode(BNEZ);
        return true;
    }

    return false;
}

bool RISCVTargetMachine::SelectJump(MachineInstruction *MI)
{
    MI->SetOpcode(J);
    return true;
}

bool RISCVTargetMachine::SelectRet(MachineInstruction *MI)
{
    MI->SetOpcode(RET);
    return true;
}
