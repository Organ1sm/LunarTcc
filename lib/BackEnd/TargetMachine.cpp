#include <cassert>
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/TargetMachine.hpp"

bool TargetMachine::SelectInstruction(MachineInstruction *MI)
{
    auto Opcode = MI->GetOpcode();

    switch (Opcode)
    {
        case MachineInstruction::And: return SelectAnd(MI);
        case MachineInstruction::Or: return SelectOr(MI);
        case MachineInstruction::XOr: return SelectXOR(MI);
        case MachineInstruction::LSL: return SelectLSL(MI);
        case MachineInstruction::LSR: return SelectLSR(MI);
        case MachineInstruction::Add: return SelectAdd(MI);
        case MachineInstruction::Sub: return SelectSub(MI);
        case MachineInstruction::Mul: return SelectMul(MI);
        case MachineInstruction::Div: return SelectDiv(MI);
        case MachineInstruction::Cmp: return SelectCmp(MI);
        case MachineInstruction::Mod: return SelectMod(MI);
        case MachineInstruction::DivU: return SelectDivU(MI);
        case MachineInstruction::ModU: return SelectModU(MI);

        case MachineInstruction::AddS: return SelectAddS(MI);
        case MachineInstruction::AddC: return SelectAddC(MI);
        case MachineInstruction::MulHU: return SelectMulHU(MI);

        case MachineInstruction::CmpF: return SelectCmpF(MI);
        case MachineInstruction::AddF: return SelectAddF(MI);
        case MachineInstruction::SubF: return SelectSubF(MI);
        case MachineInstruction::MulF: return SelectMulF(MI);
        case MachineInstruction::DivF: return SelectDivF(MI);

        case MachineInstruction::IntToFloat: return SelectIntToFloat(MI);
        case MachineInstruction::FloatToInt: return SelectFloatToInt(MI);

        case MachineInstruction::SExt: return SelectSExt(MI);
        case MachineInstruction::ZExt: return SelectZExt(MI);
        case MachineInstruction::SExtLoad: return SelectSExtLoad(MI);
        case MachineInstruction::ZExtLoad: return SelectZExtLoad(MI);
        case MachineInstruction::Trunc: return SelectTrunc(MI);
        case MachineInstruction::Mov: return SelectMov(MI);
        case MachineInstruction::MovF: return SelectMovF(MI);
        case MachineInstruction::LoadImm: return SelectLoadImm(MI);
        case MachineInstruction::Load: return SelectLoad(MI);
        case MachineInstruction::Store: return SelectStore(MI);
        case MachineInstruction::StackAddress: return SelectStackAddress(MI);
        case MachineInstruction::GlobalAddress: return SelectGlobalAddress(MI);
        case MachineInstruction::Call: return SelectCall(MI);
        case MachineInstruction::Branch: return SelectBranch(MI);
        case MachineInstruction::Jump: return SelectJump(MI);
        case MachineInstruction::Ret: return SelectRet(MI);

        default: assert(!"Unimplemented."); break;
    }

    return false;
}
