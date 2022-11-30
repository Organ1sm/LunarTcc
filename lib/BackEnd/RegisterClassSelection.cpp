#include "BackEnd/RegisterClassSelection.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/TargetRegister.hpp"
#include <cassert>
#include <map>
#include <vector>

bool IsFPInstruction(MachineInstruction *MI, std::size_t Idx)
{
    switch (MI->GetOpcode())
    {
        case MachineInstruction::AddF:
        case MachineInstruction::SubF:
        case MachineInstruction::MulF:
        case MachineInstruction::DivF:
        case MachineInstruction::MovF: return true;

        case MachineInstruction::CmpF:          // the result is boolean and not fp.
        case MachineInstruction::FloatToInt:    // the result is integer, the operand is
            return Idx != 0;                    // fp.

        case MachineInstruction::IntToFloat:    // the result is fp, the operand is
            return Idx == 0;                    // integer

        default: return false;
    }
}

void RegisterClassSelection::Run()
{
    // To store the register class of the stored registers to the stack
    std::map<unsigned, unsigned> StackSlotToRegClass;

    // To store already processed virtual register's register class
    std::map<unsigned, unsigned> VRegToRegClass;

    unsigned ParameterCounter;
    for (auto &MFunc : MIRM->GetFunctions())
    {
        // reset
        StackSlotToRegClass.clear();
        VRegToRegClass.clear();
        ParameterCounter = 0;

        for (auto &MBB : MFunc.GetBasicBlocks())
        {
            for (std::size_t i = 0; i < MBB.GetInstructions().size(); i++)
            {
                for (std::size_t opIdx = 0;
                     opIdx < MBB.GetInstructions()[i].GetOperandsNumber();
                     opIdx++)
                {
                    MachineInstruction *MI = &MBB.GetInstructions()[i];
                    MachineOperand *OP     = MI->GetOperand(opIdx);

                    // if it is a store instruction accessing the stack, then map the
                    // stack slot to the appropriate register class
                    if (OP->IsStackAccess() && MI->IsStore() &&
                        StackSlotToRegClass.count(OP->GetSlot()) == 0 &&
                        MI->GetOperandsNumber() > opIdx + 1)
                    {
                        MachineOperand *NextOP = MI->GetOperand(opIdx + 1);

                        if (NextOP->IsVirtualReg() &&
                            VRegToRegClass.count(NextOP->GetReg()))
                        {
                            StackSlotToRegClass[OP->GetSlot()] =
                                VRegToRegClass[NextOP->GetReg()];
                            continue;
                        }
                        else if (NextOP->IsRegister())
                        {
                            StackSlotToRegClass[OP->GetSlot()] =
                                TM->GetRegInfo()->GetRegClassFromReg(NextOP->GetReg());
                            continue;
                        }
                        else if (NextOP->IsParameter())
                        {
                            auto [ParamNum, Type, IsStructPtr, IsFP] =
                                MFunc.GetParameters()[ParameterCounter++];
                            unsigned RC =
                                TM->GetRegInfo()->GetRegisterClass(Type.GetBitWidth(),
                                                                   IsFP);
                            StackSlotToRegClass[OP->GetSlot()] = RC;
                        }
                    }

                    // if it is a load instruction accessing the stack, then map the
                    // stack slot to the appropriate register class
                    if (OP->IsVirtualReg() && MI->IsLoad() &&
                        MI->GetOperandsNumber() > opIdx)
                    {
                        auto *NextOp = MI->GetOperand(opIdx + 1);

                        if (NextOp->IsStackAccess() &&
                            StackSlotToRegClass.count(NextOp->GetReg()))
                        {
                            VRegToRegClass[OP->GetReg()] =
                                StackSlotToRegClass[NextOp->GetSlot()];
                            OP->SetRegClass(VRegToRegClass[OP->GetReg()]);
                            continue;
                        }
                    }

                    // at this point only interested in virtual registers
                    if (!OP->IsVirtualReg())
                        continue;

                    auto Reg = OP->GetReg();

                    // check if this virtual register is already encountered
                    if (VRegToRegClass.count(Reg))
                    {
                        OP->SetRegClass(VRegToRegClass[Reg]);
                        continue;
                    }

                    const bool IsFP = IsFPInstruction(&MBB.GetInstructions()[i], opIdx);
                    unsigned RC = TM->GetRegInfo()->GetRegisterClass(OP->GetSize(), IsFP);
                    assert(TM->GetRegInfo()->GetRegClassRegsSize(RC) >= OP->GetSize());

                    OP->SetRegClass(RC);

                    VRegToRegClass[Reg] = RC;
                }
            }
        }
    }
}
