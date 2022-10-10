#include "BackEnd/RegisterAllocator.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/Support.hpp"
#include "BackEnd/TargetInstruction.hpp"
#include "BackEnd/TargetMachine.hpp"
#include <cassert>
#include <map>

void PreAllocateParameters(MachineFunction &Func,
                           TargetMachine *TM,
                           std::map<unsigned, unsigned> &AllocatedRegisters)
{
    auto ArgRegs             = TM->GetABI()->GetArgumentRegisters();
    unsigned CurrentParamReg = 0;

    for (auto Param : Func.GetParameters())
    {
        // FIXME: Handle others as well
        assert(Param.second.GetBitWidth() <= 32 && "Only handling <= 32 bits for now");

        // FIXME: excess parameters should be on the stack
        assert(CurrentParamReg < ArgRegs.size() && "Run out of param regs");

        // allocate the parameter to the CurrentParamReg -th param register
        AllocatedRegisters[Param.first] = ArgRegs[CurrentParamReg++]->GetID();
    }
}

void PreAllocateReturnRegister(MachineFunction &Func,
                               TargetMachine *TM,
                               std::map<unsigned, unsigned> &AllocatedRegisters)
{
    auto RetRegs      = TM->GetABI()->GetReturnRegisters();
    auto LastBBInstrs = Func.GetBasicBlocks().back().GetInstructions();

    for (auto It = LastBBInstrs.rbegin(); It != LastBBInstrs.rend(); It++)
    {
        // If return instruction
        //
        if (auto TargetInstr = TM->GetInstrDefs()->GetTargetInstr(It->GetOpcode());
            TargetInstr->IsReturn())
        {
            AllocatedRegisters[It->GetOperand(0)->GetReg()] = RetRegs[0]->GetID();
        }
    }
}

void RegisterAllocator::RunRA()
{
    // mapping from virtual reg to physical reg
    std::map<unsigned, unsigned> AllocatedRegisters;
    std::vector<unsigned> RegisterPool;

    // Initialize the usable register's pool
    for (auto TargetReg : TM->GetABI()->GetCallerSavedRegisters())
        RegisterPool.push_back(TargetReg->GetID());

    for (auto &Func : MIRM->GetFunctions())
    {
        PreAllocateParameters(Func, TM, AllocatedRegisters);
        PreAllocateReturnRegister(Func, TM, AllocatedRegisters);

        // Remove the pre allocated registers from the register pool
        for (const auto &AllocRegEntry : AllocatedRegisters)
        {
            auto position =
                std::find(RegisterPool.begin(), RegisterPool.end(), AllocRegEntry.second);

            if (position != RegisterPool.end())
                RegisterPool.erase(position);
        }

        for (auto &BB : Func.GetBasicBlocks())
            for (auto &Instr : BB.GetInstructions())
                for (auto &Operand : Instr.GetOperands())
                {
                    if (Operand.IsVirtualReg() || Operand.IsParameter())
                    {
                        // FIXME: should be runtime error in the future or better:
                        // implemetn spilling

                        assert(RegisterPool.size() > 0 && "Ran out of registers");

                        // if not allocated yet
                        if (AllocatedRegisters.count(Operand.GetReg()) == 0)
                        {
                            // then allocate yet.
                            auto Reg                             = RegisterPool[0];
                            AllocatedRegisters[Operand.GetReg()] = Reg;

                            // FIXME: vector is not a good container type if we commonly
                            // removing the first element HINT:
                            // list or dequeue
                            // Remove the register from the available register pool
                            RegisterPool.erase(RegisterPool.begin());

                            Operand.SetTypeToRegister();
                            Operand.SetReg(Reg);
                        }
                        else
                        {
                            Operand.SetTypeToRegister();
                            Operand.SetReg(AllocatedRegisters[Operand.GetReg()]);
                        }
                    }
                }
    }

    // FIXME: Move this out from here and make it a PostRA Pass
    // After RA lower the stack ascessing operands to their final
    // form based on the final stack frame.
    for (auto &Func : MIRM->GetFunctions())
    {
        unsigned StackFrameSize = Func.GetStackFrameSize();
        StackFrameSize =
            GetNextAlignedValue(StackFrameSize, TM->GetABI()->GetStackAlignment());

        for (auto &BB : Func.GetBasicBlocks())
            for (auto &Instr : BB.GetInstructions())
            {
                // Only consider load and stores.
                auto TargetInstr = TM->GetInstrDefs()->GetTargetInstr(Instr.GetOpcode());
                if (!TargetInstr->IsLoadOrStore())
                    continue;

                for (auto &Operand : Instr.GetOperands())
                {
                    // Only consider load and stores.
                    if (!Operand.IsStackAccess())
                        continue;

                    auto FrameReg = TM->GetRegInfo()->GetStackRegister();
                    auto ObjSize =
                        static_cast<int>(Func.GetStackObjectPosition(Operand.GetSlot()));
                    auto Offset = StackFrameSize - 4 - ObjSize;

                    // using SP as frame register fro simplicity
                    // TODO: Add FP register handing if target support it.

                    Instr.RemoveMemOperand();
                    Instr.AddRegister(FrameReg);
                    Instr.AddImmediate(Offset);

                    break;
                }
            }
    }
}
