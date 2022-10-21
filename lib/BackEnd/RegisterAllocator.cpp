#include "BackEnd/RegisterAllocator.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/Support.hpp"
#include "BackEnd/TargetInstruction.hpp"
#include "BackEnd/TargetMachine.hpp"
#include <cassert>
#include <map>
#include <vector>

using VirtualReg  = unsigned;
using PhysicalReg = unsigned;

void PreAllocateParameters(MachineFunction &Func,
                           TargetMachine *TM,
                           std::map<VirtualReg, PhysicalReg> &AllocatedRegisters)
{
    auto ArgRegs             = TM->GetABI()->GetArgumentRegisters();
    unsigned CurrentParamReg = 0;

    for (auto Param : Func.GetParameters())
    {
        // FIXME: excess parameters should be on the stack
        assert(CurrentParamReg < ArgRegs.size() && "Run out of param regs");

        if (Param.second.GetBitWidth() <= 32)
            AllocatedRegisters[Param.first] = ArgRegs[CurrentParamReg++]->GetSubRegs()[0];
        else
            // allocate the parameter to the CurrentParamReg -th param register
            AllocatedRegisters[Param.first] = ArgRegs[CurrentParamReg++]->GetID();
    }
}

void PreAllocateReturnRegister(MachineFunction &Func,
                               TargetMachine *TM,
                               std::map<VirtualReg, PhysicalReg> &AllocatedRegisters)
{
    auto RetRegs      = TM->GetABI()->GetReturnRegisters();
    auto LastBBInstrs = Func.GetBasicBlocks().back().GetInstructions();

    for (auto It = LastBBInstrs.rbegin(); It != LastBBInstrs.rend(); It++)
    {
        // If return instruction
        auto Opcode = It->GetOpcode();
        if (auto TargetInstr = TM->GetInstrDefs()->GetTargetInstr(Opcode);
            TargetInstr->IsReturn())
        {
            auto RetValSize = It->GetOperands()[0].GetSize();

            if (RetValSize == RetRegs[0]->GetBitWidth())
                AllocatedRegisters[It->GetOperand(0)->GetReg()] = RetRegs[0]->GetID();
            else
                AllocatedRegisters[It->GetOperand(0)->GetReg()] =
                    RetRegs[0]->GetSubRegs()[0];
        }
    }
}

PhysicalReg
    GetNextAvaiableReg(uint8_t BitSize, std::vector<PhysicalReg> &Pool, TargetMachine *TM)
{
    unsigned loopCounter = 0;
    for (auto UnAllocatedReg : Pool)
    {
        auto UnAllocatedRegInfo = TM->GetRegInfo()->GetRegisterByID(UnAllocatedReg);

        // if the register bit width matches the requested size
        // then return this register and delete it from the pool

        if (UnAllocatedRegInfo->GetBitWidth() == BitSize)
        {
            Pool.erase(Pool.begin() + loopCounter);
            return UnAllocatedReg;
        }

        // Otherwise check the subregisters of the register if it has, and try to
        // find a right candiate
        for (auto SubReg : UnAllocatedRegInfo->GetSubRegs())
        {
            auto SubRegInfo = TM->GetRegInfo()->GetRegisterByID(SubReg);
            if (SubRegInfo->GetBitWidth() == BitSize)
            {
                Pool.erase(Pool.begin() + loopCounter);
                return SubReg;
            }
        }
        loopCounter++;
    }

    assert(!"Have not found the right register");
    return 0;
}

void RegisterAllocator::RunRA()
{
    // mapping from virtual reg to physical reg
    std::map<VirtualReg, PhysicalReg> AllocatedRegisters;
    std::vector<PhysicalReg> RegisterPool;    // available registers
    std::set<VirtualReg> RegistersToSpill;    // register require spilling


    for (auto &Func : MIRM->GetFunctions())
    {
        AllocatedRegisters.clear();
        RegisterPool.clear();
        RegistersToSpill.clear();

        // Initialize the usable register's pool
        for (auto TargetReg : TM->GetABI()->GetCallerSavedRegisters())
            RegisterPool.push_back(TargetReg->GetID());

        PreAllocateParameters(Func, TM, AllocatedRegisters);
        PreAllocateReturnRegister(Func, TM, AllocatedRegisters);

        // Remove the pre allocated registers from the register pool
        for (const auto [VirtReg, PhysReg] : AllocatedRegisters)
        {
            auto RegsToCheck = TM->GetRegInfo()->GetRegisterByID(PhysReg)->GetSubRegs();
            RegsToCheck.push_back(PhysReg);

            for (auto Reg : RegsToCheck)
            {
                auto position = std::find(RegisterPool.begin(), RegisterPool.end(), Reg);

                if (position != RegisterPool.end())
                    RegisterPool.erase(position);
            }
        }

        // we want to keep track of how many consecutive renames happened since only
        // two can be afforded for now, doing more then that is an error
        unsigned ConsecutiveLoadRenames  = 0;
        unsigned ConsecutiveStoreRenames = 0;

        for (auto &BB : Func.GetBasicBlocks())
            for (auto &Instr : BB.GetInstructions())
                for (auto &Operand : Instr.GetOperands())
                {
                    if (Operand.IsVirtualReg() || Operand.IsParameter())
                    {
                        const auto UsedReg    = Operand.GetReg();
                        bool AlreadyAllocated = 0 != AllocatedRegisters.count(UsedReg);

                        // when short on allocatable registers, then add the register
                        // to the spillable set if its not a load or a store
                        if (!AlreadyAllocated && RegisterPool.size() <= 3)
                        {
                            // if the instruction is NOT a load, then spill it
                            if (!Instr.IsLoadOrStore())
                            {
                                ConsecutiveLoadRenames = ConsecutiveStoreRenames = 0;
                                RegistersToSpill.insert(UsedReg);
                                continue;
                            }

                            // Depending on whether its a load or a store, rename the
                            // register (allocating essentially)
                            if (Instr.IsLoad())
                            {
                                // Note that the last 2 register is used for this purpose
                                AllocatedRegisters[UsedReg] =
                                    RegisterPool.rbegin()[1 - ConsecutiveLoadRenames];
                                ConsecutiveLoadRenames++;
                            }
                            else
                            {
                                AllocatedRegisters[UsedReg] = RegisterPool.rbegin()[2];
                                ConsecutiveStoreRenames++;
                            }

                            // if its a load then rename it, which means to allocate it
                            // one of the registers used for loading spilled regs
                            assert(ConsecutiveLoadRenames <= 2
                                   && "To much consecutive loads to rename");
                            assert(ConsecutiveStoreRenames <= 1
                                   && "To much consecutive stores to rename");

                            AlreadyAllocated = true;
                        }
                        // if not allocated yet
                        if (!AlreadyAllocated)
                        {
                            // then allocate yet.
                            auto Reg =
                                GetNextAvaiableReg(Operand.GetSize(), RegisterPool, TM);
                            AllocatedRegisters[UsedReg] = Reg;

                            Operand.SetTypeToRegister();
                            Operand.SetReg(Reg);
                        }
                        // else its already alloacted so just look it up
                        else
                        {
                            Operand.SetTypeToRegister();
                            Operand.SetReg(AllocatedRegisters[UsedReg]);
                        }
                    }
                }
        // if there is nothing to spill then nothing todo
        if (RegistersToSpill.size() == 0)
        {
            for (auto Reg : RegistersToSpill)
                // TODO: Size should be the spilled register size, not hardcoded
                Func.InsertStackSlot(Reg, 4);

            for (auto &BB : Func.GetBasicBlocks())
            {
                auto &Instructions = BB.GetInstructions();

                for (size_t i = 0; i < Instructions.size(); i++)
                {
                    // Check operands which use the register (the first operand defining
                    // the reg the rest uses regs)
                    for (size_t OpIndex = 1;
                         OpIndex < Instructions[i].GetOperands().size(); OpIndex++)
                    {
                        auto &Operand = Instructions[i].GetOperands()[OpIndex];
                        auto UsedReg  = Operand.GetReg();

                        // if the used register is not spilled, then nothing to do
                        if (RegistersToSpill.count(UsedReg) == 0)
                            continue;

                        ////// Insert a LOAD

                        // First make the operand into a physical register which using
                        // the register used for the spilling
                        Operand.SetTypeToRegister();
                        Operand.SetReg(RegisterPool[OpIndex]);

                        auto Load = MachineInstruction(MachineInstruction::Load, &BB);
                        /// NOTE: 3 register left in pool using the 2nd and 3rd for uses
                        Load.AddRegister(RegisterPool[OpIndex]);
                        Load.AddStackAccess(UsedReg);
                        TM->SelectInstruction(&Load);

                        BB.InsertInstr(std::move(Load), i++);
                    }

                    /// Insert STORE if needed
                    auto &Operand = Instructions[i].GetOperands()[0];
                    auto DefReg   = Operand.GetReg();

                    if (RegistersToSpill.count(DefReg) == 0)
                        continue;

                    Operand.SetTypeToRegister();
                    Operand.SetReg(RegisterPool[0]);

                    auto Store = MachineInstruction(MachineInstruction::Store, &BB);

                    Store.AddStackAccess(DefReg);
                    /// NOTE: 3 register left in pool using the 1st for def
                    Store.AddRegister(RegisterPool[0]);
                    TM->SelectInstruction(&Store);

                    BB.InsertInstr(std::move(Store), ++i);
                }
            }
        }


        // FIXME: Move this out from here and make it a PostRA Pass
        // After RA lower the stack accessing operands to their final
        // form based on the final stack frame.

        for (auto &BB : Func.GetBasicBlocks())
            for (auto &Instr : BB.GetInstructions())
            {
                for (auto &Operand : Instr.GetOperands())
                {
                    // Only consider load and stores.
                    if (!Operand.IsStackAccess() && !Operand.IsMemory())
                        continue;

                    // Handle stack access
                    if (Operand.IsStackAccess())
                    {
                        auto FrameReg = TM->GetRegInfo()->GetStackRegister();
                        auto ObjPos   = static_cast<int>(
                            Func.GetStackObjectPosition(Operand.GetSlot()));
                        auto Offset = ObjPos + Operand.GetOffset();

                        // using SP as frame register fro simplicity
                        // TODO: Add FP register handing if target support it.

                        Instr.RemoveMemOperand();
                        Instr.AddRegister(FrameReg, TM->GetPointerSize());
                        Instr.AddImmediate(Offset);
                    }
                    // Handle memory access
                    else
                    {
                        auto BaseReg = Operand.GetReg();
                        auto Offset  = 0;
                        auto RegSize = TM->GetRegInfo()
                                           ->GetRegister(AllocatedRegisters[BaseReg])
                                           ->GetBitWidth();


                        Instr.RemoveMemOperand();
                        Instr.AddRegister(AllocatedRegisters[BaseReg], RegSize);
                        Instr.AddImmediate(Offset);
                    }

                    break;
                }
            }
    }
}
