#include "BackEnd/RegisterAllocator.hpp"
#include "BackEnd/MachineBasicBlock.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/MachineFunction.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/Support.hpp"
#include "BackEnd/TargetInstruction.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "fmt/core.h"
#include <cassert>
#include <map>
#include <tuple>
#include <algorithm>

using VirtualReg     = unsigned;
using PhysicalReg    = unsigned;
using LiveRangeMap   = std::map<VirtualReg, std::pair<unsigned, unsigned>>;
using LiveRangeTuple = std::tuple<VirtualReg, unsigned, unsigned>;

void PreAllocateParameters(MachineFunction &Func,
                           TargetMachine *TM,
                           std::map<VirtualReg, PhysicalReg> &AllocatedRegisters,
                           LiveRangeMap &LiveRanges)
{
    auto ArgRegs             = TM->GetABI()->GetArgumentRegisters();
    unsigned CurrentParamReg = 0;

    for (auto [ParamID, ParamLowLevelType, IsImplicitStructPtr, IsFP] :
         Func.GetParameters())
    {
        // FIXME: excess parameters should be on the stack
        assert(CurrentParamReg < ArgRegs.size() && "Run out of param regs");

        LiveRanges[ParamID] = {0, ~0};

        TargetRegister *CurrentArgReg = nullptr;
        if (IsImplicitStructPtr)
            CurrentArgReg = TM->GetRegInfo()->GetRegisterByID(
                TM->GetRegInfo()->GetStructPtrRegister());
        else if (IsFP)
            CurrentArgReg =
                ArgRegs[TM->GetABI()->GetFirstFPArgRegIdx() + CurrentParamReg++];
        else
            CurrentArgReg = ArgRegs[CurrentParamReg++];

        assert(CurrentArgReg && "Cannot be null");

        // allocate the parameter to the CurrentParamReg -th param register
        if (ParamLowLevelType.GetBitWidth() <= 32)
        {
            if (CurrentArgReg->GetBitWidth() > 32 && !CurrentArgReg->GetSubRegs().empty())
                AllocatedRegisters[ParamID] = CurrentArgReg->GetSubRegs()[0];
            else
                AllocatedRegisters[ParamID] = CurrentArgReg->GetID();
        }
        else
        {
            AllocatedRegisters[ParamID] = CurrentArgReg->GetID();
        }
    }
}

void PreAllocateReturnRegister(MachineFunction &Func,
                               TargetMachine *TM,
                               std::map<VirtualReg, PhysicalReg> &AllocatedRegisters)
{
    auto RetRegs      = TM->GetABI()->GetReturnRegisters();
    auto LastBBInstrs = Func.GetBasicBlocks().back().GetInstructions();

    for (auto MBBIt = Func.GetBasicBlocks().rbegin();
         MBBIt != Func.GetBasicBlocks().rend();
         MBBIt++)
    {
        for (auto It = MBBIt->GetInstructions().rbegin();
             It != MBBIt->GetInstructions().rend();
             It++)
        {
            // If return instruction
            const auto Opcode = It->GetOpcode();
            if (auto TargetInstr = TM->GetInstrDefs()->GetTargetInstr(Opcode);
                TargetInstr->IsReturn())
            {
                // if the ret has no operands it means the function ret type is void and
                // therefore does not need allocation for return registers
                if (It->GetOperandsNumber() == 0)
                    continue;

                const auto RetValSize = It->GetOperands()[0].GetSize();

                // find the appropriate sized target register for the return value.
                if (RetValSize == RetRegs[0]->GetBitWidth())
                    AllocatedRegisters[It->GetOperand(0)->GetReg()] = RetRegs[0]->GetID();
                else
                    AllocatedRegisters[It->GetOperand(0)->GetReg()] =
                        RetRegs[0]->GetSubRegs()[0];
            }
        }
    }
}

PhysicalReg GetNextAvaiableReg(MachineOperand *MOperand,
                               std::set<PhysicalReg> &Pool,
                               std::set<PhysicalReg> &BackupPool,
                               TargetMachine *TM,
                               MachineFunction &MFunc)
{
    // TODO: implement spilling and remove this assertion then
    assert(!(Pool.empty() && BackupPool.empty()) && "Ran out of registers");

    if (Pool.empty())
    {
        const auto BackUpReg = *BackupPool.begin();
        Pool.insert(BackUpReg);
        MFunc.GetUsedCalleeSavedRegs().push_back(BackUpReg);
        BackupPool.erase(BackUpReg);
    }

    for (auto UnAllocatedReg : Pool)
    {
        // if the register bit width matches the requested size
        // then return this register and delete it from the pool

        if (TM->GetRegInfo()->GetRegClassFromReg(UnAllocatedReg) ==
            MOperand->GetRegClass())
        {
            Pool.erase(UnAllocatedReg);
            return UnAllocatedReg;
        }

        // Otherwise check the subregisters of the register if it has, and try to
        // find a right candiate
        auto UnAllocatedRegInfo = TM->GetRegInfo()->GetRegisterByID(UnAllocatedReg);
        for (auto SubReg : UnAllocatedRegInfo->GetSubRegs())
        {
            if (TM->GetRegInfo()->GetRegClassFromReg(SubReg) == MOperand->GetRegClass())
            {
                Pool.erase(UnAllocatedReg);
                return SubReg;
            }
        }
    }

    assert(!"Have not found the right register");
    return 0;
}

void RegisterAllocator::RunRA()
{
    for (auto &Func : MIRM->GetFunctions())
    {
        // mapping virtual registers to live ranges, where the live range represent
        // the pair of the first definition (def) of the virtual register and the
        // last use (kill) of it. Kill initialized to ~0 to signal errors
        // potentially dead regs in the future
        LiveRangeMap LiveRanges;
        std::map<VirtualReg, MachineOperand *> VRegToMOMap;
        std::map<VirtualReg, PhysicalReg> AllocatedRegisters;
        std::set<PhysicalReg> RegisterPool;    // available registers

        // Used if run out of caller saved registers
        std::set<PhysicalReg> BackupRegisterPool;

        // Initialize the usable register's pool
        for (auto TargetReg : TM->GetABI()->GetCallerSavedRegisters())
            RegisterPool.insert(TargetReg->GetID());

        // Initialize the backup register pool with the callee saved ones
        for (auto TargetReg : TM->GetABI()->GetCalleeSavedRegisters())
            BackupRegisterPool.insert(TargetReg->GetID());

        PreAllocateParameters(Func, TM, AllocatedRegisters, LiveRanges);
        PreAllocateReturnRegister(Func, TM, AllocatedRegisters);

        // Remove the pre allocated registers from the register pool
        std::set<PhysicalReg> RegsToBeRemoved;
        for (const auto [VirtReg, PhysReg] : AllocatedRegisters)
        {
            const auto ParentReg = TM->GetRegInfo()->GetParentReg(PhysReg);
            RegsToBeRemoved.insert(ParentReg ? ParentReg->GetID() : PhysReg);
        }

        for (auto &BB : Func.GetBasicBlocks())
        {
            for (auto &Instr : BB.GetInstructions())
            {
                for (std::size_t i = 0; i < Instr.GetOperandsNumber(); i++)
                {
                    auto &Operand = Instr.GetOperands()[i];

                    if (Operand.IsRegister())
                    {
                        const auto PhysReg   = Operand.GetReg();
                        const auto ParentReg = TM->GetRegInfo()->GetParentReg(PhysReg);

                        RegsToBeRemoved.insert(ParentReg ? ParentReg->GetID() : PhysReg);
                    }
                }
            }
        }

        // Actually removing the registers from the pool
        for (auto Reg : RegsToBeRemoved)
            RegisterPool.erase(Reg);

        // Caculating the live ranges for virtual registers.
        unsigned InstrCounter = 0;
        for (auto &BB : Func.GetBasicBlocks())
        {
            for (auto &Instr : BB.GetInstructions())
            {
                for (std::size_t i = 0; i < Instr.GetOperandsNumber(); i++)
                {
                    auto &Operand = Instr.GetOperands()[i];
                    if (Operand.IsVirtualReg() || Operand.IsParameter() ||
                        Operand.IsMemory())
                    {
                        auto UsedReg = Operand.GetReg();

                        // Save the VReg Operand into a map to be able to look it up later
                        // for size information like its bit size
                        if (VRegToMOMap.count(UsedReg) == 0)
                            VRegToMOMap[UsedReg] = &Instr.GetOperands()[i];

                        // if this VirtualReg first encountered
                        // for now assuming also its a definition if we encountered it
                        // first
                        if (LiveRanges.count(UsedReg) == 0)
                            LiveRanges[UsedReg] = {InstrCounter, ~0};
                        // otherwise it was already seen (therefore defined) so we only
                        // have to update the LiveRange entry last use part
                        else
                            LiveRanges[UsedReg].second = InstrCounter;
                    }
                }
                InstrCounter++;
            }
        }

#ifdef DEBUG
        fmt::print("{:*^60}\n\n", " In Function " + Func.GetName());
        for (const auto &[VReg, LiveRange] : LiveRanges)
        {
            auto [DefLine, KillLine] = LiveRange;
            fmt::print("VReg: {}, LiveRange({}, {})\n", VReg, DefLine, KillLine);
        }
        fmt::print("\n");
#endif

        // make a sorted vector from the map where the element ordered by the
        // LiveRange kill field, if both kill field is equal then the def field will
        // decide it
        std::vector<LiveRangeTuple> SortedLiveRanges;
        for (const auto &[VReg, LiveRange] : LiveRanges)
        {
            auto [DefLine, KillLine] = LiveRange;
            SortedLiveRanges.push_back({VReg, DefLine, KillLine});
        }

        auto Compare = [](LiveRangeTuple Left, LiveRangeTuple Right) {
            auto [LVReg, LDef, LKill] = Left;
            auto [RVReg, RDef, RKill] = Right;

            if (LDef < RDef)
                return true;
            else if (LDef < RDef)
                return LKill < RKill;
            else
                return false;
        };
        std::sort(SortedLiveRanges.begin(), SortedLiveRanges.end(), Compare);

#ifdef DEBUG
        fmt::print("SortedLiveRanges\n");
        for (const auto &[VReg, DefLine, KillLine] : SortedLiveRanges)
            fmt::print("VReg: {}, LiveRange({}, {})\n", VReg, DefLine, KillLine);
        fmt::print("\n");
#endif


        // To keep track the already allocated, but not yet freed live ranges
        std::vector<LiveRangeTuple> FreeAbleWorkList;
        for (const auto &[VReg, DefLine, KillLine] : SortedLiveRanges)
        {
            // First free registers which are already killed at this point
            for (int i = 0; i < (int)FreeAbleWorkList.size(); i++)
            {
                auto [CheckVReg, CheckDefLine, CheckKillLine] = FreeAbleWorkList[i];
                // the above checked entry definitions line
                // is greater then this entry kill line. Meaning the register assigned
                // to this entry can be freed, since we already passed the line where it
                // was last used (killed)
                if (CheckKillLine < DefLine)
                {
                    // Freeing the register allocated to this live range's register
                    // If its a subregister then we have to find its parent first and then
                    // put that back to the allocatable register's RegisterPool
                    assert(AllocatedRegisters.count(CheckVReg) > 0);
                    unsigned FreeAbleReg = AllocatedRegisters[CheckVReg];
                    auto ParentReg       = TM->GetRegInfo()->GetParentReg(FreeAbleReg);
                    if (ParentReg)
                        FreeAbleReg = ParentReg->GetID();

#ifdef DEBUG
                    fmt::print("Freed Register: {}\n",
                               TM->GetRegInfo()->GetRegisterByID(FreeAbleReg)->GetName());
#endif
                    RegisterPool.insert(RegisterPool.begin(), FreeAbleReg);
                    FreeAbleWorkList.erase(FreeAbleWorkList.begin() + i);
                    i--;    // to correct the index i, because of the erase
                }
            }

            // if not allocated yet, then allocate it
            if (AllocatedRegisters.count(VReg) == 0)
            {
                AllocatedRegisters[VReg] = GetNextAvaiableReg(VRegToMOMap[VReg],
                                                              RegisterPool,
                                                              BackupRegisterPool,
                                                              TM,
                                                              Func);

                FreeAbleWorkList.push_back({VReg, DefLine, KillLine});
            }
#ifdef DEBUG
            fmt::print(
                "VReg {} allocated to {}\n",
                VReg,
                TM->GetRegInfo()->GetRegisterByID(AllocatedRegisters[VReg])->GetName());
#endif
        }

#ifdef DEBUG
        fmt::print("\n\n AllocatedRegisters\n");
        for (auto [VReg, PhysReg] : AllocatedRegisters)
            fmt::print("VReg: {} to {}\n",
                       VReg,
                       TM->GetRegInfo()->GetRegisterByID(PhysReg)->GetName());

        fmt::print("\n\n");
#endif

        // Setting to operands from virtual register to register as a last part of
        // the allocation
        for (auto &BB : Func.GetBasicBlocks())
            for (auto &Instr : BB.GetInstructions())
                for (size_t i = 0; i < Instr.GetOperandsNumber(); i++)
                {
                    auto &Operand = Instr.GetOperands()[i];
                    auto PhysReg  = AllocatedRegisters[Operand.GetReg()];
                    if (Operand.IsVirtualReg() || Operand.IsParameter())
                    {
                        Operand.SetTypeToRegister();
                        Operand.SetReg(PhysReg);
                    }
                    else if (Operand.IsMemory())
                    {
                        Operand.SetVirtual(false);
                        Operand.SetValue(PhysReg);
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
                        auto Offset  = Operand.GetOffset();

                        unsigned Reg =
                            Operand.IsVirtual() ? AllocatedRegisters[BaseReg] : BaseReg;
                        auto RegSize = TM->GetRegInfo()->GetRegister(Reg)->GetBitWidth();

                        Instr.RemoveMemOperand();
                        Instr.AddRegister(Reg, RegSize);
                        Instr.AddImmediate(Offset);
                    }
                    // there should be only at most one stack access / instr
                    break;
                }
            }
    }
}
