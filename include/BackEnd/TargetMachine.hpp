#pragma once

#include "BackEnd/TargetABI.hpp"
#include "BackEnd/RegisterInfo.hpp"
#include "BackEnd/MachineInstruction.hpp"
#include "BackEnd/InstructionDefinitions.hpp"
#include "BackEnd/TargetInstructionLegalizer.hpp"
#include <memory>

class TargetMachine
{
  public:
    TargetMachine() {}
    TargetMachine(InstructionDefinitions *ID) : InstrDefs(ID) {}
    virtual ~TargetMachine() {}

    TargetABI *GetABI() { return ABI.get(); }
    InstructionDefinitions *GetInstrDefs() { return InstrDefs.get(); }
    RegisterInfo *GetRegInfo() { return RegInfo.get(); }
    TargetInstructionLegalizer *GetLegalizer() { return Legalizer.get(); }

    virtual uint8_t GetPointerSize() { return ~0; }
    virtual uint8_t GetIntSize() { return ~0; }
    virtual uint8_t GetLongSize() { return ~0; }

    bool SelectInstruction(MachineInstruction *MI);

    virtual bool SelectAnd(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectOr(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectXOR(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectLSL(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectLSR(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectAdd(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectSub(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectMul(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectDiv(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectDivU(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectMod(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectModU(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectCmp(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectAddS(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectAddC(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectMulHU(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectCmpF(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectAddF(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectSubF(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectMulF(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectDivF(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectIntToFloat(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectFloatToInt(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectSExt(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectZExt(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectTrunc(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectSExtLoad(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectZExtLoad(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectMov(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectMovF(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectLoadImm(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectLoad(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectStore(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectStackAddress(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectGlobalAddress(MachineInstruction *MI) { assert(!"Unimplemented"); }

    virtual bool SelectCall(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectBranch(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectJump(MachineInstruction *MI) { assert(!"Unimplemented"); }
    virtual bool SelectRet(MachineInstruction *MI) { assert(!"Unimplemented"); }

  protected:
    std::unique_ptr<TargetABI> ABI                        = nullptr;
    std::unique_ptr<RegisterInfo> RegInfo                 = nullptr;
    std::unique_ptr<InstructionDefinitions> InstrDefs     = nullptr;
    std::unique_ptr<TargetInstructionLegalizer> Legalizer = nullptr;
};
