#include <cstdint>
#include <iostream>
#include "BackEnd/LowLevelType.hpp"
#include "BackEnd/MachineOperand.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "fmt/core.h"
#include "fmt/format.h"

MachineOperand MachineOperand::CreateRegister(uint64_t Reg, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToRegister();
    MO.SetReg(Reg);
    MO.SetType(LowLevelType::CreateScalar(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateImmediate(uint64_t Val, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToIntImm();
    MO.SetValue(Val);
    MO.SetType(LowLevelType::CreateScalar(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateFPImmediate(double Val, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToFPImm();
    MO.SetFPValue(Val);
    MO.SetType(LowLevelType::CreateScalar(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateVirtualRegister(uint64_t Reg, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToVirtualRegister();
    MO.SetReg(Reg);
    MO.SetType(LowLevelType::CreateScalar(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateMemory(uint64_t Id, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToMemAddr();
    MO.SetType(LowLevelType::CreatePtr(BitWidth));
    MO.SetValue(Id);

    return MO;
}

MachineOperand MachineOperand::CreateMemory(uint64_t Id, int Offset, unsigned BitWidth)
{
    MachineOperand MO = CreateMemory(Id, BitWidth);
    MO.SetOffset(Offset);

    return MO;
}

MachineOperand MachineOperand::CreateStackAccess(uint64_t Slot, int Offset)
{
    MachineOperand MO;
    MO.SetTypeToStackAccess();
    MO.SetValue(Slot);
    MO.SetOffset(Offset);

    return MO;
}

MachineOperand MachineOperand::CreateParameter(uint64_t Val)
{
    MachineOperand MO;
    MO.SetTypeToParameter();
    MO.SetReg(Val);

    return MO;
}

MachineOperand MachineOperand::CreateLabel(const char *Label)
{
    MachineOperand MO;
    MO.SetTypeToLabel();
    MO.SetLabel(Label);

    return MO;
}

MachineOperand MachineOperand::CreateFunctionName(const char *Label)
{
    MachineOperand MO;
    MO.SetTypeToFunctionName();
    MO.SetLabel(Label);

    return MO;
}

MachineOperand MachineOperand::CreateGlobalSymbol(std::string &Symbol)
{
    MachineOperand MO;
    MO.SetTypeToGlobalSymbol();
    MO.SetGlobalSymbol(Symbol);

    return MO;
}

void MachineOperand::Print(TargetMachine *TM) const
{
    std::string OperandStr;
    switch (this->Type)
    {
        case MachineOperand::Register:
            if (Virtual)
                OperandStr = fmt::format("%vr-{}", IntVal);
            else if (TM && !Virtual)
            {
                TargetRegister *Reg = TM->GetRegInfo()->GetRegisterByID(GetReg());
                std::string RegStr =
                    Reg->GetAlias() != "" ? Reg->GetAlias() : Reg->GetName();

                OperandStr = RegStr;
            }
            else
                OperandStr = fmt::format("%{}", IntVal);
            break;
        case MachineOperand::MemoryAddress:
            OperandStr = fmt::format("%ptr-vr-{}", IntVal);
            if (Offset)
                OperandStr = fmt::format("{}+{}", OperandStr, Offset);
            break;
        case MachineOperand::IntImmediate:
            OperandStr = fmt::format("#{}", static_cast<int64_t>(IntVal));
            break;
        case MachineOperand::FPImmediate:
            OperandStr = fmt::format("#{}", FloatVal);
            break;
        case MachineOperand::StackAccess:
            OperandStr = fmt::format("stack{}+{}", IntVal, Offset);
            break;
        case MachineOperand::Paramter: OperandStr = fmt::format("@{}", IntVal); break;
        case MachineOperand::Label:
            OperandStr = fmt::format("<{}>", BelongToLabel);
            break;
        case MachineOperand::FunctionName:
            OperandStr = fmt::format("@{}", BelongToLabel);
            break;
        case MachineOperand::GlobalSymbol:
            OperandStr = fmt::format("@{}", GlobalSym);
            break;

        default: break;
    }

    fmt::print("{}", OperandStr);

    if (LLT.IsValid())
    {
        std::string output;

        if ((this->Type == Register || this->Type == MemoryAddress) && IsVirtual())
        {
            output = fmt::format(":{}",
                                 (RegisterClass == ~0u) ?
                                     "_" :
                                     TM->GetRegInfo()->GetRegClassString(RegisterClass));
        }
        fmt::print("({})", LLT.ToString() + output);
    }
}

bool MachineOperand::operator==(const MachineOperand &RHS)
{
    if (Type != RHS.Type)
        return false;

    switch (Type)
    {
        case Register:
            return (IntVal == RHS.IntVal && Virtual == RHS.Virtual &&
                    RegisterClass == RHS.RegisterClass);

        case IntImmediate: return IntVal == RHS.IntVal;
        case FPImmediate: return FloatVal == RHS.FloatVal;
        case MemoryAddress:
        case StackAccess:
            return (IntVal == RHS.IntVal && Virtual == RHS.Virtual &&
                    RegisterClass == RHS.RegisterClass && Offset);

        // TODO:
        case Paramter: return false;

        case Label:
        case FunctionName:
            return std::string(BelongToLabel) == std::string(RHS.BelongToLabel);

        case GlobalSymbol: return GlobalSym == RHS.GlobalSym;

        default: assert(!"Unreachable");
    }
}
