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
    MO.SetType(LowLevelType::CreateInt(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateImmediate(uint64_t Val, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToIntImm();
    MO.SetValue(Val);
    MO.SetType(LowLevelType::CreateInt(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateVirtualRegister(uint64_t Reg, unsigned BitWidth)
{
    MachineOperand MO;
    MO.SetTypeToVirtualRegister();
    MO.SetReg(Reg);
    MO.SetType(LowLevelType::CreateInt(BitWidth));

    return MO;
}

MachineOperand MachineOperand::CreateMemory(uint64_t Id)
{
    MachineOperand MO;
    MO.SetTypeToMemAddr();
    MO.SetValue(Id);

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

void MachineOperand::Print(TargetMachine *TM) const
{
    std::string OperandStr;
    switch (this->Type)
    {
        case MachineOperand::Register:
            if (TM)
            {
                TargetRegister *Reg = TM->GetRegInfo()->GetRegisterByID(GetReg());
                std::string RegStr =
                    Reg->GetAlias() != "" ? Reg->GetAlias() : Reg->GetName();

                OperandStr = RegStr;
            }
            else
                OperandStr = fmt::format("%{}", Value);
            break;
        case MachineOperand::VirtualRegister:
            OperandStr = fmt::format("%vr-{}", Value);
            break;
        case MachineOperand::MemoryAddress:
            OperandStr = fmt::format("%vr-{}", Value);
            break;
        case MachineOperand::IntImmediate:
            OperandStr = fmt::format("#{}", static_cast<int64_t>(Value));
            break;
        case MachineOperand::StackAccess:
            OperandStr = fmt::format("stack{}+{}", Value, Offset);
            break;
        case MachineOperand::Paramter: OperandStr = fmt::format("@{}", Value); break;
        case MachineOperand::Label:
            OperandStr = fmt::format("<{}>", BelongToLabel);
            break;
        case MachineOperand::FunctionName:
            OperandStr = fmt::format("@{}", BelongToLabel);
            break;
        default: break;
    }

    fmt::print("{}", OperandStr);

    if (LLT.IsValid())
        fmt::print("({})", LLT.ToString());
}
