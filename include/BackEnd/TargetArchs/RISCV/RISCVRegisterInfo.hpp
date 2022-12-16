#pragma once

#include "BackEnd/RegisterInfo.hpp"
#include "BackEnd/TargetRegister.hpp"

namespace RISCV
{
    enum Registers : unsigned {
        Invalid,

#define RISCV_REGISTER(ID, WIDTH, NAME, ALIAS) ID,
#include "RISCVRegisters.def"

        RegisterEnd
    };

    class RISCVRegisterInfo : public RegisterInfo
    {
      public:
        RISCVRegisterInfo();
        ~RISCVRegisterInfo() override {}

        TargetRegister *GetRegister(unsigned i) override;
        TargetRegister *GetRegisterByID(unsigned i) override;
        TargetRegister *GetParentReg(unsigned ID) override;

        unsigned GetFrameRegister() override;
        unsigned GetLinkRegister() override;
        unsigned GetStackRegister() override;
        unsigned GetStructPtrRegister() override;
        unsigned GetZeroRegister(const unsigned BitWidth) override;

        unsigned GetRegisterClass(const unsigned BitWidth, const bool IsFP) override;
        std::string GetRegClassString(const unsigned RegClass) override;
        unsigned GetRegClassFromReg(const unsigned int Reg) override;
        unsigned GetRegClassRegsSize(const unsigned int RegClass) override;

      private:
        TargetRegister Registers[RegisterEnd - 1];
        std::vector<std::string> RegClassEnumStrings;
    };

}    // namespace RISCV
