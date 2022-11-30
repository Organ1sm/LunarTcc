#pragma once

#include "BackEnd/RegisterInfo.hpp"

class TargetRegister;

namespace AArch64
{
    enum Registers : unsigned {
        Invalid,

#define AARCH64_REGISTER(ID, WIDTH, NAME, ALIAS) ID,
#include "AArch64Registers.def"

        RegisterEnd,
    };

    class AArch64RegisterInfo : public RegisterInfo
    {
      public:
        AArch64RegisterInfo();
        ~AArch64RegisterInfo() override {}

        TargetRegister *GetParentReg(unsigned ID) override;
        TargetRegister *GetRegister(unsigned i) override;
        TargetRegister *GetRegisterByID(unsigned i) override;

        unsigned GetFrameRegister() override;
        unsigned GetLinkRegister() override;
        unsigned GetStackRegister() override;
        unsigned GetZeroRegister(const unsigned BitWidth) override;
        unsigned GetStructPtrRegister() override;

        unsigned GetRegisterClass(const unsigned BitWidth, const bool IsFP) override;
        std::string GetRegClassString(const unsigned RegClass) override;
        unsigned GetRegClassFromReg(const unsigned Reg) override;
        unsigned GetRegClassRegsSize(const unsigned RegClass) override;

      private:
        TargetRegister Registers[RegisterEnd - 1];
        std::vector<std::string> RegClassEnumStrings;
    };
}    // namespace AArch64
