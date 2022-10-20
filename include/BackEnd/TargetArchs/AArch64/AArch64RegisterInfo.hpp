#pragma once

#include "BackEnd/RegisterInfo.hpp"

class TargetRegister;

namespace AArch64
{
    enum Registers : unsigned {
        Invalid,
#define AARCH64_REGISTER(ID, WIDTH, NAME, ALIAS) ID,
#include "AArch64Registers.def"
    };

    class AArch64RegisterInfo : public RegisterInfo
    {
      public:
        AArch64RegisterInfo();
        ~AArch64RegisterInfo() override {}

        TargetRegister *GetRegister(unsigned i) override;
        TargetRegister *GetRegisterByID(unsigned i) override;

        unsigned GetFrameRegister() override;
        unsigned GetLinkRegister() override;
        unsigned GetStackRegister() override;

      private:
        TargetRegister Registers[67];
    };
}    // namespace AArch64
