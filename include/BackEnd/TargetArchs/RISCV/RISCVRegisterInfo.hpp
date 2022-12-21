#pragma once

#include "BackEnd/RegisterInfo.hpp"
#include "BackEnd/TargetRegister.hpp"

namespace RISCV
{
    enum Registers : unsigned {
        INVALID,
#define RISCV_REGISTER(ID, WIDTH, NAME, ALIAS) ID,
#include "RISCVRegisters.def"
    };

    class RISCVRegisterInfo : public RegisterInfo
    {
      public:
        RISCVRegisterInfo();
        ~RISCVRegisterInfo() override {}

        TargetRegister *GetRegister(unsigned i) override;
        TargetRegister *GetRegisterByID(unsigned i) override;
        unsigned GetFrameRegister() override;
        unsigned GetStackRegister() override;

      private:
        TargetRegister Registers[32];
    };

}    // namespace RISCV
