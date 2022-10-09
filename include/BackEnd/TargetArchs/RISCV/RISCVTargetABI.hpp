#pragma once

#include "BackEnd/RegisterInfo.hpp"
#include "BackEnd/TargetABI.hpp"
#include "BackEnd/TargetRegister.hpp"


namespace RISCV
{

    class RISCVTargetABI : public TargetABI
    {
      public:
        RISCVTargetABI(RegisterInfo *RI);
    };

}    // namespace RISCV
