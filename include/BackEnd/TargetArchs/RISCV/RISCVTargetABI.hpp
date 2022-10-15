#pragma once

#include "BackEnd/TargetABI.hpp"

class RegisterInfo;

namespace RISCV
{

    class RISCVTargetABI : public TargetABI
    {
      public:
        RISCVTargetABI(RegisterInfo *RI);
    };

}    // namespace RISCV
