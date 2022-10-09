#pragma once

#include "BackEnd/RegisterInfo.hpp"
#include "BackEnd/TargetABI.hpp"
#include "BackEnd/TargetRegister.hpp"


namespace AArch64
{

    class AArch64TargetABI : public TargetABI
    {
      public:
        AArch64TargetABI(RegisterInfo *RI);
    };
}    // namespace AArch64
