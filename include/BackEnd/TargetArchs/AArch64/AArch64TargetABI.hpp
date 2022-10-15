#pragma once

#include "BackEnd/TargetABI.hpp"

class RegisterInfo;

namespace AArch64
{

    class AArch64TargetABI : public TargetABI
    {
      public:
        AArch64TargetABI(RegisterInfo *RI);
    };
}    // namespace AArch64
