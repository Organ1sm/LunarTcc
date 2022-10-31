#pragma once

#include "BackEnd/TargetRegister.hpp"
#include <cassert>

class RegisterInfo
{
  public:
    RegisterInfo() {}
    virtual ~RegisterInfo() {}

    virtual unsigned GetFrameRegister() { return 0; }
    virtual unsigned GetLinkRegister() { return 0; }
    virtual unsigned GetStackRegister() { return 0; }

    virtual TargetRegister *GetRegister(unsigned i)
    {
        assert(!"Unimplemented");
        return nullptr;
    }

    virtual TargetRegister *GetRegisterByID(unsigned i)
    {
        assert(!"Unimplemented");
        return nullptr;
    }
};
