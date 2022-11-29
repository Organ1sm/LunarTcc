#include "BackEnd/LowLevelType.hpp"
#include <cassert>

LowLevelType LowLevelType::CreateScalar(unsigned int BW)
{
    LowLevelType LLT(LowLevelType::Scalar);
    LLT.SetBitWidth(BW);

    return LLT;
}

LowLevelType LowLevelType::CreatePtr(unsigned int BW)
{
    LowLevelType LLT(LowLevelType::Pointer);
    LLT.SetBitWidth(BW);

    return LLT;
}

std::string LowLevelType::ToString() const
{
    std::string str;

    if (Type == LowLevelType::Scalar)
        str = "s";
    else if (Type == LowLevelType::Pointer)
        str = "p";
    else
        assert(!"Invalid type.");

    str += std::to_string(BitWidth);

    return str;
}
