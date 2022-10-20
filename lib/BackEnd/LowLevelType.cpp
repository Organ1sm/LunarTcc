#include "BackEnd/LowLevelType.hpp"

LowLevelType LowLevelType::CreateInt(unsigned int BW)
{
    LowLevelType LLT(LowLevelType::Integer);
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

    if (Type == LowLevelType::Integer)
        str = "i";
    else if (Type == LowLevelType::FloatingPoint)
        str = "f";
    else if (Type == LowLevelType::Pointer)
        str = "p";

    str += std::to_string(BitWidth);

    return str;
}
