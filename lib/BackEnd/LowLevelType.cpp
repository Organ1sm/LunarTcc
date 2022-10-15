#include "BackEnd/LowLevelType.hpp"


LowLevelType LowLevelType::CreateInt(unsigned int BW)
{
    LowLevelType LLT(Integer);
    LLT.SetBitWidth(BW);

    return LLT;
}

std::string LowLevelType::ToString() const 
{
    std::string str;

    if (Type == Integer)
        str = "i";
    else if (Type == FloatingPoint)
        str = "f";

    str += std::to_string(BitWidth);

    return str;
}
