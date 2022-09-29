//
// Created by Organ1sm.
//
#include "MiddleEnd/IR/IRType.hpp"

std::string IRType::AsString() const
{
    std::string Str;

    switch (Kind)
    {
        case FP:
            Str += "f";
            break;
        case UInt:
            Str += "u";
            break;
        case SInt:
            Str += "i";
            break;
        case None:
            return "void";
        default:
            assert(!"Invalid type.");
            break;
    }

    Str += std::to_string(BitWidth);

    if (NumberOfElements > 1)
        Str = "[" + Str + " x " + std::to_string(NumberOfElements) + "]";

    return Str;
}
