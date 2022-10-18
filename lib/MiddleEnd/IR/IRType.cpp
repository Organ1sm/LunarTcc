//
// Created by Organ1sm.
//
#include "MiddleEnd/IR/IRType.hpp"
#include <cassert>

void IRType::SetPointerLevel(uint8_t pl)
{
    assert(pl < 10 && "Unrealistic pointer level");
    PointerLevel = pl;
}

void IRType::DecrementPointerLevel()
{
    assert(PointerLevel > 0 && "Cannot decrement bleow 0");
    PointerLevel--;
}

std::size_t IRType::GetByteSize() const
{
    unsigned NumberOfElements = 1;

    if (Dimensions.size() > 0)
        for (std::size_t i = 0; i < Dimensions.size(); i++)
            NumberOfElements *= Dimensions[i];

    if (IsStruct())
    {
        unsigned Result = 0;
        for (auto &t : MembersTypeList)
            Result += t.GetByteSize();

        return Result;
    }

    if (PointerLevel == 0)
        return (BitWidth * NumberOfElements + 7) / 8;

    return (32 * NumberOfElements + 7) / 8;
}

unsigned IRType::CalcElemSize(unsigned int dim)
{
    unsigned result = 1;

    assert(dim < Dimensions.size() && "Out of bound");

    for (std::size_t i = dim + 1; i < Dimensions.size(); i++)
        result *= Dimensions[i];

    return result * (BitWidth / 8);
}

unsigned IRType::GetElemByteOffset(const unsigned int StructElemIndex) const
{
    assert(StructElemIndex < MembersTypeList.size() && "Out of bound");

    unsigned ByteOffset = 0;
    for (std::size_t i = 0; i < StructElemIndex; ++i)
        ByteOffset += MembersTypeList[i].GetByteSize();

    return ByteOffset;
}

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
        case Struct:
            Str += StructName;
            break;
        case None:
            return "void";
        default:
            assert(!"Invalid type.");
            break;
    }

    if (!IsStruct())
        Str += std::to_string(BitWidth);

    if (!Dimensions.empty())
    {
        for (auto CurrentDim : Dimensions)
            Str += "[" + Str + " x " + std::to_string(CurrentDim) + "]";
    }

    std::string PtrStr;
    for (auto i = 0; i < PointerLevel; i++)
        PtrStr += "*";

    return PtrStr + Str;
}
