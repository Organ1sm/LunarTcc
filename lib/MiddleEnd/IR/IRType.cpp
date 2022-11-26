//
// Created by Organ1sm.
//
#include "MiddleEnd/IR/IRType.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "fmt/core.h"
#include "fmt/format.h"
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

void IRType::ReduceDimension()
{
    if (Dimensions.size() > 0)
        Dimensions.erase(Dimensions.begin());
}

std::size_t IRType::GetByteSize(TargetMachine *TM) const
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

    /// In case if it is a pointer type, then ask the target
    /// for the pointer size or if it was not given then default is 64.
    unsigned PtrSize = 64;
    if (TM)
        PtrSize = TM->GetPointerSize();

    return (PtrSize * NumberOfElements + 7) / 8;
}

unsigned IRType::CalcElemSize(unsigned int dim)
{
    unsigned result = 1;

    assert(dim == 0 || dim < Dimensions.size() && "Out of bound");

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
        case FP: Str += "f"; break;
        case UInt: Str += "u"; break;
        case SInt: Str += "i"; break;
        case Struct: Str += fmt::format("struct.{}", StructName); break;
        case None: return "void";
        default: assert(!"Invalid type."); break;
    }

    if (!IsStruct())
        Str += std::to_string(BitWidth);

    if (!Dimensions.empty())
    {
        for (int i = Dimensions.size() - 1; i >= 0; i--)
            Str = fmt::format("[{} x {}]", Dimensions[i], Str);
    }

    std::string PtrStr(PointerLevel, '*');

    return Str + PtrStr;
}
