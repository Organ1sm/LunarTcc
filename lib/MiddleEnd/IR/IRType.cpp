//
// Created by Organ1sm.
//
#include "MiddleEnd/IR/IRType.hpp"
#include "BackEnd/Support.hpp"
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

unsigned IRType::GetStructMaxAlignment(TargetMachine *TM) const
{
    unsigned Alignment = 1;

    for (auto &type : MembersTypeList)
    {
        if (type.IsPointer())
        {
            unsigned PtrSize = TM ? TM->GetPointerSize() / 8 : 8;
            Alignment        = std::max(Alignment, PtrSize);
        }
        else if (type.IsArray())
        {
            if (type.GetBaseType().IsScalar())
            {
                Alignment =
                    std::max(Alignment,
                             static_cast<unsigned>(type.GetBaseType().GetByteSize(TM)));
            }
            else
                assert(!"Unhandled array base type");
        }
        else if (type.IsScalar())
        {
            Alignment = std::max(Alignment, static_cast<unsigned>(type.GetByteSize(TM)));
        }
        else if (type.IsStruct())
        {
            Alignment = std::max(Alignment,
                                 static_cast<unsigned>(type.GetStructMaxAlignment(TM)));
        }
        else
        {
            assert(!"Unhandled type.");
        }
    }

    return Alignment;
}

std::size_t IRType::GetByteSize(TargetMachine *TM) const
{
    unsigned NumberOfElements = 1;

    if (Dimensions.size() > 0)
        for (std::size_t i = 0; i < Dimensions.size(); i++)
            NumberOfElements *= Dimensions[i];

    if (IsStruct() && !IsPointer())
    {
        unsigned Result          = 0;
        const unsigned Alignment = GetStructMaxAlignment(TM);

        for (auto &t : MembersTypeList)
        {
            auto Size = t.GetByteSize();
            Result    = GetNextAlignedValue(Result, Size);
            Result += Size;
        }

        Result = GetNextAlignedValue(Result, Alignment);
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

    std::size_t i = PointerLevel > 1 ? dim : dim + 1;
    for (; i < Dimensions.size(); i++)
        result *= Dimensions[i];

    return result * (BitWidth / 8);
}

unsigned IRType::GetElemByteOffset(const unsigned int StructElemIndex,
                                   TargetMachine *TM) const
{
    assert(StructElemIndex < MembersTypeList.size() && "Out of bound");

    unsigned ByteOffset = 0;
    for (std::size_t i = 0; i < StructElemIndex; ++i)
    {
        const unsigned Size = MembersTypeList[i].GetByteSize();
        ByteOffset          = GetNextAlignedValue(ByteOffset, Size);

        ByteOffset += Size;
    }

    const unsigned MaxAlignment = GetStructMaxAlignment(TM);
    ByteOffset                  = GetNextAlignedValue(ByteOffset, MaxAlignment);

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

std::size_t IRType::GetBaseTypeByteSize(TargetMachine *TM) const
{
    auto CopyIRType = *this;
    CopyIRType.SetPointerLevel(0);
    CopyIRType.GetDimensions().clear();

    return CopyIRType.GetByteSize(TM);
}
