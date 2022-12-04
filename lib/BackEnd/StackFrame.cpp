#include <cassert>
#include <iostream>
#include "BackEnd/StackFrame.hpp"
#include "BackEnd/Support.hpp"
#include "fmt/core.h"

void StackFrame::InsertStackSlot(unsigned int ID, unsigned int Size, unsigned Align)
{
    assert(StackSlots.count(ID) == 0 && "Already existing object on the stack.");

    ObjectsSize = GetNextAlignedValue(ObjectsSize, Align);
    ObjectsSize += Size;

    StackSlots.insert({
        ID,
        {Size, Align}
    });
}

unsigned StackFrame::GetPosition(unsigned int ID)
{
    assert(IsStackSlot(ID) && "Must be a valid stack slot ID.");

    unsigned Position = 0;
    for (const auto &[ObjectID, ObjectSizeAndAlign] : StackSlots)
    {
        const auto &[ObjectSize, ObjectAlign] = ObjectSizeAndAlign;
        if (ObjectID == ID)
            return GetNextAlignedValue(Position, ObjectAlign);

        Position = GetNextAlignedValue(Position, ObjectAlign);
        Position += ObjectSize;
    }

    return ~0;    // Error;
}

unsigned StackFrame::GetSize(unsigned int ID)
{
    assert(IsStackSlot(ID) && "Must be a valid stack slot ID.");

    return StackSlots[ID].first;
}

void StackFrame::Print() const
{
    unsigned Num         = 0;
    std::string SFFormat = "\t\tPosition: {}, ID: {}, Size: {}, Align: {}\n";

    fmt::print("{:>14}: {}\n", "FrameSize", ObjectsSize);

    for (const auto &[FrameObjID, FrameObjSizeAndAlign] : StackSlots)
    {
        fmt::print(SFFormat,
                   Num++,
                   FrameObjID,
                   FrameObjSizeAndAlign.first,
                   FrameObjSizeAndAlign.second);
    }

    fmt::print("\n");
}
