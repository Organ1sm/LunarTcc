#include <cassert>
#include <iostream>
#include "BackEnd/StackFrame.hpp"
#include "BackEnd/Support.hpp"
#include "fmt/core.h"

void StackFrame::InsertStackSlot(unsigned int ID, unsigned int Size)
{
    assert(StackSlots.count(ID) == 0 && "Already existing object on the stack.");

    ObjectsSize = GetNextAlignedValue(ObjectsSize, Size);
    StackSlots.insert({ID, Size});
}

unsigned StackFrame::GetPosition(unsigned int ID)
{
    assert(IsStackSlot(ID) && "Must be a valid stack slot ID.");

    unsigned Position = 0;
    for (const auto &[ObjectID, ObjectSize] : StackSlots)
    {
        if (ObjectID == ID)
            return GetNextAlignedValue(Position, ObjectSize);

        Position = GetNextAlignedValue(Position, ObjectSize);
        Position += ObjectSize;
    }

    return ~0;    // Error;
}

unsigned StackFrame::GetSize(unsigned int ID)
{
    assert(IsStackSlot(ID) && "Must be a valid stack slot ID.");

    return StackSlots[ID];
}

void StackFrame::Print() const
{
    unsigned Num         = 0;
    std::string SFFormat = "\t\tPosition: {}, ID: {}, Size: {}\n";

    fmt::print("{:>14}: {}\n", "FrameSize", ObjectsSize);

    for (const auto &[FrameObjID, FrameObjSize] : StackSlots)
    {
        fmt::print(SFFormat, Num++, FrameObjID, FrameObjSize);
    }

    fmt::print("\n");
}
