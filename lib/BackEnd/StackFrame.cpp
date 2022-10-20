#include <cassert>
#include <iostream>
#include "BackEnd/StackFrame.hpp"

void StackFrame::InsertStackSlot(unsigned int ID, unsigned int Size)
{
    assert(StackSlots.count(ID) == 0 && "Already existing object on the stack.");

    if (Size >= 4)
        ObjectSize += Size;
    else
        ObjectSize += 4;

    StackSlots.insert({ID, Size});
}

unsigned StackFrame::GetPosition(unsigned int ID)
{
    assert(IsStackSlot(ID) && "Must be a valid stack slot ID.");

    unsigned Position = 0;
    for (const auto &[ObjectID, ObjectSize] : StackSlots)
    {
        if (ObjectID == ID)
            return Position;

        // NOTE: Hard coded 4 byte alignment
        if (ObjectSize >= 4)
            Position += ObjectSize;
        else
            Position += 4;    // Entry.second -> object size
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
    unsigned Num = 0;

    std::cout << "\t\tFrameSize: " << ObjectSize << std::endl;

    for (const auto &FrameObj : StackSlots)
    {
        std::cout << "\t\tPosition: " << Num++ << ",  ID: " << FrameObj.first
                  << ", Size: " << FrameObj.second << std::endl;
    }

    std::cout << std::endl;
}
