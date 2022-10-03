#include <atomic>
#include <cassert>
#include "BackEnd/StackFrame.hpp"

void StackFrame::InsertStackSlot(unsigned int ID, unsigned int Size)
{
    assert(StackSlots.count(ID) == 0 && "Already existing object on the stack.");
    ObjectSize += Size;

    StackSlots.insert({ID, Size});
}

unsigned StackFrame::GetPosition(unsigned int ID)
{
    assert(IsStackSlot(ID) && "Must be a valid stack slot ID.");

    unsigned Position = 0;
    for (const auto &Entry : StackSlots)
    {
        if (Entry.first == ID)
            return Position;

        Position += Entry.second;    // Entry.second -> object size
    }

    return ~0;    // Error;
}
