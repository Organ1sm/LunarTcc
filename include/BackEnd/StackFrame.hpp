#ifndef LUNARTCC_STACKFRAME_HPP
#define LUNARTCC_STACKFRAME_HPP

#include <map>

class StackFrame
{
    using StackSlotMap = std::map<unsigned, unsigned>;

  public:
    StackFrame() {}

    unsigned GetSize() const { return ObjectSize; }
    unsigned GetEntriesCount() const { return StackSlots.size(); }

    // FIXME: Size should be incremented way more sophistaced way. For
    // example having four 1 byte size objects on the stack, but we
    // cannot access the stack by 1 byte granuality. In a 32 bit machine
    // Mostlikely each 1 byte object would take up a 4 byte slot.
    void InsertStackSlot(unsigned ID, unsigned Size);

    bool IsStackSlot(unsigned ID) const { return 0 != StackSlots.count(ID); }

    unsigned GetPosition(unsigned ID);
    unsigned GetSize(unsigned ID);

    void Print() const;

  private:
    unsigned ObjectSize {0};

    // Maps an object ID to Its size. The order of the entries represents
    // the order of the objects pushed to the stack.
    StackSlotMap StackSlots;
};

#endif    // !LUNARTCC_STACKFRAME_HPP
