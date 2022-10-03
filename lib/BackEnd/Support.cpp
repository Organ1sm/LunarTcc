#include "BackEnd/Support.hpp"

uint64_t GetNextAlignedValue(unsigned int Val, unsigned int Alginment)
{
    return (Val + Alginment - 1) & ~(Alginment - 1);
}
