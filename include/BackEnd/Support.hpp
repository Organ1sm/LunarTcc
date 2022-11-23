#ifndef LUNARTCC_SUPPORT_HPP
#define LUNARTCC_SUPPORT_HPP

#include <cstdint>
#include <string>
#include <vector>

bool IsInt(uint64_t Number, unsigned BitWidth);

template <unsigned BitWidth>
bool IsInt(uint64_t Number)
{
    int64_t S = ((int64_t)Number) >> BitWidth;
    return S == 0 || S == -1;
}

template <unsigned BitWidth>
bool IsUInt(uint64_t NUmber)
{
    int64_t S = ((int64_t)NUmber) >> BitWidth;
    return S == 0;
}


// Only use with power of 2 alignments
// FixMe: make it moore general and safe
uint64_t GetNextAlignedValue(unsigned Val, unsigned Alginment);

struct Filer
{
    static bool getFileContent(const std::string &fileName,
                               std::vector<std::string> &VecOfStrs);
};

#endif    // !LUNARTCC_SUPPORT_HPP
