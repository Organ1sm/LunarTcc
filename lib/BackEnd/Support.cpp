#include "BackEnd/Support.hpp"
#include "Utils/ErrorLogger.hpp"
#include <iostream>
#include <fstream>

uint64_t GetNextAlignedValue(unsigned int Val, unsigned int Alginment)
{
    return (Val + Alginment - 1) & ~(Alginment - 1);
}

bool Filer::getFileContent(const std::string &fileName,
                           std::vector<std::string> &VecOfStrs)
{
    std::ifstream in(fileName.c_str());

    if (!in)
    {
        PrintError("Cannot open the file: '{}'\n", fileName);
        return false;
    }

    std::string str;

    while (std::getline(in, str))
    {
        VecOfStrs.push_back(std::move(str));
    }

    in.close();
    return true;
}

bool IsInt(uint64_t Number, unsigned BitWidth)
{
    int64_t S = ((int64_t)Number) >> BitWidth;
    return S == 0 || S == -1;
}
