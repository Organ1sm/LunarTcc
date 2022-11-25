#include "BackEnd/GlobalData.hpp"
#include "fmt/core.h"
#include <cassert>

void GlobalData::InsertAllocation(std::size_t ByteSize, int64_t InitVal)
{
    Directives D = None;

    switch (ByteSize)
    {
        case 1: D = Byte; break;
        case 2: D = HalfWord; break;
        case 4: D = Word; break;
        case 8: D = DoubleWord;

        default:
            if (InitVal == 0)
            {
                D       = Zero;
                InitVal = Size;
            }
            else
            {
                assert(!"Invalid size");
            }
    }

    InitValues.push_back({D, std::to_string(InitVal)});
}


void GlobalData::InsertAllocation(std::string str, Directives d)
{
    InitValues.push_back({d, str});
}

std::string GlobalData::DirectivesToString(Directives D)
{
    switch (D)
    {
        case None: return "";
        case Zero: return "zero";
        case Byte: return "byte";
        case HalfWord: return "short";
        case Word: return "long";
        case DoubleWord: return "quad";
        case String: return "asciz";

        default: assert(!"Unreachable");
    }
}

void GlobalData::Print() const
{
    auto Str = fmt::format("{}:\n", Name);

    for (auto &[Directive, InitVal] : InitValues)
    {
        Str +=
            fmt::format("  .{}\t{}\n",
                        DirectivesToString(Directive),
                        Directive == String ? fmt::format("\"{}\"", InitVal) : InitVal);
    }

    fmt::print("{}\n", Str);
}
