#include <iostream>
#include "Utils/ErrorLogger.hpp"
#include "fmt/core.h"

void PrintImpl(const char *str, unsigned int tab, bool newline)
{
    auto s = fmt::format("{:^{}}{}", "", tab, str);
    fmt::print(s);

    if (newline)
        fmt::print("\n");
}

void Print(const char *str, unsigned int tab) { PrintImpl(str, tab); }

void PrintLn(const char *str, unsigned int tab) { PrintImpl(str, tab, true); }
