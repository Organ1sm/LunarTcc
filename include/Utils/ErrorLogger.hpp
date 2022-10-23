#pragma once

#include <fmt/core.h>
#include <fmt/color.h>

void PrintImpl(const char *str, unsigned tab = 0, bool newline = false);

void Print(const char *str, unsigned tab = 0);

void PrintLn(const char *str, unsigned tab = 0);

template <typename T, typename... Args>
void PrintError(const T &Msg, const Args &...args)
{
    fmt::print(stderr, fmt::emphasis::bold | fg(fmt::color::red), Msg, args...);
}
