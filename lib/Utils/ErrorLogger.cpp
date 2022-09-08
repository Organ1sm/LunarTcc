#include <iostream>
#include "Utils/ErrorLogger.hpp"


void PrintImpl(const char *str, unsigned int tab, bool newline)
{
    for (int i = 0; i < tab; i++)
        std::cout << " ";
    std::cout << str;

    if (newline)
        std::cout << std::endl;
}
void Print(const char *str, unsigned int tab) { PrintImpl(str, tab); }
void PrintLn(const char *str, unsigned int tab) { PrintImpl(str, tab, true); }
