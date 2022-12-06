//
// Created by Organ1sm.
//

#include "FrontEnd/Parser/SymbolTable.hpp"
#include "FrontEnd/Lexer/Token.hpp"
#include <optional>

void SymbolTableStack::PushSymbolTable(SymbolTableStack::Table t)
{
    SymTabStack.push_back(std::move(t));
}

SymbolTableStack::Table SymbolTableStack::PopSymbolTable()
{
    assert(!SymTabStack.empty() && "Popping item from emtpy stack.");

    Table t = SymTabStack[SymTabStack.size() - 1];
    SymTabStack.pop_back();

    return t;
}

void SymbolTableStack::InsertEntry(const SymbolTableStack::Entry &e)
{
    auto idx = Size() > 0 ? Size() - 1 : 0;
    SymTabStack[idx].push_back(e);
}

void SymbolTableStack::InsertGlobalEntry(const SymbolTableStack::Entry &e)
{
    SymTabStack[0].push_back(e);
}


std::optional<SymbolTableStack::Entry> SymbolTableStack::Contains(const std::string &sym)
{
    for (int i = Size() - 1; i >= 0; i--)
    {
        auto t = SymTabStack[i];
        for (int j = t.size() - 1; j >= 0; j--)
        {
            if (sym == std::get<0>(t[j]).GetString())
                return t[j];
        }
    }
    return std::nullopt;
}

std::optional<SymbolTableStack::Entry>
    SymbolTableStack::ContainsInCurrentScope(const std::string &sym)
{
    auto table = SymTabStack.back();

    for (int i = table.size() - 1; i >= 0; i--)
    {
        if (sym == std::get<0>(table[i]).GetString())
            return table[i];
    }

    return std::nullopt;
}

std::optional<SymbolTableStack::Entry>
    SymbolTableStack::ContainsInGlobalScope(const std::string &sym)
{
    auto table = SymTabStack[0];

    for (int i = table.size() - 1; i >= 0; i--)
    {
        if (sym == std::get<0>(table[i]).GetString())
            return table[i];
    }

    return std::nullopt;
}
