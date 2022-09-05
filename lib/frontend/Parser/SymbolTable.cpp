//
// Created by Organ1sm.
//

#include "frontend/Parser/SymbolTable.hpp"

void SymbolTableStack::PushSymbolTable(SymbolTableStack::Table t)
{
    SymTabStack.push_back(std::move(t));
}

SymbolTableStack::Table SymbolTableStack::PopSymbolTable()
{
    assert(SymTabStack.size() > 0 && "Popping item from emtpy stack.");

    Table t = SymTabStack[SymTabStack.size() - 1];
    SymTabStack.pop_back();

    return t;
}

void SymbolTableStack::InsertEntry(const SymbolTableStack::Entry &e)
{
    auto idx = Size() > 0 ? Size() - 1 : 0;
    SymTabStack[idx].push_back(e);
}

bool SymbolTableStack::Contains(SymbolTableStack::Entry e)
{
    for (int i = Size() - 1; i >= 0; i--)
    {
        auto t = SymTabStack[i];
        for (int j = t.size() - 1; j >= 0; j--)
        {
            if (t[j] == e)
                return true;
        }
    }

    return false;
}

bool SymbolTableStack::ContainsInCurrentScope(SymbolTableStack::Entry e)
{
    auto idx = Size() > 0 ? Size() - 1 : 0;

    for (int i = SymTabStack[idx].size() - 1; i >= 0; i--)
    {
        if (SymTabStack[idx][i] == e)
            return true;
    }

    return false;
}
