//
// Created by Organ1sm.
//

#pragma once

#include "FrontEnd/AST/Type.hpp"
#include <cassert>
#include <optional>
#include <tuple>
#include <variant>

class Token;

class SymbolTableStack
{
  public:
    using Entry = std::tuple<Token, Type, ValueType>;
    using Table = std::vector<Entry>;

    // Adding the first empty table
    SymbolTableStack() { SymTabStack.emplace_back(Table()); }

    void PushSymbolTable(Table t = Table());
    Table PopSymbolTable();

    std::size_t Size() { return SymTabStack.size(); }

    Table &GetTopTable() { return SymTabStack.back(); }

    void InsertEntry(const Entry &e);
    void InsertGlobalEntry(const Entry &e);

    std::optional<Entry> Contains(const std::string &sym);
    std::optional<Entry> ContainsInCurrentScope(const std::string &sym);
    std::optional<Entry> ContainsInGlobalScope(const std::string &sym);


  private:
    std::vector<Table> SymTabStack;
};
