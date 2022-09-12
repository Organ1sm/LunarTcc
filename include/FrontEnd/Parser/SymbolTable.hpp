//
// Created by Organ1sm.
//

#ifndef LUNARTCC_SYMBOLTABLE_HPP
#define LUNARTCC_SYMBOLTABLE_HPP

#include "FrontEnd/AST/Type.hpp"
#include <cassert>
#include <optional>

class SymbolTableStack
{
  public:
    using Entry = std::tuple<std::string, ComplexType, ValueType>;
    using Table = std::vector<Entry>;

    // Adding the first empty table
    SymbolTableStack() { SymTabStack.emplace_back(Table()); }

    void PushSymbolTable(Table t = Table());
    Table PopSymbolTable();

    std::size_t Size() { return SymTabStack.size(); }

    Table &GetTopTable() { return SymTabStack.back(); }

    void InsertEntry(const Entry &e);
    void InsertGlobalEntry(const Entry &e);
    bool Contains(Entry e);
    std::optional<Entry> Contains(const std::string &sym);
    bool ContainsInCurrentScope(Entry e);

  private:
    std::vector<Table> SymTabStack;
};



#endif    // LUNARTCC_SYMBOLTABLE_HPP
