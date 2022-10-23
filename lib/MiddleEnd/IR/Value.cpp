//
// Created by Organ1sm.
//
#include <iostream>
#include "MiddleEnd/IR/Value.hpp"
#include "fmt/core.h"

std::string Value::ValueString() const
{
    return "$" + std::to_string(UniqueId) + "<" + ValueType.AsString() + ">";
}

std::string Constant::ValueString() const
{
    if (IsFPConst())
        return std::to_string(std::get<double>(Val));
    else
        return std::to_string(std::get<uint64_t>(Val));
}

void GlobalVariable::Print() const
{
    auto s = fmt::format("global {} :{};\n\n", Name, GetType().AsString());
    fmt::print(s);
}
