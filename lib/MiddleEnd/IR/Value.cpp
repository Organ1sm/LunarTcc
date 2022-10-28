//
// Created by Organ1sm.
//
#include <cassert>
#include <cstdint>
#include <iostream>
#include "MiddleEnd/IR/Value.hpp"
#include "fmt/core.h"

std::string Value::ValueString() const
{
    return "$" + std::to_string(UniqueId) + "<" + ValueType.AsString() + ">";
}

uint64_t Constant::GetIntValue()
{
    assert(ValueType.IsInt());
    return std::get<uint64_t>(Val);
}

std::string Constant::ValueString() const
{
    if (IsFPConst())
        return std::to_string(std::get<double>(Val));
    else
        return std::to_string(std::get<uint64_t>(Val));
}


std::string GlobalVariable::ValueString() const
{
    return fmt::format("@{}<{}>", Name, ValueType.AsString());
}

void GlobalVariable::Print() const
{
    auto s = fmt::format("global-var ({}):\n\t{}", GetType().AsString(), Name);
    fmt::print(s);

    if (!InitList.empty())
    {
        std::string InitListFormat = " = {{ {} }}";
        std::string OutputStr;

        for (std::size_t i = 0; i < InitList.size(); i++)
        {
            OutputStr += fmt::format(" {}", InitList[i]);
            if (i + 1 < InitList.size())
                OutputStr += ", ";
        }

        fmt::print(InitListFormat, OutputStr);
    }

    fmt::print("\n\n");
}
