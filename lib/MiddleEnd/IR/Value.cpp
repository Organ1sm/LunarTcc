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

uint64_t Constant::GetIntValue() const
{
    assert(ValueType.IsInt());

    int64_t result;
    auto value = std::get<uint64_t>(Val);

    // signed extend it
    // TODO: maybe value should be stored as int64_t

    if (GetBitWidth() == 8)
        result = static_cast<int8_t>(value);
    else if (GetBitWidth() == 16)
        result = static_cast<int16_t>(value);
    else if (GetBitWidth() == 32)
        result = static_cast<int32_t>(value);
    else
        result = value;

    return result;
}

double Constant::GetFloatValue() const
{
    assert(IsFPType());
    assert(GetBitWidth() == 64 || GetBitWidth() == 32);

    double result = std::get<double>(Val);

    return result;
}

std::string Constant::ValueString() const
{
    std::string Str;
    if (IsFPType())
        Str += std::to_string(std::get<double>(Val));
    else
        Str += std::to_string((int64_t)std::get<uint64_t>(Val));

    Str += fmt::format("<{}>", ValueType.AsString());

    return Str;
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
            OutputStr += fmt::format("{}", InitList[i]);
            if (i + 1 < InitList.size())
                OutputStr += ", ";
        }

        fmt::print(InitListFormat, OutputStr);
    }
    else if (!InitString.empty())
    {
        fmt::print(" = \"{}\"", InitString);
    }
    else if (auto GV = dynamic_cast<GlobalVariable *>(InitValue); GV != nullptr)
    {
        fmt::print(" = {}", GV->GetName());
    }

    fmt::print("\n\n");
}
