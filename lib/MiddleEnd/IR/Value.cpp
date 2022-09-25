//
// Created by Organ1sm.
//
#include <iostream>
#include "middleend/IR/Value.hpp"

std::string Constant::ValueString() const
{
    if (IsFPConst())
        return std::to_string(std::get<double>(Val));
    else
        return std::to_string(std::get<uint64_t>(Val));
}


void GlobalVariable::Print() const
{
    std::cout << "global " << Name << " :" << GetType().AsString() << ";" << std::endl
              << std::endl;
}
