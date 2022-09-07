//
// Created by Organ1sm.
//

#include "frontend/AST/Type.hpp"


std::string Type::ToString(const Type::VariantKind vk)
{
    switch (vk)
    {
        case Double:
            return "double";
        case Int:
            return "int";
        case Void:
            return "void";
        case InValid:
            return "invalid";
        default:
            assert(false && "Unknown type.");
            break;
    }
}

std::string ArrayType::ToString() const
{
    auto TypeStr = Type::ToString(Ty);

    for (int i = 0; i < Dimensions.size(); i++)
        TypeStr += "[" + std::to_string(Dimensions[i]) + "]";

    return TypeStr;
}

std::string FunctionType::ToString() const
{
    auto TypeStr = Type::ToString(GetReturnType());
    auto ArgSize = ArgumentTypes.size();

    if (ArgSize != 0)
        TypeStr += " (";

    for (int i = 0; i < ArgSize; i++)
    {
        TypeStr += Type::ToString(ArgumentTypes[i]);
        TypeStr += ", ";
    }

    if (ArgSize != 0)
        TypeStr += " )";

    return TypeStr;
}

std::string ComplexType::ToString() const
{
    if (Kind == Function)
    {
        auto TyStr   = Type::ToString(Ty);
        auto ArgSize = ArgumentTypes.size();
        if (ArgSize > 0)
            TyStr += " (";
        for (int i = 0; i < ArgSize; i++)
        {
            TyStr += Type::ToString(ArgumentTypes[i]);
            if (i + 1 < ArgSize)
                TyStr += ",";
            else
                TyStr += ")";
        }
        return TyStr;
    }
    else if (Kind == Array)
    {
        auto TyStr = Type::ToString(Ty);

        for (int i = 0; i < Dimensions.size(); i++)
            TyStr += "[" + std::to_string(Dimensions[i]) + "]";
        return TyStr;
    }
    else
    {
        return Type::ToString(Ty);
    }
}

ComplexType::ComplexType(Type t, const std::vector<unsigned int> &d)
{
    if (d.size() == 0)
        ComplexType(t);
    else
    {
        Kind       = Array;
        Dimensions = d;
    }
    Ty = t.GetTypeVariant();
}
