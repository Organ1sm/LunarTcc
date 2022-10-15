//
// Created by Organ1sm.
//

#include "FrontEnd/AST/Type.hpp"

std::string Type::ToString(const Type::VariantKind vk)
{
    switch (vk)
    {
        case Double:
            return "double";
        case Int:
            return "int";
        case Char:
            return "char";
        case Void:
            return "void";
        case Invalid:
            return "invalid";
        default:
            assert(false && "Unknown type.");
            break;
    }
}

bool Type::IsImplicitlyCastable(const Type::VariantKind from, const Type::VariantKind to)
{
    return (from == Int && to == Double) || (from == Double && to == Int)
           || (from == Char && to == Int) || (from == Int && to == Char);
}

std::string ArrayType::ToString() const
{
    auto TypeStr = Type::ToString(Ty);

    for (auto Dimension : Dimensions)
        TypeStr += "[" + std::to_string(Dimension) + "]";

    return TypeStr;
}

std::string FunctionType::ToString() const
{
    auto TypeStr = Type::ToString(GetReturnType());
    auto ArgSize = ArgumentTypes.size();

    if (ArgSize != 0)
        TypeStr += " (";

    for (std::size_t i = 0; i < ArgSize; i++)
    {
        TypeStr += Type::ToString(ArgumentTypes[i]);
        if (i == ArgSize - 1)
            break;

        TypeStr += ", ";
    }

    if (ArgSize != 0)
        TypeStr += ")";

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
        for (std::size_t i = 0; i < ArgSize; i++)
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

        for (auto Dimension : Dimensions)
            TyStr += "[" + std::to_string(Dimension) + "]";
        return TyStr;
    }
    else
    {
        return Type::ToString(Ty);
    }
}

ComplexType::ComplexType(Type t, std::vector<unsigned int> d)
{
    if (d.size() == 0)
    {
        Kind = Simple;
        Ty   = t.GetTypeVariant();
    }
    else
    {
        Kind       = Array;
        Dimensions = std::move(d);
    }
    Ty = t.GetTypeVariant();
}

ComplexType::ComplexType(Type t, std::vector<VariantKind> a)
{
    Ty            = t.GetTypeVariant();
    Kind          = Function;
    ArgumentTypes = std::move(a);
}

bool operator==(const ComplexType &lhs, const ComplexType &rhs)
{
    bool result = lhs.Kind == rhs.Kind && lhs.Ty == rhs.Ty;

    if (!result)
        return false;

    switch (lhs.Kind)
    {
        case ComplexType::Function:
            result = result && (lhs.ArgumentTypes == rhs.ArgumentTypes);
            break;
        case ComplexType::Array:
            result = result && (lhs.Dimensions == rhs.Dimensions);
            break;
        default:
            break;
    }
    return result;
}

ComplexType::ComplexType(ComplexType &&ct)
{
    Ty   = ct.Ty;
    Kind = ct.Kind;

    if (ct.Kind == Array)
        Dimensions = std::move(ct.Dimensions);
    else if (ct.Kind == Function)
        ArgumentTypes = std::move(ct.ArgumentTypes);
}

ComplexType::ComplexType(const ComplexType &ct)
{
    Ty   = ct.Ty;
    Kind = ct.Kind;

    if (ct.Kind == Array)
        Dimensions = ct.Dimensions;
    else if (ct.Kind == Function)
        ArgumentTypes = ct.ArgumentTypes;
}

ComplexType &ComplexType::operator=(const ComplexType &ct)
{
    Ty   = ct.Ty;
    Kind = ct.Kind;

    if (ct.Kind == Array)
        Dimensions = ct.Dimensions;
    else if (ct.Kind == Function)
        ArgumentTypes = ct.ArgumentTypes;

    return *this;
}

bool operator==(const ValueType &lhs, const ValueType &rhs)
{
    bool result = (lhs.Kind == rhs.Kind);

    if (!result)
        return false;

    switch (lhs.Kind)
    {
        case ValueType::Integer:
            result = result && (lhs.IntVal == rhs.IntVal);
            break;
        case ValueType::Float:
            result = result && (lhs.FloatVal == rhs.FloatVal);
            break;
        default:
            break;
    }
    return result;
}
