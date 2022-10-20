//
// Created by Organ1sm.
//

#include "FrontEnd/AST/Type.hpp"

Type::Type(Type::TypeKind tk) : Kind(tk)
{
    switch (tk)
    {
        case Array:
        case Struct:
        case Function:
            Ty = Composite;
            break;
        case Simple:
        default:
            Ty = Invalid;
            break;
    }
}

Type::Type(Type t, std::vector<unsigned> d) : Type(t)
{
    if (d.empty())
    {
        Kind = Simple;
    }
    else
    {
        Kind       = Array;
        Dimensions = std::move(d);
    }
}

Type::Type(Type t, std::vector<Type> a)
{
    Kind     = Function;
    TypeList = std::move(a);
    Ty       = t.GetTypeVariant();
}

Type::Type(Type &&ct)
{
    PointerLevel = ct.PointerLevel;
    Ty           = ct.Ty;
    Kind         = ct.Kind;
    Dimensions   = std::move(ct.Dimensions);
    TypeList     = std::move(ct.TypeList);
    Name         = ct.Name;
}

Type &Type::operator=(Type &&ct)
{
    PointerLevel = ct.PointerLevel;
    Ty           = ct.Ty;
    Kind         = ct.Kind;
    Dimensions   = std::move(ct.Dimensions);
    TypeList     = std::move(ct.TypeList);
    Name         = ct.Name;

    return *this;
}

Type::Type(const Type &ct)
{
    PointerLevel = ct.PointerLevel;
    Ty           = ct.Ty;
    Kind         = ct.Kind;
    Dimensions   = ct.Dimensions;
    TypeList     = ct.TypeList;
    Name         = ct.Name;
}

Type &Type::operator=(const Type &ct)
{
    PointerLevel = ct.PointerLevel;
    Ty           = ct.Ty;
    Kind         = ct.Kind;
    Dimensions   = ct.Dimensions;
    TypeList     = ct.TypeList;
    Name         = ct.Name;

    return *this;
}

std::vector<unsigned> &Type::GetDimensions()
{
    assert(IsArray() && "Must be an Array type to access Dimensions.");
    return Dimensions;
}

std::vector<Type> &Type::GetArgTypes()
{
    assert(IsFunction() && "Must be a Function type to access TypeList.");
    return TypeList;
}

void Type::DecrementPointerLevel()
{
    assert(PointerLevel > 0 && "Cannot decrement below 0");
    PointerLevel--;
}

std::string Type::ToString(const Type &t)
{
    switch (t.GetTypeVariant())
    {
        case Double:
            return "double";
        case Int:
            return "int";
        case Char:
            return "char";
        case Void:
            return "void";
        case Composite:
            return t.GetName();
        case Invalid:
            return "invalid";
        default:
            assert(false && "Unknown type.");
            break;
    }
}

std::string Type::ToString() const
{
    if (Kind == Function)
    {
        auto TyStr   = Type::ToString(*this);
        auto ArgSize = TypeList.size();
        if (ArgSize > 0)
            TyStr += " (";
        for (size_t i = 0; i < ArgSize; i++)
        {
            TyStr += Type::ToString(TypeList[i]);
            if (i + 1 < ArgSize)
                TyStr += ",";
            else
                TyStr += ")";
        }
        return TyStr;
    }
    else if (Kind == Array)
    {
        auto TyStr = Type::ToString(*this);

        for (size_t i = 0; i < Dimensions.size(); i++)
            TyStr += "[" + std::to_string(Dimensions[i]) + "]";
        return TyStr;
    }
    else
    {
        return Type::ToString(*this);
    }
}

bool Type::IsImplicitlyCastable(const Type::VariantKind from, const Type::VariantKind to)
{
    return (from == Int && to == Double) || (from == Double && to == Int)
           || (from == Char && to == Int) || (from == Int && to == Char);
}

bool operator==(const Type &lhs, const Type &rhs)
{
    bool result = lhs.Kind == rhs.Kind && lhs.Ty == rhs.Ty;

    if (!result)
        return false;

    switch (lhs.Kind)
    {
        case Type::Function:
            result = result && (lhs.TypeList == rhs.TypeList);
            break;
        case Type::Array:
            result = result && (lhs.Dimensions == rhs.Dimensions);
            break;
        default:
            break;
    }
    return result;
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
