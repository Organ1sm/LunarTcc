//
// Created by Organ1sm.
//

#include "FrontEnd/AST/Type.hpp"

Type::Type(Type::TypeKind tk) : Kind(tk)
{
    switch (tk)
    {
        case Array:
        case Struct: Ty = Composite; break;

        case Simple:
        default: Ty = Invalid; break;
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
    ParamList = std::move(a);
    Ty        = t.GetTypeVariant();
}

Type::Type(Type &&ct)
{
    PointerLevel = ct.PointerLevel;
    Ty           = ct.Ty;
    Kind         = ct.Kind;
    Dimensions   = std::move(ct.Dimensions);
    TypeList     = std::move(ct.TypeList);
    ParamList    = std::move(ct.ParamList);
    Name         = ct.Name;
}

Type &Type::operator=(Type &&ct)
{
    PointerLevel = ct.PointerLevel;
    Ty           = ct.Ty;
    Kind         = ct.Kind;
    Dimensions   = std::move(ct.Dimensions);
    TypeList     = std::move(ct.TypeList);
    ParamList    = std::move(ct.ParamList);
    Name         = ct.Name;

    return *this;
}


std::vector<unsigned> &Type::GetDimensions()
{
    assert(IsArray() && "Must be an Array type to access Dimensions.");
    return Dimensions;
}

void Type::SetDimensions(std::vector<unsigned> D)
{
    Kind       = Type::Array;
    Dimensions = std::move(D);
}

std::vector<Type> &Type::GetArgTypes() { return ParamList; }

void Type::DecrementPointerLevel()
{
    assert(PointerLevel > 0 && "Cannot decrement below 0");
    PointerLevel--;
}

std::string Type::ToString(const Type &t)
{
    switch (t.GetTypeVariant())
    {
        case Double: return "double";
        case Int: return "int";
        case UnsignedInt: return "unsigned int";
        case Char: return "char";
        case UnsignedChar: return "unsigned char";
        case Void: return "void";
        case Composite: return t.GetName();
        case Invalid: return "invalid";

        default: assert(false && "Unknown type."); break;
    }
}

std::string Type::ToString() const
{
    if (IsFunction())
    {
        auto TyStr   = Type::ToString(*this);
        auto ArgSize = ParamList.size();

        if (ArgSize > 0)
            TyStr += " (";
        for (size_t i = 0; i < ArgSize; i++)
        {
            TyStr += Type::ToString(ParamList[i]);
            if (i + 1 < ArgSize)
                TyStr += ",";
            else
                TyStr += ")";
        }
        return TyStr;
    }
    else if (IsArray())
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

bool Type::IsIntegerType() const
{
    switch (Ty)
    {
        case Char:
        case UnsignedChar:
        case Int:
        case UnsignedInt: return true;

        default: return false;
    }
}

bool Type::IsImplicitlyCastable(const Type::VariantKind from, const Type::VariantKind to)
{
    return (from == Int && to == Double) || (from == Double && to == Int)
           || (from == Char && to == Int) || (from == Int && to == Char);
}

bool Type::IsSmallerThanInt(const Type::VariantKind V)
{
    switch (V)
    {
        case Char:
        case UnsignedChar: return true;

        default: return false;
    }
}

bool Type::OnlySignednessDifference(const Type::VariantKind V1,
                                    const Type::VariantKind V2)
{
    if ((V1 == Int && V2 == UnsignedInt) || (V1 == UnsignedInt && V2 == Int)
        || (V1 == Char && V2 == UnsignedChar) || (V1 == UnsignedChar && V2 == Char))
        return true;

    return false;
}

bool operator==(const Type &lhs, const Type &rhs)
{
    bool result = lhs.Kind == rhs.Kind && lhs.Ty == rhs.Ty;

    if (!result)
        return false;

    switch (lhs.Kind)
    {
        case Type::Array: result = result && (lhs.Dimensions == rhs.Dimensions); break;
        default: break;
    }
    return result;
}

int ValueType::GetIntVal()
{
    assert(IsInt() && "Must be an integer type.");
    return IntVal;
}

double ValueType::GetFloatVal()
{
    assert(IsFloat() && "Must be a float type.");
    return FloatVal;
}

bool operator==(const ValueType &lhs, const ValueType &rhs)
{
    bool result = (lhs.Kind == rhs.Kind);

    if (!result)
        return false;

    switch (lhs.Kind)
    {
        case ValueType::Integer: result = result && (lhs.IntVal == rhs.IntVal); break;
        case ValueType::Float: result = result && (lhs.FloatVal == rhs.FloatVal); break;
        default: break;
    }
    return result;
}
