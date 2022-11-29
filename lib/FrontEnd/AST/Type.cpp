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
        Kind = t.Kind;
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
    std::string ConstStr;
    std::string Result;

    if (t.IsConst())
        ConstStr = "const ";

    switch (t.GetTypeVariant())
    {
        case Float: Result = "float"; break;
        case Double: Result = "double"; break;
        case Long: Result = "long"; break;
        case UnsignedLong: Result = "unsigned long"; break;
        case LongLong: Result = "long long"; break;
        case UnsignedLongLong: Result = "unsigned long long"; break;
        case Int: Result = "int"; break;
        case UnsignedInt: Result = "unsigned int"; break;
        case Short: Result = "short"; break;
        case UnsignedShort: Result = "unsigned short"; break;
        case Char: Result = "char"; break;
        case UnsignedChar: Result = "unsigned char"; break;
        case Void: Result = "void"; break;
        case Composite: Result = t.GetName(); break;
        case Invalid: return "invalid";

        default: assert(!"Unknown type."); break;
    }

    return ConstStr + Result + std::string(t.GetPointerLevel(), '*');
}

// TODO: Simplify this.
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
                TyStr += ", ";
            else
            {
                if (VarArg)
                    TyStr += ", ...";

                TyStr += ")";
            }
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
        case Short:
        case UnsignedShort:
        case Int:
        case UnsignedInt:
        case Long:
        case UnsignedLong:
        case LongLong:
        case UnsignedLongLong: return true;

        default: return false;
    }
}

bool Type::IsUnsigned() const
{
    switch (Ty)
    {
        case UnsignedChar:
        case UnsignedShort:
        case UnsignedInt:
        case UnsignedLong:
        case UnsignedLongLong: return true;

        default: return false;
    }
}

bool Type::IsImplicitlyCastable(const Type::VariantKind from, const Type::VariantKind to)
{
    switch (to)
    {
        case Char:
        case UnsignedChar:
        case Short:
        case UnsignedShort:
        case Int:
        case UnsignedInt:
        case Long:
        case UnsignedLong:
        case LongLong:
        case UnsignedLongLong:
        case Double: return from >= Char;

        default: return false;
    }
}

bool Type::IsImplicitlyCastable(const Type from, const Type to)
{
    const auto IsFromPtr   = from.IsPointerType();
    const auto IsFromArray = from.IsArray();
    const auto IsToPtr     = to.IsPointerType();

    // array to pointer decay case
    if (IsFromArray && !IsFromPtr && IsToPtr)
    {
        if (from.GetTypeVariant() == to.GetTypeVariant())
            return true;

        return false;
    }

    return IsImplicitlyCastable(from.GetTypeVariant(), to.GetTypeVariant());
}

bool Type::IsSmallerThanInt(const Type::VariantKind V)
{
    switch (V)
    {
        case Short:
        case UnsignedShort:
        case Char:
        case UnsignedChar: return true;

        default: return false;
    }
}

bool Type::OnlySignednessDifference(const Type::VariantKind V1,
                                    const Type::VariantKind V2)
{
    if ((V1 == Int && V2 == UnsignedInt) || (V1 == UnsignedInt && V2 == Int) ||
        (V1 == Char && V2 == UnsignedChar) || (V1 == UnsignedChar && V2 == Char) ||
        (V1 == Short && V2 == UnsignedShort) || (V2 == Short && V1 == UnsignedShort) ||
        ((V1 == Long || V1 == LongLong) && (V2 == UnsignedLong || V2 == UnsignedLongLong))

        || ((V2 == Long || V2 == LongLong) &&
            (V1 == UnsignedLong || V1 == UnsignedLongLong)))

        return true;

    // special case, not really sign difference, rather just same size
    if ((V1 == Long && V2 == LongLong) || (V2 == Long && V1 == LongLong))
        return true;

    return false;
}

bool operator==(const Type &lhs, const Type &rhs)
{
    bool result = lhs.Kind == rhs.Kind && lhs.Ty == rhs.Ty;
    result      = result && lhs.GetPointerLevel() == rhs.GetPointerLevel();

    if (!result)
        return false;

    switch (lhs.Kind)
    {
        case Type::Array: result = result && (lhs.Dimensions == rhs.Dimensions); break;
        default: break;
    }
    return result;
}

bool operator!=(const Type &lhs, const Type &rhs) { return !(lhs == rhs); }

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
