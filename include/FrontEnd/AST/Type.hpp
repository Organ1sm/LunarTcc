#pragma once

#include <cstdint>
#include <vector>
#include <string>
#include <cassert>

/// TODO: The types here should be subdivided into categories as before,
/// type class is too bloated.
class Type
{
  public:
    // ordered -> conversion rank
    enum VariantKind {
        Invalid,
        Composite,
        Void,
        Char,
        UnsignedChar,
        Short,
        UnsignedShort,
        Int,
        UnsignedInt,
        Long,
        UnsignedLong,
        LongLong,
        UnsignedLongLong,
        Float,
        Double,
    };

    enum TypeKind { Simple, Array, Struct };
    enum TypeQualifier : unsigned { None, TypeDef, Const };

    Type() : Kind(Simple), Ty(Invalid) {};
    Type(VariantKind vk) : Ty(vk), Kind(Simple) {}
    Type(TypeKind tk);

    Type(Type t, std::vector<unsigned> d);
    Type(Type t, std::vector<Type> a);

    Type(Type &&) = default;
    Type &operator=(Type &&);

    Type(const Type &)            = default;
    Type &operator=(const Type &) = default;

    std::string GetName() const { return Name; }
    void SetName(std::string &n) { Name = n; }

    TypeKind GetTypeKind() const { return Kind; }
    void SetTypeKind(TypeKind t) { Kind = t; }

    VariantKind GetTypeVariant() const { return Ty; }
    void SetTypeVariant(VariantKind t) { Ty = t; }

    std::vector<Type> &GetTypeList() { return TypeList; }
    VariantKind GetReturnType() const { return Ty; }

    uint8_t GetPointerLevel() const { return PointerLevel; }
    void SetPointerLevel(uint8_t pl) { PointerLevel = pl; }

    std::vector<Type> &GetParamList() { return ParamList; }
    std::vector<Type> &GetArgTypes();

    std::vector<unsigned> &GetDimensions();
    void SetDimensions(std::vector<unsigned> D);

    void IncrementPointerLevel() { PointerLevel++; }
    void DecrementPointerLevel();

    unsigned GetQualifiers() const { return Qualifiers; }
    void SetQualifiers(unsigned q) { Qualifiers = q; }
    void AddQualifiers(unsigned q) { Qualifiers |= q; }

    bool IsPointerType() const { return PointerLevel != 0; }
    bool IsSimpleType() const { return Kind == Simple; }
    bool IsArray() const { return Kind == Array; }
    bool IsFunction() const { return !ParamList.empty(); }
    bool IsStruct() const { return Kind == Struct; }

    bool IsConst() const { return Qualifiers & Const; }
    bool IsTypeDef() const { return Qualifiers & TypeDef; }

    bool IsFloatingPoint() const { return Ty == Double || Ty == Float; }
    bool IsIntegerType() const;
    bool IsUnsigned() const;

    bool HasVarArg() const { return VarArg; }
    void SetVarArg(bool p) { VarArg = p; }

    friend bool operator==(const Type &lhs, const Type &rhs);
    friend bool operator!=(const Type &lhs, const Type &rhs);

    std::string ToString() const;
    static std::string ToString(const Type &);

    static Type GetStrongestType(const VariantKind T1, const VariantKind T2)
    {
        return std::max(T1, T2);
    }

    static bool IsImplicitlyCastable(const Type from, const Type to);
    static bool IsImplicitlyCastable(const VariantKind from, const VariantKind to);
    static bool IsSmallerThanInt(const Type::VariantKind V);
    static bool OnlySignednessDifference(const Type::VariantKind V1,
                                         const Type::VariantKind V2);

  protected:
    std::string Name;    // For Structs
    VariantKind Ty;
    uint8_t PointerLevel {0};

    TypeKind Kind;
    unsigned Qualifiers {None};
    std::vector<Type> TypeList;
    std::vector<Type> ParamList;
    std::vector<unsigned> Dimensions;

    /// To indicate whether the function type has variable arguments or not
    bool VarArg = false;
};

// Hold an integer or a float value
class ValueType
{
  public:
    ValueType() : Kind(Empty) {}
    ValueType(unsigned v) : Kind(Integer), IntVal(v) {}
    ValueType(double v) : Kind(Float), FloatVal(v) {}

    ValueType(ValueType &&)            = default;
    ValueType &operator=(ValueType &&) = delete;

    ValueType(const ValueType &)            = default;
    ValueType &operator=(const ValueType &) = delete;

    bool IsInt() { return Kind == Integer; }
    bool IsFloat() { return Kind == Float; }
    bool IsEmpty() { return Kind == Empty; }

    int GetIntVal();
    double GetFloatVal();

    friend bool operator==(const ValueType &lhs, const ValueType &rhs);

  private:
    enum ValueKind { Empty, Integer, Float };
    ValueKind Kind;
    union
    {
        unsigned IntVal;
        double FloatVal;
    };
};
