#ifndef LUNARTCC_TYPE_H
#define LUNARTCC_TYPE_H

#include <cstdint>
#include <vector>
#include <string>
#include <cassert>

class Type
{
  public:
    // ordered -> conversion rank
    enum VariantKind {
        Invalid,
        Void,
        Char,
        Int,
        Double,
    };

    Type() : Ty(Invalid) {};
    Type(VariantKind vk) : Ty(vk) {}

    Type(Type &&)            = default;
    Type &operator=(Type &&) = default;

    Type(const Type &)            = default;
    Type &operator=(const Type &) = default;

    virtual ~Type() = default;

    VariantKind GetTypeVariant() const { return Ty; }
    void SetTypeVariant(VariantKind t) { Ty = t; }

    uint8_t GetPointerLevel() const { return PointerLevel; }
    void SetPointerLevel(uint8_t pl) { PointerLevel = pl; }

    void IncrementPointerLevel() { PointerLevel++; }
    void DecrementPointerLevel()
    {
        assert(PointerLevel > 0 && "Cannot decrement below 0");
        PointerLevel--;
    }

    bool IsPointerTYpe() const { return PointerLevel != 0; }

    virtual std::string ToString() const { return ToString(Ty); }
    static std::string ToString(const VariantKind vk);

    static Type GetStrongestType(const VariantKind T1, const VariantKind T2)
    {
        return std::max(T1, T2);
    }

    static bool IsImplicitlyCastable(const VariantKind from, const VariantKind to);

  protected:
    VariantKind Ty;
    uint8_t PointerLevel {0};
};

class ArrayType : public Type
{
  public:
    ArrayType() = default;
    ArrayType(Type t) : Type(t) {}
    ArrayType(Type t, std::vector<unsigned> d) : Type(t), Dimensions {d} {}

    ArrayType(ArrayType &&)            = default;
    ArrayType &operator=(ArrayType &&) = default;

    ArrayType(const ArrayType &)            = default;
    ArrayType &operator=(const ArrayType &) = default;

    std::vector<unsigned> &GetDimensions() { return Dimensions; }
    void SetDimensions(std::vector<unsigned> d) { Dimensions = std::move(d); }

    bool IsArray() { return !Dimensions.empty(); }
    std::string ToString() const override;

  private:
    std::vector<unsigned> Dimensions;
};

class FunctionType : public Type
{
  public:
    FunctionType() = default;
    FunctionType(Type t) : Type(t) {}
    FunctionType(Type t, std::vector<VariantKind> a) : Type(t), ArgumentTypes(a) {}

    FunctionType(FunctionType &&)            = default;
    FunctionType &operator=(FunctionType &&) = delete;

    FunctionType(const FunctionType &)            = default;
    FunctionType &operator=(const FunctionType &) = default;

    std::vector<VariantKind> &GetArgumentTypes() { return ArgumentTypes; }
    void SetArgumentTypes(std::vector<VariantKind> &v) { ArgumentTypes = v; }
    void AddArgumentTypes(VariantKind vk) { ArgumentTypes.push_back(vk); }

    VariantKind GetReturnType() const { return GetTypeVariant(); }
    void SetReturnType(VariantKind vk) { SetTypeVariant(vk); }

    std::string ToString() const override;

  private:
    std::vector<VariantKind> ArgumentTypes;
};

class ComplexType : public Type
{
  public:
    ComplexType() : Kind(Simple), Type(Invalid) {}
    ComplexType(VariantKind vk) : Kind(Simple), Type(vk) {}
    ComplexType(Type t) : Kind(Simple), Type(t) {}

    ComplexType(ComplexType &&ct);

    ComplexType(const ComplexType &ct);
    ComplexType &operator=(const ComplexType &ct);

    ComplexType(Type t, std::vector<unsigned> d);
    ComplexType(Type t, std::vector<VariantKind> a);

    ComplexType(ArrayType t)
        : Type(t.GetTypeVariant()), Kind(Array), Dimensions(t.GetDimensions())
    {}

    ComplexType(FunctionType t)
        : Type(t.GetReturnType()), Kind(Function), ArgumentTypes(t.GetArgumentTypes())
    {}

    bool IsSimpleType() { return Kind == Simple; }
    bool IsArrayType() { return Kind == Array; }
    bool IsFunctionType() { return Kind == Function; }

    Type GetType() { return Type(Ty); }
    ArrayType GetArrayType() { return ArrayType(Ty, Dimensions); }
    FunctionType GetFunctionType() { return FunctionType(Ty, ArgumentTypes); }

    std::vector<unsigned> &GetDimensions()
    {
        assert(IsArrayType() && "Must be an Array type to access Dimensions.");
        return Dimensions;
    }

    std::vector<Type::VariantKind> &GetArgTypes()
    {
        assert(IsFunctionType() && "Must be a Function type to access ArgumentTypes.");
        return ArgumentTypes;
    }

    friend bool operator==(const ComplexType &lhs, const ComplexType &rhs);

    std::string ToString() const override;

  private:
    enum TypeKind {
        Simple,
        Array,
        Function
    };
    TypeKind Kind;
    std::vector<VariantKind> ArgumentTypes;
    std::vector<unsigned> Dimensions;
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

    unsigned GetIntVal()
    {
        assert(IsInt() && "Must be an integer type.");
        return IntVal;
    }
    double GetFloatVal()
    {
        assert(IsFloat() && "Must be a float type.");
        return FloatVal;
    }

    friend bool operator==(const ValueType &lhs, const ValueType &rhs);

  private:
    enum ValueKind {
        Empty,
        Integer,
        Float
    };
    ValueKind Kind;
    union
    {
        unsigned IntVal;
        double FloatVal;
    };
};

#endif
