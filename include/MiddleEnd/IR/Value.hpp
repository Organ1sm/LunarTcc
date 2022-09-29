//
// Created by Organ1sm.
//

#ifndef LUNARTCC_VALUE_HPP
#define LUNARTCC_VALUE_HPP


#include <variant>
#include "MiddleEnd/IR/IRType.hpp"

class Value
{
  public:
    enum VKind {
        Invalid = 1,
        None,
        Register,
        Label,
        Const,
        Param,
        GlobalVar
    };

    Value() : Kind(Invalid) {}
    Value(VKind VK) : Kind(VK) {}
    Value(IRType T) : ValueType(T), Kind(Register) {}
    Value(VKind VK, IRType T) : ValueType(T), Kind(VK) {}

    virtual ~Value() {};
    IRType &GetType() { return ValueType; }
    IRType GetType() const { return ValueType; }

    unsigned GetID() const { return UniqueId; }
    void SetId(unsigned i) { UniqueId = i; }

    bool IsConstant() { return Kind == Const; }

    virtual std::string ValueString() const { return "$" + std::to_string(UniqueId); }

  protected:
    unsigned UniqueId;
    VKind Kind;
    IRType ValueType;
};

class Constant : public Value
{
  public:
    Constant() = delete;
    Constant(uint64_t V) : Value(Value::Const, IRType::UInt), Val(V) {}
    Constant(double V) : Value(Value::Const, IRType(IRType::FP, 64)), Val(V) {}

    bool IsFPConst() const { return ValueType.IsFP(); }

    std::string ValueString() const override;

  private:
    std::variant<uint64_t, double> Val;
};

class FunctionParameter : public Value
{
  public:
    FunctionParameter() = delete;
    FunctionParameter(std::string &Name, IRType T) : Value(Value::Param, T) , Name(Name){}

    std::string ValueString() const override { return "$" + Name; }

  private:
    std::string Name;
};

class GlobalVariable : public Value
{
  public:
    GlobalVariable() = delete;
    GlobalVariable(std::string Name, IRType T) : Value(Value::GlobalVar, T), Name(Name) {}

    std::string ValueString() const override { return "@" + Name; }
    void Print() const;

  private:
    std::string Name;
};


#endif    // LUNARTCC_VALUE_HPP
