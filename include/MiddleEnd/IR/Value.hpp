//
// Created by Organ1sm.
//

#ifndef LUNARTCC_VALUE_HPP
#define LUNARTCC_VALUE_HPP

#include <cstdint>
#include <variant>
#include "MiddleEnd/IR/IRType.hpp"

class Value
{
  public:
    enum VKind { Invalid = 1, None, Register, Label, Const, Param, GlobalVar };

    Value() : Kind(Invalid) {}
    Value(VKind VK) : Kind(VK) {}
    Value(IRType T) : ValueType(T), Kind(Register) {}
    Value(VKind VK, IRType T) : ValueType(T), Kind(VK) {}

    virtual ~Value() {};
    IRType &GetTypeRef() { return ValueType; }
    IRType GetType() const { return ValueType; }

    unsigned GetID() const { return UniqueId; }
    void SetId(unsigned i) { UniqueId = i; }

    unsigned GetBitWidth() const { return ValueType.GetBitSize(); }

    bool IsConstant() const { return Kind == Const; }
    bool IsRegister() const { return Kind == Register; }
    bool IsParameter() const { return Kind == Param; }
    bool IsIntType() const { return ValueType.IsInt(); }
    bool IsGlobalVar() const { return Kind == GlobalVar; }

    virtual std::string ValueString() const;

  protected:
    unsigned UniqueId;
    VKind Kind {Register};
    IRType ValueType;
};

class Constant : public Value
{
  public:
    Constant() = delete;
    Constant(uint64_t V) : Value(Value::Const, IRType::UInt), Val(V) {}
    Constant(double V) : Value(Value::Const, IRType(IRType::FP, 64)), Val(V) {}

    bool IsFPConst() const { return ValueType.IsFP(); }
    uint64_t GetIntValue(); 

    std::string ValueString() const override;

  private:
    std::variant<uint64_t, double> Val;
};

class FunctionParameter : public Value
{
  public:
    FunctionParameter() = delete;
    FunctionParameter(std::string &Name, IRType T) : Value(Value::Param, T), Name(Name) {}

    std::string &GetName() { return Name; }

    std::string ValueString() const override { return "$" + Name; }

  private:
    std::string Name;
};

class GlobalVariable : public Value
{
  public:
    GlobalVariable() = delete;
    GlobalVariable(std::string Name, IRType T) : Value(Value::GlobalVar, T), Name(Name) {}
    GlobalVariable(std::string Name, IRType T, std::vector<uint64_t> InitList)
        : Value(Value::GlobalVar, T), Name(Name), InitList(std::move(InitList))
    {}

    void SetName(std::string N) { Name = N; }
    std::string &GetName() { return Name; }

    std::vector<uint64_t> &GetInitList() { return InitList; }


    std::string ValueString() const override;
    void Print() const;

  private:
    std::string Name;
    std::vector<uint64_t> InitList;
};

#endif    // LUNARTCC_VALUE_HPP
