//
// Created by Organ1sm.
//

#pragma once

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
    void SetType(IRType t) { ValueType = t; }

    unsigned GetID() const { return UniqueId; }
    void SetId(unsigned i) { UniqueId = i; }

    unsigned GetBitWidth() const { return ValueType.GetBitSize(); }

    bool IsConstant() const { return Kind == Const; }
    bool IsRegister() const { return Kind == Register; }
    bool IsParameter() const { return Kind == Param; }
    bool IsGlobalVar() const { return Kind == GlobalVar; }

    bool IsIntType() const { return ValueType.IsInt(); }
    bool IsFPType() const { return ValueType.IsFP(); }

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
    Constant(uint64_t V, uint8_t BW = 32)
        : Value(Value::Const, IRType(IRType::UInt, BW)), Val(V)
    {}

    Constant(double V, uint8_t BW = 32)
        : Value(Value::Const, IRType(IRType::FP, BW)), Val(V)
    {}

    uint64_t GetIntValue() const;
    double GetFloatValue() const;

    std::string ValueString() const override;

  private:
    std::variant<uint64_t, double> Val;
};

class FunctionParameter : public Value
{
  public:
    FunctionParameter() = delete;
    FunctionParameter(std::string &Name, IRType T, bool IsStruct = false)
        : Value(Value::Param, T), Name(Name), ImplicitStructPtr(IsStruct)
    {}

    std::string &GetName() { return Name; }
    bool IsImplicitStructPtr() const { return ImplicitStructPtr; }

    std::string ValueString() const override { return "$" + Name; }

  private:
    std::string Name;
    bool ImplicitStructPtr {false};
};

class GlobalVariable : public Value
{
  public:
    GlobalVariable() = delete;
    GlobalVariable(std::string Name, IRType T) : Value(Value::GlobalVar, T), Name(Name) {}
    GlobalVariable(std::string Name, IRType T, std::vector<uint64_t> InitList)
        : Value(Value::GlobalVar, T), Name(Name), InitList(std::move(InitList))
    {}

    GlobalVariable(std::string &Name, IRType T, Value *InitValue)
        : Value(Value::GlobalVar, T), Name(Name), InitValue(InitValue)
    {}

    GlobalVariable(std::string &Name, IRType T, std::string InitStr)
        : Value(Value::GlobalVar, T), Name(Name), InitString(InitStr)
    {}

    void SetName(std::string N) { Name = N; }
    std::string &GetName() { return Name; }

    std::string &GetInitString() { return InitString; }
    Value *GetInitValue() { return InitValue; }

    std::vector<uint64_t> &GetInitList() { return InitList; }


    std::string ValueString() const override;
    void Print() const;

  private:
    std::string Name;
    std::vector<uint64_t> InitList;
    std::string InitString;
    Value *InitValue {nullptr};
};
