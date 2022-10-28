//
// Created by Organ1sm.
//
#pragma once

#include <cstdint>
#include <vector>
#include <string>
#include <cassert>

class IRType
{
  public:
    enum TKind : uint8_t {
        Invalid,
        None,    // Void
        FP,      // Float
        UInt,    // Unsigned Int
        SInt,    // Signed Int
        Ptr,     // Pointer
        Struct
    };

    IRType() : Kind(Invalid), BitWidth(0) {}
    IRType(TKind kind) : Kind(kind), BitWidth(32) {}
    IRType(TKind kind, uint8_t bw) : Kind(kind), BitWidth(bw) {}

    void SetKind(TKind K) { Kind = K; }
    void SetToPointerKind() { Kind = Ptr; }

    uint8_t GetPointerLevel() const { return PointerLevel; }
    void SetPointerLevel(uint8_t pl);
    void IncrementPointerLevel() { PointerLevel++; }
    void DecrementPointerLevel();

    void ReduceDimension();

    void SetDimensions(const std::vector<unsigned> &N) { Dimensions = N; }
    std::vector<unsigned> &GetDimensions() { return Dimensions; }

    void SetStructName(std::string &N) { StructName = N; }
    const std::string &GetStructName() const { return StructName; }

    std::vector<IRType> &GetMemberTypes() { return MembersTypeList; }

    bool IsFP() const { return Kind == FP; }
    bool IsVoid() const { return Kind == None; }
    bool IsSInt() const { return Kind == SInt; }
    bool IsUInt() const { return Kind == UInt; }
    bool IsInvalid() const { return Kind == Invalid; }
    bool IsInt() const { return IsSInt() || IsUInt(); }
    bool IsPointer() const { return PointerLevel > 0; }
    bool IsStruct() const { return Kind == Struct; }
    bool IsArray() const { return !Dimensions.empty(); }

    std::size_t GetBitSize() const { return BitWidth; }
    std::size_t GetByteSize() const;

    unsigned CalcElemSize(unsigned dim);
    unsigned GetElemByteOffset(const unsigned StructElemIndex) const;

    IRType GetBaseType() const { return IRType(Kind, BitWidth); }

    std::string AsString() const;

    bool operator==(const IRType &RHS)
    {
        return BitWidth == RHS.BitWidth && Kind == RHS.Kind;
    }

    static IRType CreateBool() { return IRType(SInt, 1); }
    static IRType CreateInt(uint8_t BW = 32) { return IRType(SInt, BW); }
    static IRType CreateFloat(uint8_t BW = 32) { return IRType(FP, BW); }

  private:
    TKind Kind;
    uint8_t BitWidth;
    uint8_t PointerLevel {0};
    std::string StructName;
    std::vector<IRType> MembersTypeList;
    std::vector<unsigned> Dimensions;
};
