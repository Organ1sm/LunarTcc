//
// Created by Organ1sm.
//

#ifndef LUNARTCC_IRTYPE_HPP
#define LUNARTCC_IRTYPE_HPP

#include <cstdint>
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
        Ptr      // Pointer
    };

    IRType() : Kind(Invalid), BitWidth(0) {}
    IRType(TKind kind) : Kind(kind), BitWidth(32) {}
    IRType(TKind kind, uint8_t bw) : Kind(kind), BitWidth(bw) {}

    IRType(TKind kind, uint8_t bw, unsigned num)
        : Kind(kind), BitWidth(bw), NumberOfElements(num)
    {}

    void SetKind(TKind K) { Kind = K; }
    void SetToPointerKind() { Kind = Ptr; }

    bool IsFP() const { return Kind == FP; }
    bool IsVoid() const { return Kind == None; }
    bool IsSInt() const { return Kind == SInt; }
    bool IsUInt() const { return Kind == UInt; }
    bool IsInvalid() const { return Kind == Invalid; }

    void SetNumberOfElements(unsigned N) { NumberOfElements = N; }
    std::size_t GetByteSize() const { return (BitWidth * NumberOfElements + 7) / 8; }

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
    unsigned NumberOfElements {1};
};

#endif    // LUNARTCC_IRTYPE_HPP
