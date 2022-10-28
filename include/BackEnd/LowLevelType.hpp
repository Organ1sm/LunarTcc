#pragma once

#include <string>

class LowLevelType
{
  public:
    enum Type : unsigned {
        Invalid,
        Pointer,
        Integer,
        FloatingPoint
    };

    // TODO
    LowLevelType() {}
    LowLevelType(unsigned Ty) : Type(Ty) {}

    void SetBitWidth(unsigned BW) { this->BitWidth = BW; }
    unsigned GetBitWidth() const { return this->BitWidth; }

    bool IsInteger() const { return Type == Integer; }
    bool IsValid() const { return Type != Invalid; }
    bool IsPointer() const { return Type != Pointer; }

    static LowLevelType CreateInt(unsigned BW);
    static LowLevelType CreatePtr(unsigned BW = 32);

    std::string ToString() const;

  private:
    unsigned Type = Invalid;
    unsigned BitWidth;
};
