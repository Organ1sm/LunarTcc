#pragma once

#include <string>

class LowLevelType
{
  public:
    enum Type : unsigned {
        Invalid,
        Pointer,
        Scalar,
    };

    // TODO
    LowLevelType() {}
    LowLevelType(unsigned Ty) : Type(Ty) {}

    void SetBitWidth(unsigned BW) { this->BitWidth = BW; }
    unsigned GetBitWidth() const { return this->BitWidth; }

    bool IsScalar() const { return Type == Scalar; }
    bool IsValid() const { return Type != Invalid; }
    bool IsPointer() const { return Type == Pointer; }

    static LowLevelType CreateScalar(unsigned BW);
    static LowLevelType CreatePtr(unsigned BW = 32);

    std::string ToString() const;

  private:
    unsigned Type = Invalid;
    unsigned BitWidth;
};
