#pragma once

class LowLevelType
{
  public:
    enum Type : unsigned {
        Invalid,
        Pointer,
        Integer,
        FloatingPoint
    };

    LowLevelType() {}
    LowLevelType(unsigned Ty) : Type(Ty) {}

    void SetBitWidth(unsigned BW) { this->BitWidth = BW; }
    unsigned GetBitWidth() { return this->BitWidth; }

    bool IsInteger() const { return Type == Integer; }

    static LowLevelType CreateInt(unsigned BW)
    {
        LowLevelType LLT(Integer);
        LLT.SetBitWidth(BW);

        return LLT;
    }

  private:
    unsigned Type = Invalid;
    unsigned BitWidth;
};
