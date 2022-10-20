#ifndef LUNARTCC_TARGETREGISTER_HPP
#define LUNARTCC_TARGETREGISTER_HPP

#include <functional>
#include <string>
#include <set>
#include <vector>

class TargetRegister
{
  public:
    TargetRegister() {}

    void SetID(unsigned ID) { this->ID = ID; }
    unsigned GetID() const { return ID; }

    void SetBitWidth(unsigned BitWidth) { this->BitWidth = BitWidth; }
    unsigned GetBitWidth() const { return BitWidth; }

    void SetName(std::string &N) { this->Name = N; }
    void SetName(const char *N) { this->Name = std::string(N); }
    std::string &GetName() { return this->Name; }

    void SetAlias(const char *N) { AliasName = N; }
    std::string &GetAlias() { return AliasName; }

    void SetSubRegs(std::vector<unsigned> &N) { SubRegisters = N; }
    std::vector<unsigned> &GetSubRegs() { return SubRegisters; }

    static TargetRegister Create(unsigned ID,
                                 unsigned BitWidth,
                                 const char *Name,
                                 const char *Alias,
                                 std::vector<unsigned> SubRegs = {})
    {
        TargetRegister NewReg;
        NewReg.SetID(ID);
        NewReg.SetBitWidth(BitWidth);
        NewReg.SetName(Name);
        NewReg.SetAlias(Alias);
        NewReg.SetSubRegs(SubRegs);

        return NewReg;
    }

  private:
    unsigned ID       = 0;
    unsigned BitWidth = 0;
    std::string Name;
    std::string AliasName;
    std::vector<unsigned> SubRegisters;
};

class RegisterClass
{
  public:
    void AddRegister(unsigned ID) { Registers.insert(ID); }
    bool Contains(unsigned ID) { return Registers.count(ID) != 0; }

  private:
    std::set<unsigned, std::greater<unsigned>> Registers;
};


#endif    // !LUNARTCC_TARGETREGISTER_HPP
