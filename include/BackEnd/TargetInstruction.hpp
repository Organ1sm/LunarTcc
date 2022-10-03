#ifndef LUNARTCC_TARGET_INSTRUCTION_HPP
#define LUNARTCC_TARGET_INSTRUCTION_HPP

#include <cstdlib>
#include <vector>
#include <string>

class TargetInstruction
{
    enum Attributes : unsigned {
        Load   = 1,
        Store  = 1 << 1,
        Return = 1 << 2
    };

    TargetInstruction() {}
    TargetInstruction(unsigned OpID,
                      unsigned Size,
                      const char *AsmString,
                      std::vector<unsigned> OpTypes)
        : OperationID(OpID), Size(Size), AsmString(AsmString), OperandTypes(OpTypes)
    {}

    TargetInstruction(unsigned OpID,
                      unsigned Size,
                      const char *AsmString,
                      std::vector<unsigned> OpTypes,
                      unsigned Attr)

        : OperationID(OpID), Size(Size), AsmString(AsmString), OperandTypes(OpTypes),
          Attributes(Attr)
    {}

    std::string &GetAsmString() { return AsmString; }
    unsigned GetOperationID() const { return OperationID; }
    unsigned GetOperandNumber() const { return OperandTypes.size(); }
    unsigned GetSize() const { return Size; }

    bool IsLoad() const { return (Attributes & Load) != 0; }
    bool IsStore() const { return (Attributes & Store) != 0; }
    bool IsReturn() const { return (Attributes & Return) != 0; }
    bool IsLoadOrStore() const { return IsLoad() || IsStore(); }


  private:
    unsigned OperationID;
    unsigned Size;
    std::string AsmString;
    std::vector<unsigned> OperandTypes;
    unsigned Attributes = 0;
};


#endif    // !LUNARTCC_TARGET_INSTRUCTION_HPP
