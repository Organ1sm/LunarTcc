#pragma once

#include <cstdint>
#include <string>
#include <vector>

class GlobalData
{
  public:
    enum Directives { None = -1, Zero, Byte, HalfWord, Word, DoubleWord };
    using InfoVec = std::vector<std::pair<Directives, int64_t>>;

  public:
    GlobalData() {}

    GlobalData(std::string Name, std::size_t Size) : Name(std::move(Name)), Size(Size) {}

    std::string &GetName() { return Name; }
    void SetName(std::string &N) { Name = N; }

    std::size_t GetSize() const { return Size; }
    void SetSize(std::size_t S) { Size = S; }

    InfoVec &GetInitValues() { return InitValues; }

    void InsertAllocation(std::size_t ByteSize, int64_t InitVal);

    static std::string DirectivesToString(Directives D);

    void Print() const;

  private:
    /// Name of the global object, with used to create its label;
    std::string Name;

    /// How much bytes needs to be allocated for it overall. With the below
    /// example it would be 4.
    std::size_t Size;


    /// If it was initialised, then this vector hold the information about
    /// what directive needs to be used and with what value. Example:
    ///   struct S { int16_t, int8_t }
    ///   void foo() { struct S s = { 6, 9 }; }
    ///
    /// Will create the following private data:
    ///   .L_foo().s:
    ///     .short 6
    ///     .byte 9
    ///     .zero 1
    ///
    /// Assuming ".L_foo().s" will be the name of the automatically generated
    /// initializer data. ".zero 1" allocate one byte set to zero. This is for
    /// padding in the example.
    InfoVec InitValues;
};
