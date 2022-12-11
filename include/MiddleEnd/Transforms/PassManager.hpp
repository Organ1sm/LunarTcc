#pragma once

#include <set>
class Module;

enum class Optimization {
    None,
};

class PassManager
{
  public:
    explicit PassManager(Module *M, std::set<Optimization> &Opts)
        : IRModule(M), Optimizations(Opts)
    {}

    bool RunAll();

  private:
    Module *IRModule;
    std::set<Optimization> &Optimizations;
};
