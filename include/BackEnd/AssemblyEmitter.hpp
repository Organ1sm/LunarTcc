#include "MachineIRModule.hpp"
#include "TargetMachine.hpp"

class AssemblyEmitter
{
  public:
    AssemblyEmitter(MachineIRModule *Module, TargetMachine *TM) : TM(TM), MIRM(Module) {}

    void GenerateAssembly();

  private:
    TargetMachine *TM;
    MachineIRModule *MIRM;
};
