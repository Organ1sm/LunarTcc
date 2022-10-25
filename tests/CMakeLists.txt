cmake_minimum_required(VERSION 3.19)

add_executable(
  LunarTcc
  frontendTest.cpp
  ../lib/FrontEnd/Lexer/Lexer.cpp
  ../lib/FrontEnd/Lexer/Token.cpp
  ../lib/FrontEnd/Parser/Parser.cpp
  ../lib/FrontEnd/Parser/SymbolTable.cpp
  ../lib/FrontEnd/AST/AST.cpp
  ../lib/FrontEnd/AST/Type.cpp
  ../lib/Utils/ErrorLogger.cpp
  ../lib/MiddleEnd/IR/IRType.cpp
  ../lib/MiddleEnd/IR/Module.cpp
  ../lib/MiddleEnd/IR/Function.cpp
  ../lib/MiddleEnd/IR/BasicBlock.cpp
  ../lib/MiddleEnd/IR/Value.cpp
  ../lib/MiddleEnd/IR/Instruction.cpp
  ../lib/MiddleEnd/IR/IRFactory.cpp
  ../lib/BackEnd/AssemblyEmitter.cpp
  ../lib/BackEnd/Support.cpp
  ../lib/BackEnd/IRtoLLIR.cpp
  ../lib/BackEnd/InstructionSelection.cpp
  ../lib/BackEnd/MachineBasicBlock.cpp
  ../lib/BackEnd/MachineOperand.cpp
  ../lib/BackEnd/MachineFunction.cpp
  ../lib/BackEnd/MachineInstruction.cpp
  ../lib/BackEnd/MachineInstructionLegalizer.cpp
  ../lib/BackEnd/PrologueEpilogInsertion.cpp
  ../lib/BackEnd/RegisterAllocator.cpp
  ../lib/BackEnd/StackFrame.cpp
  ../lib/BackEnd/LowLevelType.cpp
  ../lib/BackEnd/TargetMachine.cpp
  ../lib/BackEnd/GlobalData.cpp
  ../lib/BackEnd/TargetInstructionLegalizer.cpp
  ../lib/BackEnd/TargetArchs/AArch64/AArch64TargetABI.cpp
  ../lib/BackEnd/TargetArchs/AArch64/AArch64TargetMachine.cpp
  ../lib/BackEnd/TargetArchs/AArch64/AArch64RegisterInfo.cpp
  ../lib/BackEnd/TargetArchs/AArch64/AArch64InstructionLegalizer.cpp
  ../lib/BackEnd/TargetArchs/AArch64/AArch64InstructionDefinitions.cpp
  ../lib/BackEnd/TargetArchs/RISCV/RISCVRegisterInfo.cpp
  ../lib/BackEnd/TargetArchs/RISCV/RISCVInstructionDefinitions.cpp
  ../lib/BackEnd/TargetArchs/RISCV/RISCVTargetABI.cpp
  ../lib/BackEnd/TargetArchs/RISCV/RISCVTargetMachine.cpp
)

target_link_libraries(LunarTcc PRIVATE fmt::fmt)
