cmake_minimum_required(VERSION 3.19)

add_executable(Test frontendTest.cpp
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
               ../lib/MiddleEnd/IR/IRFactory.cpp)


