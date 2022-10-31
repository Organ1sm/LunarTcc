//
// Created by yw.
//
#include <iostream>
#include <fstream>
#include <memory>
#include <fmt/core.h>
#include <fmt/color.h>
#include "Utils/ErrorLogger.hpp"
#include "BackEnd/MachineIRModule.hpp"
#include "FrontEnd/Lexer/Lexer.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "FrontEnd/Parser/Parser.hpp"
#include "FrontEnd/PreProcessor/PreProcessor.hpp"
#include "MiddleEnd/IR/Function.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/Module.hpp"
#include "BackEnd/AssemblyEmitter.hpp"
#include "BackEnd/IRtoLLIR.hpp"
#include "BackEnd/TargetMachine.hpp"
#include "BackEnd/InstructionSelection.hpp"
#include "BackEnd/MachineInstructionLegalizer.hpp"
#include "BackEnd/PrologueEpilogInsertion.hpp"
#include "BackEnd/RegisterAllocator.hpp"
#include "BackEnd/TargetArchs/AArch64/AArch64TargetMachine.hpp"
#include "BackEnd/TargetArchs/RISCV/RISCVTargetMachine.hpp"
#include "fmt/format.h"


bool getFileContent(const std::string &fileName, std::vector<std::string> &VecOfStrs)
{
    std::ifstream in(fileName.c_str());

    if (!in)
    {
        PrintError("Cannot open the file: '{}'\n", fileName);
        return false;
    }

    std::string str;

    while (std::getline(in, str))
    {
        VecOfStrs.push_back(std::move(str));
    }

    in.close();
    return true;
}

int main(int argc, char *argv[])
{
    std::string FilePath = "../tests/test.c";

    bool DumpTokens           = false;
    bool DumpAst              = false;
    bool DumpIR               = false;
    bool DumpPreProcessedFile = false;
    std::string TargetArch    = "aarch64";
    bool PrintBeforePasses    = false;

    for (auto i = 1; i < argc; ++i)
    {
        if (argv[i][0] != '-')
        {
            FilePath = std::string(argv[i]);
        }
        else
        {
            std::string option {&argv[i][1]};
            if (!option.compare("dump-tokens"))
            {
                DumpTokens = true;
                continue;
            }
            else if (!option.compare("dump-ast"))
            {
                DumpAst = true;
                continue;
            }
            else if (!option.compare("dump-ir"))
            {
                DumpIR = true;
                continue;
            }
            else if (!option.compare(0, 5, "arch="))
            {
                TargetArch = std::string(&argv[i][6]);
                continue;
            }
            else if (!option.compare("print-before-passes"))
            {
                PrintBeforePasses = true;
                continue;
            }
            else if (!option.compare("E"))
            {
                DumpPreProcessedFile = true;
                continue;
            }
            else if (!option.compare("debug"))
            {
                DumpTokens        = true;
                DumpAst           = true;
                DumpIR            = true;
                PrintBeforePasses = true;
            }
            else
            {
                PrintError("Error: Unknown argument `{}`\n", argv[i]);
                return -1;
            }
        }
    }


    std::vector<std::string> src;

    if (DumpTokens)
    {
        getFileContent(FilePath, src);
        Lexer lexer(src);
        auto t = lexer.Lex();

        while (t.GetKind() != Token::EndOfFile && t.GetKind() != Token::Invalid)
        {
            fmt::print("{}\n", t.ToString());
            t = lexer.Lex();
        }
    }

    getFileContent(FilePath, src);

    PreProcessor(src).Run();

    if (DumpPreProcessedFile)
    {
        for (auto &Line : src)
            std::cout << Line << std::endl;
        std::cout << std::endl;
    }

    Module IRModule;
    IRFactory IRF(IRModule);
    Parser parser(src, &IRF);

    auto AST = parser.Parse();

    if (DumpAst)
        AST->ASTDump();

    AST->IRCodegen(&IRF);

    if (DumpIR)
        IRModule.Print();

    std::unique_ptr<TargetMachine> TM;

    if (TargetArch == "riscv")
        TM = std::make_unique<RISCV::RISCVTargetMachine>();
    else
        TM = std::make_unique<AArch64::AArch64TargetMachine>();

    MachineIRModule LLIRModule;
    IRtoLLIR I2LLIR(IRModule, &LLIRModule, TM.get());
    I2LLIR.GenerateLLIRFromIR();

    if (PrintBeforePasses)
    {
        fmt::print(FMT_STRING("{:*^60}\n\n"), " Before Legalizer ");
        LLIRModule.Print(TM.get());
        fmt::print("\n");
    }

    MachineInstructionLegalizer Legalizer(&LLIRModule, TM.get());
    Legalizer.Run();

    if (PrintBeforePasses)
    {
        fmt::print(FMT_STRING("{:*^60}\n\n"), " Before Instruction Selection ");
        LLIRModule.Print(TM.get());
        fmt::print("\n");
    }

    InstructionSelection IS(&LLIRModule, TM.get());
    IS.InstrSelect();

    if (PrintBeforePasses)
    {
        fmt::print(FMT_STRING("{:*^60}\n\n"), " Before Register Allocate ");
        LLIRModule.Print(TM.get());
        fmt::print("\n");
    }

    RegisterAllocator RA(&LLIRModule, TM.get());
    RA.RunRA();

    if (PrintBeforePasses)
    {
        fmt::print(FMT_STRING("{:*^60}\n\n"), " Before Prolgue/Epilog Insertion ");
        LLIRModule.Print(TM.get());
        fmt::print("\n");
    }

    PrologueEpilogInsertion PEI(&LLIRModule, TM.get());
    PEI.Run();

    if (PrintBeforePasses)
    {
        fmt::print(FMT_STRING("{:*^60}\n\n"), " Before Emitting Assembly ");
        LLIRModule.Print(TM.get());
        fmt::print("\n");
    }

    AssemblyEmitter AE(&LLIRModule, TM.get());
    AE.GenerateAssembly();

    return 0;
}
