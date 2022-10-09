//
// Created by yw.
//
#include <iostream>
#include <fstream>
#include <memory>
#include "BackEnd/MachineIRModule.hpp"
#include "FrontEnd/Lexer/Lexer.hpp"
#include "FrontEnd/Parser/Parser.hpp"
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


bool getFileContent(const std::string &fileName, std::vector<std::string> &VecOfStrs)
{
    std::ifstream in(fileName.c_str());

    if (!in)
    {
        std::cerr << "Cannot open the file: " << fileName << std::endl;
        return false;
    }

    std::string str;

    while (std::getline(in, str))
    {
        VecOfStrs.push_back(str);
    }

    in.close();
    return true;
}

int main(int argc, char *argv[])
{
    std::string FilePath = "../tests/test.c";

    bool DumpTokens        = false;
    bool DumpAst           = false;
    bool DumpIR            = false;
    std::string TargetArch = "aarch64";

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
            else
            {
                std::cerr << "Error: Unknown argument '" << argv[i] << "'" << std::endl;
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
            std::cout << t.ToString() << std::endl;
            t = lexer.Lex();
        }
    }

    getFileContent(FilePath, src);

    Module IRModule;
    IRFactory IRF(IRModule);
    Parser parser(src, &IRF);

    auto AST = parser.Parse();
    AST->IRCodegen(&IRF);

    if (DumpAst)
        AST->ASTDump();

    if (DumpIR)
        IRModule.Print();

    MachineIRModule LLIRModule;
    IRtoLLIR I2LLIR(IRModule, &LLIRModule);
    I2LLIR.GenerateLLIRFromIR();

    std::unique_ptr<TargetMachine> TM;

    if (TargetArch == "riscv")
        TM = std::make_unique<RISCV::RISCVTargetMachine>();
    else
        TM = std::make_unique<AArch64::AArch64TargetMachine>();

    MachineInstructionLegalizer Legalizer(&LLIRModule, TM.get());
    Legalizer.Run();

    InstructionSelection IS(&LLIRModule, TM.get());
    IS.InstrSelect();

    RegisterAllocator RA(&LLIRModule, TM.get());
    RA.RunRA();

    PrologueEpilogInsertion PEI(&LLIRModule, TM.get());
    PEI.Run();

    AssemblyEmitter AE(&LLIRModule, TM.get());
    AE.GenerateAssembly();


    return 0;
}
