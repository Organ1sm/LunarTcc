//
// Created by yw.
//
#include <iostream>
#include <fstream>
#include "FrontEnd/Lexer/Lexer.hpp"
#include "FrontEnd/Parser/Parser.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "MiddleEnd/IR/Module.hpp"


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

    bool DumpTokens = false;
    bool DumpAst    = false;
    bool DumpIR     = false;

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
            if (!option.compare("dump-ast"))
            {
                DumpAst = true;
                continue;
            }
            if (!option.compare("dump-ir"))
            {
                DumpIR = true;
                continue;
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
    if (DumpAst)
        AST->ASTDump();

    if (DumpIR)
    {
        AST->IRCodegen(&IRF);
        IRModule.Print();
    }

    return 0;
}
