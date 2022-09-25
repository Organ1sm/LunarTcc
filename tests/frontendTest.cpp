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

int main()
{
    std::string FilePath = "../tests/test.c";

    bool DumpTokens = true;
    bool DumpAst    = true;
    bool DumpIR     = true;

    std::vector<std::string> src;
    getFileContent(FilePath, src);

    if (DumpTokens)
    {
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
        AST->IRCodegen(&IRF), IRModule.Print();

    return 0;
}
