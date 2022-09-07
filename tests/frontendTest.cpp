//
// Created by yw.
//
#include <iostream>
#include <fstream>
#include "frontend/Lexer/Lexer.hpp"
#include "frontend/Parser/Parser.hpp"


bool getFileContent(const std::string& fileName, std::vector<std::string> &VecOfStrs)
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
    std::string FilePath = "./test.txt";

    bool DumpTokens = true;
    bool DumpAst    = true;

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


    Parser parser(src);
    auto AST = parser.Parse();
    if (DumpAst)
        AST->ASTDump();

    return 0;
}
