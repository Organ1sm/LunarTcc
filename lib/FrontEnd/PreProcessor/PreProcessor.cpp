#include "FrontEnd/PreProcessor/PreProcessor.hpp"
#include "BackEnd/Support.hpp"
#include "FrontEnd/PreProcessor/PPLexer.hpp"
#include "FrontEnd/PreProcessor/PPToken.hpp"
#include "fmt/core.h"
#include <cassert>
#include <cctype>
#include <cstddef>
#include <string>
#include <filesystem>

PreProcessor::PreProcessor(std::vector<std::string> &Src, const std::string &Path)
    : Source(Src)
{
    FilePath = Path.substr(0, Path.rfind('/'));

    if (FilePath.length() > 0 && FilePath[FilePath.length() - 1] != '/')
        FilePath.push_back('/');

    DefinedMacros["__LINE__"] = {"1", 0};
    DefinedMacros["__FILE__"] = {"\"" + Path + "\"", 0};
}

void PreProcessor::ParseDirective(std::string &Line, std::size_t LineIdx)
{
    PPLexer lexer(Line);
    lexer.Lex();    // eat '#'

    auto Directive = lexer.Lex();
    assert(Directive.IsKeyword() && "Must be a keyword at this point");

    if (Directive.GetKind() == PPToken::Define)
    {
        auto DefinedID = lexer.Lex();
        assert(DefinedID.GetKind() != PPToken::EndOfFile &&
               DefinedID.GetKind() == PPToken::Identifier);

        // must be called here otherwise the token lookaheads mess up the lineindex
        // TODO: solve this problem, maybe with giving tokens the line number
        auto RemainingText = lexer.GetRemainingText();

        if (lexer.Is(PPToken::EndOfFile))
            DefinedMacros[DefinedID.GetString()] = {"", 0};
        else if (lexer.Is(PPToken::LeftParen))
        {
            lexer.Lex();    // eat'('
            std::vector<std::string> Params;

            do
            {
                auto Param = lexer.Lex();
                assert(Param.GetKind() == PPToken::Identifier);
                Params.push_back(Param.GetString());

                if (lexer.IsNot(PPToken ::Colon))
                    break;

                lexer.Lex();    // eat ','
            }
            while (true);

            assert(lexer.IsNot(PPToken::LeftParen));
            lexer.Lex();    // eat ')'

            auto Body = lexer.GetRemainingText();

            // replacing the parameters with their index eg.: with the below macro
            //    #define MAX(A,B) (((A) > (B)) ? (A) : (B))
            // the Body is "(((A) > (B)) ? (A) : (B))"
            // and it became "((($0) > ($1)) ? ($0) : ($1))"
            // this will make the substitution easier later
            for (std::size_t i = 0; i < Params.size(); i++)
            {
                while (Body.find(Params[i]) != std::string::npos)
                    Body.replace(Body.find(Params[i]),
                                 Params[i].length(),
                                 "$" + std::to_string(i));
            }

            DefinedMacros[DefinedID.GetString()] = {Body, Params.size()};
        }
        // plain define (eg.: #define TRUE 1)
        else
        {
            DefinedMacros[DefinedID.GetString()] = {RemainingText, 0};
        }
    }
    else if (Directive.GetKind() == PPToken::Include)
    {
        assert(lexer.Is(PPToken::DoubleQuote) || lexer.Is(PPToken::LessThan));
        bool IsSysHeaderFile = lexer.Is(PPToken::LessThan);
        lexer.Lex();    // eat the token.

        std::string FileName;
        auto NextToken     = lexer.Lex();
        auto NextTokenKind = NextToken.GetKind();
        while (NextTokenKind == PPToken::Identifier || NextTokenKind == PPToken::Dot ||
               NextTokenKind == PPToken::ForwardSlash)
        {
            FileName.append(NextToken.GetString());
            NextToken     = lexer.Lex();
            NextTokenKind = NextToken.GetKind();
        }

        assert((NextTokenKind == PPToken::DoubleQuote && !IsSysHeaderFile) ||
               (NextTokenKind == PPToken::GreaterThan && IsSysHeaderFile));

        if (IsSysHeaderFile)
        {
            auto Path = std::filesystem::current_path();
            Path.remove_filename();

            FilePath = Path;
            FilePath += "includes/";
        }

        std::vector<std::string> FileContent;
        auto Success = Filer::getFileContent(FilePath + FileName, FileContent);
        assert(Success && "Cannot open file.");

        Source.insert(Source.begin() + LineIdx + 1,
                      FileContent.begin(),
                      FileContent.end());
    }
    else if (Directive.GetKind() == PPToken::IfNotDefine)
    {
        // TODO
    }
    else if (Directive.GetKind() == PPToken::EndIf)
    {
        // TODO
    }
}

void PreProcessor::SubstituteMacros(std::string &Line)
{
    for (auto &[MacroID, MacroData] : DefinedMacros)
    {
        auto &[MacroBody, MacroParam] = MacroData;

        // simple search and replace of plain macros
        if (MacroParam == 0)
        {
            while (Line.find(MacroID) != std::string::npos)
                Line.replace(Line.find(MacroID), MacroID.length(), MacroBody);
        }
        else
        {
            auto Pos = Line.find(MacroID);
            if (Pos == std::string::npos)
                continue;

            Pos += MacroID.length();
            assert(Line[Pos] == '(');

            Pos++;

            auto RemainingLine = Line.substr(Pos);
            size_t StartPos    = 0;
            std::vector<std::string> ActualParams;

            for (std::size_t i = 0; i < MacroParam; i++)
            {
                std::size_t EndPos = i != MacroParam - 1 ? RemainingLine.find(',') :
                                                           RemainingLine.find(')');

                ActualParams.push_back(RemainingLine.substr(StartPos, EndPos - StartPos));
                StartPos = EndPos + 1;
            }

            // replace actual param by $N
            auto ReplacedMacroBody = MacroBody;
            for (std::size_t i = 0; i < ActualParams.size(); i++)
            {
                auto Param = "$" + std::to_string(i);
                while (ReplacedMacroBody.find(Param) != std::string::npos)
                {
                    ReplacedMacroBody.replace(ReplacedMacroBody.find(Param),
                                              Param.length(),
                                              ActualParams[i]);
                }
            }

            if (Line.find(MacroID) != std::string::npos)
                Line.replace(Line.find(MacroID),
                             MacroID.length() + StartPos + 1,
                             ReplacedMacroBody);
        }
    }
}

void PreProcessor::Run()
{
    for (std::size_t LineIndex = 0; LineIndex < Source.size(); LineIndex++)
    {
        auto &Line = Source[LineIndex];
        if (Line.empty())
            continue;

        if (Line[0] == '#')
        {
            ParseDirective(Line, LineIndex);

            /// delete current line, assuming the directive only used one line
            Source.erase(Source.begin() + LineIndex);
            LineIndex--;
        }
        else if (!DefinedMacros.empty())
        {
            DefinedMacros["__LINE__"].first = std::to_string(LineIndex + 1);
            SubstituteMacros(Line);
        }
    }
}
