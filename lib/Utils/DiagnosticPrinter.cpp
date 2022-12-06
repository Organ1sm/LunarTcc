#include "Utils/DiagnosticPrinter.hpp"
#include "FrontEnd/Lexer/Token.hpp"
#include "fmt/color.h"
#include "fmt/core.h"
#include <cassert>
#include <iostream>

void PrintImpl(const char *str, unsigned int tab, bool newline)
{
    auto s = fmt::format("{:^{}}{}", "", tab, str);
    fmt::print(s);

    if (newline)
        fmt::print("\n");
}

void Print(const char *str, unsigned int tab) { PrintImpl(str, tab); }

void PrintLn(const char *str, unsigned int tab) { PrintImpl(str, tab, true); }


std::string CreateCodePointerString(const Token &T)
{
    std::size_t StartIndex = T.GetColumn();

    std::string Spaces(StartIndex, ' ');
    std::string Hats(T.GetString().length(), '^');

    return Spaces +
           fmt::format(
               "{}",
               fmt::styled(Hats, fmt::emphasis::bold | fmt::fg(fmt::color::green)));
}

void DiagnosticPrinter::AddMessage(const std::string &Msg)
{
    ErrorMessages.push_back(Msg);
}

void DiagnosticPrinter::AddMessage(const std::string &Msg, MessageType MsgType)
{
    ErrorMessages.push_back(MessageTypeToString(MsgType) + Msg);
}

void DiagnosticPrinter::AddMessage(const std::string &Msg,
                                   MessageType MsgType,
                                   const Token &T)
{
    assert(T.GetLine() < Source.size() && "Out of bound index");


    auto Type     = MessageTypeToString(MsgType);
    auto Location = fmt::format(":{}:{}: ", T.GetLine() + 1, T.GetColumn() + 1);
    auto WholeMsg = fmt::format("{}{}{}\n",
                                fmt::styled(Location, fmt::emphasis::bold),
                                fmt::styled(Type, fmt::emphasis::bold),
                                fmt::styled(Msg, fmt::emphasis::bold));

    std::string Message = fmt::format("{}{}\n{}\n",
                                      WholeMsg,
                                      Source[T.GetLine()],
                                      CreateCodePointerString(T));

    AddMessage(Message);
}

void DiagnosticPrinter::AddError(const std::string &Msg)
{
    HasError = true;
    AddMessage(Msg, MessageType::Error);
}

void DiagnosticPrinter::AddError(const std::string &Msg, const Token &T)
{
    HasError = true;
    AddMessage(Msg, MessageType::Error, T);
}

void DiagnosticPrinter::AddWarning(const std::string &Msg)
{
    HasWarning = true;
    AddMessage(Msg, MessageType::Warning);
}

void DiagnosticPrinter::AddWarning(const std::string &Msg, const Token &T)
{
    HasWarning = true;
    AddMessage(Msg, MessageType::Warning, T);
}

void DiagnosticPrinter::AddNote(const std::string &Msg)
{
    AddMessage(Msg, MessageType::Note);
}

void DiagnosticPrinter::AddNote(const std::string &Msg, const Token &T)
{
    AddMessage(Msg, MessageType::Note, T);
}

bool DiagnosticPrinter::HasErrors(bool Wall) const
{
    return Wall ? HasError || HasWarning : HasError;
}

void DiagnosticPrinter::ReportErrors() const
{
    for (auto &Msg : ErrorMessages)
    {
        fmt::print("{}{}\n", fmt::styled(FileName, fmt::emphasis::bold), Msg);
    }
}

std::string DiagnosticPrinter::MessageTypeToString(MessageType MsgType)
{
    switch (MsgType)
    {
        case MessageType::Error:
            return fmt::format("{}", fmt::styled("error: ", fmt::fg(fmt::color::red)));

        case MessageType::Warning:
            return fmt::format("{}",
                               fmt::styled("warning: ", fmt::fg(fmt::color::yellow)));

        case MessageType::Note:
            return fmt::format("{}", fmt::styled("note: ", fmt::fg(fmt::color::red)));
    }
}
