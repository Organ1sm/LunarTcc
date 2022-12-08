#pragma once

#include <vector>
#include <fmt/core.h>
#include <fmt/color.h>

class Token;

void PrintImpl(const char *str, unsigned tab = 0, bool newline = false);

void Print(const char *str, unsigned tab = 0);

void PrintLn(const char *str, unsigned tab = 0);

template <typename T, typename... Args>
void PrintError(const T &Msg, const Args &...args)
{
    fmt::print(stderr, fmt::emphasis::bold | fg(fmt::color::red), Msg, args...);
}


class DiagnosticPrinter
{
    enum class MessageType {
        Error,
        Warning,
        Note,
    };

  public:
    DiagnosticPrinter(std::string FileName, std::vector<std::string> Source)
        : FileName(std::move(FileName)), Source(std::move(Source))
    {}

    void AddMessage(const std::string &Msg);
    void AddMessage(const std::string &Msg, const MessageType MsgType);
    void AddMessage(const std::string &Msg, const MessageType MsgType, const Token &T);

    void AddError(const std::string &Msg);
    void AddError(const std::string &Msg, const Token &T);

    void AddWarning(const std::string &Msg);
    void AddWarning(const std::string &Msg, const Token &T);

    void AddNote(const std::string &Msg);

    void AddNote(const std::string &Msg, const Token &T);

    bool HasErrors(bool Wall = false) const;

    void ReportErrors() const;

    static std::string MessageTypeToString(MessageType MsgType);

  private:
    std::string FileName;
    const std::vector<std::string> Source;
    std::vector<std::string> ErrorMessages;

    bool HasError {false};
    bool HasWarning {false};
};
