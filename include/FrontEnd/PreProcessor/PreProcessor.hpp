#pragma once

#include <vector>
#include <string>
#include <map>

class PreProcessor
{
  public:
    PreProcessor() = delete;
    explicit PreProcessor(std::vector<std::string> &Src) : Source(Src) {}

    void ParseDirective(std::string &Line);
    void SubstituteMacros(std::string &line);
    void Run();

  private:
    std::vector<std::string> &Source;
    std::map<std::string, std::pair<std::string, unsigned>> DefinedMacros;

};
