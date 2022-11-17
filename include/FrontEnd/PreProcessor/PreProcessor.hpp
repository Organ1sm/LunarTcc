#pragma once

#include <vector>
#include <string>
#include <map>

class PreProcessor
{
  public:
    PreProcessor() = delete;
    explicit PreProcessor(std::vector<std::string> &Src, std::string Path);


    void ParseDirective(std::string &Line, std::size_t LineIdx);
    void SubstituteMacros(std::string &line);
    void Run();

  private:
    std::string FilePath;
    std::vector<std::string> &Source;
    std::map<std::string, std::pair<std::string, unsigned>> DefinedMacros;
};
