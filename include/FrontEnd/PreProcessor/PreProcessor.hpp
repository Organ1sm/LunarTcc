#pragma once

#include <vector>
#include <string>
#include <map>

class PreProcessor
{
  public:
    PreProcessor() = delete;
    PreProcessor(std::vector<std::string> &Src, const std::string &Path);

    void ParseDirective(std::string &Line, std::size_t LineIdx);
    void SubstituteMacros(std::string &line);
    void Run();

  private:
    std::string FilePath;
    std::vector<std::string> &Source;

    /// Macro -> {"MacroBody", SubstituteParamSize}
    std::map<std::string, std::pair<std::string, unsigned>> DefinedMacros;
};
