#include <regex>

const std::regex RE_DATE("-");

namespace rapidcsv
{
  template<>
  void Converter<long>::ToVal(const std::string& pStr, long& pVal) const
  {
    pVal = std::stol(std::regex_replace(pStr, RE_DATE, ""));
  }
}
