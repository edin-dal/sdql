#include <regex>

namespace rapidcsv
{
  const std::regex RE_DATE("-");

  template<>
  void Converter<long>::ToVal(const std::string& pStr, long& pVal) const
  {
    pVal = std::stol(std::regex_replace(pStr, RE_DATE, ""));
  }
}

#include <sstream>

std::string print_date(long yyyymmdd) {
  auto yyyy = yyyymmdd / 10000;
  auto mmdd = yyyymmdd - yyyy * 10000;
  auto mm = mmdd / 100;
  auto dd = mmdd - mm * 100;
  std::string mm_str = (mm < 10) ? "0" + std::to_string(mm) : std::to_string(mm);
  std::string dd_str = (dd < 10) ? "0" + std::to_string(dd) : std::to_string(dd);
  std::stringstream ss;
  ss << yyyy << "-" << mm_str << "-" << dd_str;
  return ss.str();
}
