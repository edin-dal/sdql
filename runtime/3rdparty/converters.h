#include <regex>

namespace rapidcsv
{
  const std::regex RE_DATE("-");

  template<>
  void Converter<long>::ToVal(const std::string& pStr, long& pVal) const
  {
    pVal = std::stol(std::regex_replace(pStr, RE_DATE, ""));
  }

  // TODO rewrite as converter - if I get C++'s template notation right
  // template<unsigned maxLen>
  // void Converter<VarChar<maxLen>>::ToVal(const std::string& pStr, int& pVal)
  // {
  //     pVal = string_to_varchar<maxLen>(str);
  // }
}

template<unsigned maxLen>
VarChar<maxLen> string_to_varchar(std::string str) {
    return VarChar<maxLen>(std::wstring(str.begin(), str.end()).c_str());
}

template<unsigned maxLen>
vector<VarChar<maxLen>> strings_to_varchars(vector<std::string> strings) {
    auto varchars = vector<VarChar<maxLen>>(strings.size());
    std::transform(strings.begin(), strings.end(), varchars.begin(), [](std::string str) {
        return string_to_varchar<maxLen>(str);
    });
    return varchars;
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
