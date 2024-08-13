#pragma once

#include <regex>
#include <sstream>

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const std::regex RE_DATE("-");

inline long date_to_numeric(std::string str) {
    return std::stol(std::regex_replace(str, RE_DATE, ""));
}

inline vector<long> dates_to_numerics(vector<std::string> strings) {
    auto numerics = vector<long>(strings.size());
    std::transform(strings.begin(), strings.end(), numerics.begin(), date_to_numeric);
    return numerics;
}

template<unsigned maxLen>
VarChar<maxLen> string_to_varchar(std::string str) {
    return VarChar<maxLen>(std::wstring(str.begin(), str.end()).c_str());
}

template<unsigned maxLen>
vector<VarChar<maxLen>> strings_to_varchars(vector<std::string> strings) {
    auto varchars = vector<VarChar<maxLen>>(strings.size());
    std::transform(strings.begin(), strings.end(), varchars.begin(), string_to_varchar<maxLen>);
    return varchars;
}

inline std::string print_date(const long yyyymmdd) {
  const auto yyyy = yyyymmdd / 10000;
  const auto mmdd = yyyymmdd - yyyy * 10000;
  const auto mm = mmdd / 100;
  const auto dd = mmdd - mm * 100;
  const auto mm_str = (mm < 10) ? "0" + std::to_string(mm) : std::to_string(mm);
  const auto dd_str = (dd < 10) ? "0" + std::to_string(dd) : std::to_string(dd);
  std::stringstream ss;
  ss << yyyy << "-" << mm_str << "-" << dd_str;
  return ss.str();
}
