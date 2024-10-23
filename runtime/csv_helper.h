#pragma once

#include <regex>
#include <sstream>

static_assert(sizeof(int) * CHAR_BIT == 32, "int is not 32 bits");
inline rapidcsv::ConverterParams IntNanConverter(const uint16_t offset) {
    return rapidcsv::ConverterParams(
        true,
        std::numeric_limits<long double>::signaling_NaN(),
        std::numeric_limits<int>::min() + offset,
        false
    );
}

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const std::regex RE_DATE("-");

inline int date_to_numeric(std::string str) {
    return std::stoi(std::regex_replace(str, RE_DATE, ""));
}

inline vector<int> dates_to_numerics(vector<std::string> strings) {
    auto numerics = vector<int>(strings.size());
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

inline std::string print_date(const int yyyymmdd) {
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
