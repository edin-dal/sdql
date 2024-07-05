#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);

struct Lineitem {
  std::vector<long> l_orderkey;
  std::vector<long> l_partkey;
  std::vector<long> l_suppkey;
  std::vector<long> l_linenumber;
  std::vector<double> l_quantity;
  std::vector<double> l_extendedprice;
  std::vector<double> l_discount;
  std::vector<double> l_tax;
  std::vector<VarChar<1>> l_returnflag;
  std::vector<VarChar<1>> l_linestatus;
  std::vector<long> l_shipdate;
  std::vector<long> l_commitdate;
  std::vector<long> l_receiptdate;
  std::vector<VarChar<25>> l_shipinstruct;
  std::vector<VarChar<10>> l_shipmode;
  std::vector<VarChar<44>> l_comment;
  static unsigned long size() { return LINEITEM.GetRowCount(); }
};

const Lineitem lineitem{
    LINEITEM.GetColumn<long>(0),
    LINEITEM.GetColumn<long>(1),
    LINEITEM.GetColumn<long>(2),
    LINEITEM.GetColumn<long>(3),
    LINEITEM.GetColumn<double>(4),
    LINEITEM.GetColumn<double>(5),
    LINEITEM.GetColumn<double>(6),
    LINEITEM.GetColumn<double>(7),
    strings_to_varchars<1>(LINEITEM.GetColumn<std::string>(8)),
    strings_to_varchars<1>(LINEITEM.GetColumn<std::string>(9)),
    LINEITEM.GetColumn<long>(10),
    LINEITEM.GetColumn<long>(11),
    LINEITEM.GetColumn<long>(12),
    strings_to_varchars<25>(LINEITEM.GetColumn<std::string>(13)),
    strings_to_varchars<10>(LINEITEM.GetColumn<std::string>(14)),
    strings_to_varchars<44>(LINEITEM.GetColumn<std::string>(15)),
};

class LineitemValues {
public:
  int operator[](const int i) const { return 0 <= i < LINEITEM.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto li_h =
      phmap::flat_hash_map<std::tuple<VarChar<1>, VarChar<1>>, std::tuple<double, double, double, double, long>>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &li = lineitem;
    constexpr auto li_v = LineitemValues();
    if (li.l_shipdate[i] <= 19980902) {
      li_h[std::tuple<VarChar<1>, VarChar<1>>(li.l_returnflag[i], li.l_linestatus[i])] +=
          std::tuple<double, double, double, double, long>(
              li.l_quantity[i], li.l_extendedprice[i], ((li_v[i] * li.l_extendedprice[i]) * (1 - li.l_discount[i])),
              (((li_v[i] * li.l_extendedprice[i]) * (1 - li.l_discount[i])) * (1 + li.l_tax[i])), 1);
    }
  }

  ;
  auto result =
      phmap::flat_hash_map<std::tuple<VarChar<1>, VarChar<1>, double, double, double, double, long>, long>({});
  for (const auto &[k, v] : li_h) {
    result[std::tuple_cat(k, v)] += 1;
  };
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}