#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PART("../datasets/tpch/part.tbl", NO_HEADERS, SEPARATOR);

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

struct Part {
  std::vector<long> p_partkey;
  std::vector<VarChar<55>> p_name;
  std::vector<VarChar<25>> p_mfgr;
  std::vector<VarChar<10>> p_brand;
  std::vector<VarChar<25>> p_type;
  std::vector<long> p_size;
  std::vector<VarChar<10>> p_container;
  std::vector<double> p_retailprice;
  std::vector<VarChar<23>> p_comment;
  static unsigned long size() { return PART.GetRowCount(); }
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

const Part part{
    PART.GetColumn<long>(0),
    strings_to_varchars<55>(PART.GetColumn<std::string>(1)),
    strings_to_varchars<25>(PART.GetColumn<std::string>(2)),
    strings_to_varchars<10>(PART.GetColumn<std::string>(3)),
    strings_to_varchars<25>(PART.GetColumn<std::string>(4)),
    PART.GetColumn<long>(5),
    strings_to_varchars<10>(PART.GetColumn<std::string>(6)),
    PART.GetColumn<double>(7),
    strings_to_varchars<23>(PART.GetColumn<std::string>(8)),
};

class LineitemValues {
public:
  int operator[](const int i) const { return 0 <= i < LINEITEM.GetRowCount(); }
};

class PartValues {
public:
  int operator[](const int i) const { return 0 <= i < PART.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto brand = ConstantString("Brand#23", 9);
  auto medbox = ConstantString("MED BOX", 8);
  auto p_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Part::size(); i++) {
    const auto &p = part;
    constexpr auto p_v = PartValues();
    if ((p.p_brand[i] == brand && p.p_container[i] == medbox)) {
      p_h.emplace(p.p_partkey[i], std::tuple<long>(p.p_partkey[i]));
    }
  }

  ;
  auto l_h = phmap::flat_hash_map<long, std::tuple<double, double>>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &l = lineitem;
    constexpr auto l_v = LineitemValues();
    if (p_h.contains(l.l_partkey[i])) {
      l_h[l.l_partkey[i]] += std::tuple<double, double>(l.l_quantity[i], 1.0);
    }
  }

  ;
  auto tot = double(0.0);
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &l = lineitem;
    constexpr auto l_v = LineitemValues();
    auto avg = ((0.2 * std::get<0>(l_h[l.l_partkey[i]])) / std::get<1>(l_h[l.l_partkey[i]]));
    if ((l_h.contains(l.l_partkey[i]) && l.l_quantity[i] < avg)) {
      tot += l.l_extendedprice[i];
    }
  }

  ;
  auto result = (tot / 7.0);
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  std::cout << std::setprecision(std::numeric_limits<double>::digits10) << result << std::endl;
}