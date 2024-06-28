#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document SUPPLIER("../datasets/tpch/supplier.tbl", NO_HEADERS, SEPARATOR);

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

struct Supplier {
  std::vector<long> s_suppkey;
  std::vector<VarChar<25>> s_name;
  std::vector<VarChar<40>> s_address;
  std::vector<long> s_nationkey;
  std::vector<VarChar<15>> s_phone;
  std::vector<double> s_acctbal;
  std::vector<VarChar<101>> s_comment;
  static unsigned long size() { return SUPPLIER.GetRowCount(); }
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

const Supplier supplier{
    SUPPLIER.GetColumn<long>(0),
    strings_to_varchars<25>(SUPPLIER.GetColumn<std::string>(1)),
    strings_to_varchars<40>(SUPPLIER.GetColumn<std::string>(2)),
    SUPPLIER.GetColumn<long>(3),
    strings_to_varchars<15>(SUPPLIER.GetColumn<std::string>(4)),
    SUPPLIER.GetColumn<double>(5),
    strings_to_varchars<101>(SUPPLIER.GetColumn<std::string>(6)),
};

class LineitemValues {
public:
  int operator[](const int i) const { return 0 <= i < LINEITEM.GetRowCount(); }
};

class SupplierValues {
public:
  int operator[](const int i) const { return 0 <= i < SUPPLIER.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto suppkey_to_revenue = phmap::flat_hash_map<long, double>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &l = lineitem;
    constexpr auto l_v = LineitemValues();
    if ((19960101 <= l.l_shipdate[i] && l.l_shipdate[i] < 19960401)) {
      suppkey_to_revenue[l.l_suppkey[i]] += (l.l_extendedprice[i] * (1.0 - l.l_discount[i]));
    }
  }

  ;
  const auto max_revenue = 1772627.2087;
  auto suppkey_to_supp = phmap::flat_hash_map<long, std::tuple<VarChar<25>, VarChar<40>, VarChar<15>>>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    suppkey_to_supp.emplace(
        s.s_suppkey[i], std::tuple<VarChar<25>, VarChar<40>, VarChar<15>>(s.s_name[i], s.s_address[i], s.s_phone[i]));
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<long, VarChar<25>, VarChar<40>, VarChar<15>, double>, long>({});
  for (const auto &[suppkey, revenue] : suppkey_to_revenue) {
    if (revenue == max_revenue) {
      result[std::tuple<long, VarChar<25>, VarChar<40>, VarChar<15>, double>(
          suppkey, /* name */ std::get<0>(suppkey_to_supp[suppkey]),
          /* address */ std::get<1>(suppkey_to_supp[suppkey]), /* phone */ std::get<2>(suppkey_to_supp[suppkey]),
          revenue)] += 1;
    }
  };
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}