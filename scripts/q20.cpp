#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document NATION("../datasets/tpch/nation.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PARTSUPP("../datasets/tpch/partsupp.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document SUPPLIER("../datasets/tpch/supplier.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PART("../datasets/tpch/part.tbl", NO_HEADERS, SEPARATOR);

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

struct Nation {
  std::vector<long> n_nationkey;
  std::vector<VarChar<25>> n_name;
  std::vector<long> n_regionkey;
  std::vector<VarChar<152>> r_comment;
  static unsigned long size() { return NATION.GetRowCount(); }
};

struct Partsupp {
  std::vector<long> ps_partkey;
  std::vector<long> ps_suppkey;
  std::vector<double> ps_availqty;
  std::vector<double> ps_supplycost;
  std::vector<VarChar<199>> ps_comment;
  static unsigned long size() { return PARTSUPP.GetRowCount(); }
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

const Supplier supplier{
    SUPPLIER.GetColumn<long>(0),
    strings_to_varchars<25>(SUPPLIER.GetColumn<std::string>(1)),
    strings_to_varchars<40>(SUPPLIER.GetColumn<std::string>(2)),
    SUPPLIER.GetColumn<long>(3),
    strings_to_varchars<15>(SUPPLIER.GetColumn<std::string>(4)),
    SUPPLIER.GetColumn<double>(5),
    strings_to_varchars<101>(SUPPLIER.GetColumn<std::string>(6)),
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

const Nation nation{
    NATION.GetColumn<long>(0),
    strings_to_varchars<25>(NATION.GetColumn<std::string>(1)),
    NATION.GetColumn<long>(2),
    strings_to_varchars<152>(NATION.GetColumn<std::string>(3)),
};

const Partsupp partsupp{
    PARTSUPP.GetColumn<long>(0),
    PARTSUPP.GetColumn<long>(1),
    PARTSUPP.GetColumn<double>(2),
    PARTSUPP.GetColumn<double>(3),
    strings_to_varchars<199>(PARTSUPP.GetColumn<std::string>(4)),
};

class LineitemValues {
public:
  int operator[](const int i) const { return 0 <= i < LINEITEM.GetRowCount(); }
};

class NationValues {
public:
  int operator[](const int i) const { return 0 <= i < NATION.GetRowCount(); }
};

class PartsuppValues {
public:
  int operator[](const int i) const { return 0 <= i < PARTSUPP.GetRowCount(); }
};

class PartValues {
public:
  int operator[](const int i) const { return 0 <= i < PART.GetRowCount(); }
};

class SupplierValues {
public:
  int operator[](const int i) const { return 0 <= i < SUPPLIER.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto forest = ConstantString("forest", 7);
  auto canada = ConstantString("CANADA", 7);
  auto p_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Part::size(); i++) {
    const auto &p = part;
    constexpr auto p_v = PartValues();
    if (p.p_name[i].startsWith(forest)) {
      p_h.emplace(p.p_partkey[i], std::tuple<long>(p.p_partkey[i]));
    }
  }

  ;
  auto n_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Nation::size(); i++) {
    const auto &n = nation;
    constexpr auto n_v = NationValues();
    if (n.n_name[i] == canada) {
      n_h.emplace(n.n_nationkey[i], std::tuple<long>(n.n_nationkey[i]));
    }
  }

  ;
  auto s_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    if (n_h.contains(s.s_nationkey[i])) {
      s_h.emplace(s.s_suppkey[i], std::tuple<long>(s.s_suppkey[i]));
    }
  }

  ;
  auto l_h = phmap::flat_hash_map<std::tuple<long, long>, double>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &l = lineitem;
    constexpr auto l_v = LineitemValues();
    if ((((19940101 <= l.l_shipdate[i] && l.l_shipdate[i] < 19950101) && p_h.contains(l.l_partkey[i])) &&
         s_h.contains(l.l_suppkey[i]))) {
      l_h.emplace(std::tuple<long, long>(l.l_partkey[i], l.l_suppkey[i]), (0.5 * l.l_quantity[i]));
    }
  }

  ;
  auto ps_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Partsupp::size(); i++) {
    const auto &ps = partsupp;
    constexpr auto ps_v = PartsuppValues();
    auto key = std::tuple<long, long>(ps.ps_partkey[i], ps.ps_suppkey[i]);
    if ((l_h.contains(key) && l_h[key] < ps.ps_availqty[i])) {
      ps_h.emplace(ps.ps_suppkey[i], std::tuple<long>(ps.ps_suppkey[i]));
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<VarChar<25>, VarChar<40>>, long>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    if (ps_h.contains(s.s_suppkey[i])) {
      result.emplace(std::tuple<VarChar<25>, VarChar<40>>(s.s_name[i], s.s_address[i]), 1);
    }
  }

  ;
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}