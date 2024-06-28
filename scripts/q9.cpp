#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document NATION("../datasets/tpch/nation.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PARTSUPP("../datasets/tpch/partsupp.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document SUPPLIER("../datasets/tpch/supplier.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document ORDERS("../datasets/tpch/orders.tbl", NO_HEADERS, SEPARATOR);
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

struct Orders {
  std::vector<long> o_orderkey;
  std::vector<long> o_custkey;
  std::vector<VarChar<1>> o_orderstatus;
  std::vector<double> o_totalprice;
  std::vector<long> o_orderdate;
  std::vector<VarChar<15>> o_orderpriority;
  std::vector<VarChar<15>> o_clerk;
  std::vector<long> o_shippriority;
  std::vector<VarChar<79>> o_comment;
  static unsigned long size() { return ORDERS.GetRowCount(); }
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

const Orders orders{
    ORDERS.GetColumn<long>(0),
    ORDERS.GetColumn<long>(1),
    strings_to_varchars<1>(ORDERS.GetColumn<std::string>(2)),
    ORDERS.GetColumn<double>(3),
    ORDERS.GetColumn<long>(4),
    strings_to_varchars<15>(ORDERS.GetColumn<std::string>(5)),
    strings_to_varchars<15>(ORDERS.GetColumn<std::string>(6)),
    ORDERS.GetColumn<long>(7),
    strings_to_varchars<79>(ORDERS.GetColumn<std::string>(8)),
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

class OrdersValues {
public:
  int operator[](const int i) const { return 0 <= i < ORDERS.GetRowCount(); }
};

class SupplierValues {
public:
  int operator[](const int i) const { return 0 <= i < SUPPLIER.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto green = ConstantString("green", 6);
  auto n_h = phmap::flat_hash_map<long, std::tuple<VarChar<25>>>({});
  for (int i = 0; i < Nation::size(); i++) {
    const auto &n = nation;
    constexpr auto n_v = NationValues();
    n_h.emplace(n.n_nationkey[i], std::tuple<VarChar<25>>(n.n_name[i]));
  }

  ;
  auto s_h = phmap::flat_hash_map<long, VarChar<25>>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    s_h.emplace(s.s_suppkey[i], std::get<0>(n_h[s.s_nationkey[i]]));
  }

  ;
  auto p_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Part::size(); i++) {
    const auto &p = part;
    constexpr auto p_v = PartValues();
    if (p.p_name[i].contains(green, 5)) {
      p_h.emplace(p.p_partkey[i], std::tuple<long>(p.p_partkey[i]));
    }
  }

  ;
  auto ps_h = phmap::flat_hash_map<std::tuple<long, long>, std::tuple<VarChar<25>, double>>({});
  for (int i = 0; i < Partsupp::size(); i++) {
    const auto &ps = partsupp;
    constexpr auto ps_v = PartsuppValues();
    if (p_h.contains(ps.ps_partkey[i])) {
      ps_h.emplace(std::tuple<long, long>(ps.ps_partkey[i], ps.ps_suppkey[i]),
                   std::tuple<VarChar<25>, double>(s_h[ps.ps_suppkey[i]], ps.ps_supplycost[i]));
    }
  }

  ;
  auto o_h = vector<long>(6000001);
  for (int i = 0; i < Orders::size(); i++) {
    const auto &o = orders;
    constexpr auto o_v = OrdersValues();
    o_h[o.o_orderkey[i]] = o.o_orderdate[i];
  }

  ;
  auto li_h = phmap::flat_hash_map<std::tuple<VarChar<25>, long>, std::tuple<double>>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &li = lineitem;
    constexpr auto li_v = LineitemValues();
    if (ps_h.contains(std::tuple<long, long>(li.l_partkey[i], li.l_suppkey[i]))) {
      li_h[std::tuple<VarChar<25>, long>(std::get<0>(ps_h[std::tuple<long, long>(li.l_partkey[i], li.l_suppkey[i])]),
                                         (o_h[li.l_orderkey[i]] / 10000))] +=
          std::tuple<double>(
              ((li.l_extendedprice[i] * (1.0 - li.l_discount[i])) -
               (std::get<1>(ps_h[std::tuple<long, long>(li.l_partkey[i], li.l_suppkey[i])]) * li.l_quantity[i])));
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<VarChar<25>, long, double>, long>({});
  for (const auto &[k, v] : li_h) {
    result.emplace(std::tuple_cat(k, v), 1);
  };
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}