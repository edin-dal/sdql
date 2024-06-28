#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document NATION("../datasets/tpch/nation.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document CUSTOMER("../datasets/tpch/customer.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document SUPPLIER("../datasets/tpch/supplier.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document ORDERS("../datasets/tpch/orders.tbl", NO_HEADERS, SEPARATOR);

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

struct Customer {
  std::vector<long> c_custkey;
  std::vector<VarChar<25>> c_name;
  std::vector<VarChar<40>> c_address;
  std::vector<long> c_nationkey;
  std::vector<VarChar<15>> c_phone;
  std::vector<double> c_acctbal;
  std::vector<VarChar<10>> c_mktsegment;
  std::vector<VarChar<117>> c_comment;
  static unsigned long size() { return CUSTOMER.GetRowCount(); }
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

const Nation nation{
    NATION.GetColumn<long>(0),
    strings_to_varchars<25>(NATION.GetColumn<std::string>(1)),
    NATION.GetColumn<long>(2),
    strings_to_varchars<152>(NATION.GetColumn<std::string>(3)),
};

const Customer customer{
    CUSTOMER.GetColumn<long>(0),
    strings_to_varchars<25>(CUSTOMER.GetColumn<std::string>(1)),
    strings_to_varchars<40>(CUSTOMER.GetColumn<std::string>(2)),
    CUSTOMER.GetColumn<long>(3),
    strings_to_varchars<15>(CUSTOMER.GetColumn<std::string>(4)),
    CUSTOMER.GetColumn<double>(5),
    strings_to_varchars<10>(CUSTOMER.GetColumn<std::string>(6)),
    strings_to_varchars<117>(CUSTOMER.GetColumn<std::string>(7)),
};

class LineitemValues {
public:
  int operator[](const int i) const { return 0 <= i < LINEITEM.GetRowCount(); }
};

class NationValues {
public:
  int operator[](const int i) const { return 0 <= i < NATION.GetRowCount(); }
};

class CustomerValues {
public:
  int operator[](const int i) const { return 0 <= i < CUSTOMER.GetRowCount(); }
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

  auto france = ConstantString("FRANCE", 7);
  auto germany = ConstantString("GERMANY", 8);
  auto nationkey_to_name = phmap::flat_hash_map<long, std::tuple<VarChar<25>>>({});
  for (int i = 0; i < Nation::size(); i++) {
    const auto &n = nation;
    constexpr auto n_v = NationValues();
    if ((n.n_name[i] == france || n.n_name[i] == germany)) {
      nationkey_to_name.emplace(n.n_nationkey[i], std::tuple<VarChar<25>>(n.n_name[i]));
    }
  }

  ;
  auto custkey_to_name = phmap::flat_hash_map<long, VarChar<25>>({});
  for (int i = 0; i < Customer::size(); i++) {
    const auto &c = customer;
    constexpr auto c_v = CustomerValues();
    if (nationkey_to_name.contains(c.c_nationkey[i])) {
      custkey_to_name.emplace(c.c_custkey[i], std::get<0>(nationkey_to_name[c.c_nationkey[i]]));
    }
  }

  ;
  auto orderkey_to_name = phmap::flat_hash_map<long, VarChar<25>>({});
  for (int i = 0; i < Orders::size(); i++) {
    const auto &o = orders;
    constexpr auto o_v = OrdersValues();
    if (custkey_to_name.contains(o.o_custkey[i])) {
      orderkey_to_name.emplace(o.o_orderkey[i], custkey_to_name[o.o_custkey[i]]);
    }
  }

  ;
  auto suppkey_to_name = phmap::flat_hash_map<long, VarChar<25>>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    if (nationkey_to_name.contains(s.s_nationkey[i])) {
      suppkey_to_name.emplace(s.s_suppkey[i], std::get<0>(nationkey_to_name[s.s_nationkey[i]]));
    }
  }

  ;
  auto l_h = phmap::flat_hash_map<std::tuple<VarChar<25>, VarChar<25>, long>, std::tuple<double>>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &l = lineitem;
    constexpr auto l_v = LineitemValues();
    if (((((19950101 <= l.l_shipdate[i] && l.l_shipdate[i] <= 19961231) &&
           orderkey_to_name.contains(l.l_orderkey[i])) &&
          suppkey_to_name.contains(l.l_suppkey[i])) &&
         ((orderkey_to_name[l.l_orderkey[i]] == france && suppkey_to_name[l.l_suppkey[i]] == germany) ||
          (orderkey_to_name[l.l_orderkey[i]] == germany && suppkey_to_name[l.l_suppkey[i]] == france)))) {
      l_h[std::tuple<VarChar<25>, VarChar<25>, long>(suppkey_to_name[l.l_suppkey[i]], orderkey_to_name[l.l_orderkey[i]],
                                                     (l.l_shipdate[i] / 10000))] +=
          std::tuple<double>((l.l_extendedprice[i] * (1.0 - l.l_discount[i])));
    }
  }

  ;
  auto result =
      phmap::flat_hash_map<std::tuple<std::tuple<VarChar<25>, VarChar<25>, long>, std::tuple<double>>, long>({});
  for (const auto &[k, v] : l_h) {
    result.emplace(std::tuple<std::tuple<VarChar<25>, VarChar<25>, long>, std::tuple<double>>(k, v), 1);
  };
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}