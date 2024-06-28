#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document CUSTOMER("../datasets/tpch/customer.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document ORDERS("../datasets/tpch/orders.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);

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

class CustomerValues {
public:
  int operator[](const int i) const { return 0 <= i < CUSTOMER.GetRowCount(); }
};

class OrdersValues {
public:
  int operator[](const int i) const { return 0 <= i < ORDERS.GetRowCount(); }
};

class LineitemValues {
public:
  int operator[](const int i) const { return 0 <= i < LINEITEM.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto building = ConstantString("BUILDING", 9);
  auto c_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Customer::size(); i++) {
    const auto &c = customer;
    constexpr auto c_v = CustomerValues();
    if (c.c_mktsegment[i] == building) {
      c_h.emplace(c.c_custkey[i], std::tuple<long>(c.c_custkey[i]));
    }
  }

  ;
  auto o_h = phmap::flat_hash_map<long, std::tuple<long, long>>({});
  for (int i = 0; i < Orders::size(); i++) {
    const auto &o = orders;
    constexpr auto o_v = OrdersValues();
    if ((o.o_orderdate[i] < 19950315 && c_h.contains(o.o_custkey[i]))) {
      o_h.emplace(o.o_orderkey[i], std::tuple<long, long>(o.o_orderdate[i], o.o_shippriority[i]));
    }
  }

  ;
  auto li_h = phmap::flat_hash_map<std::tuple<long, long, long>, std::tuple<double>>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &li = lineitem;
    constexpr auto li_v = LineitemValues();
    if ((19950315 < li.l_shipdate[i] && o_h.contains(li.l_orderkey[i]))) {
      li_h[std::tuple<long, long, long>(li.l_orderkey[i], std::get<0>(o_h[li.l_orderkey[i]]),
                                        std::get<1>(o_h[li.l_orderkey[i]]))] +=
          std::tuple<double>((li.l_extendedprice[i] * (1 - li.l_discount[i])));
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<long, long, long, double>, long>({});
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