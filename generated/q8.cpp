#include "../runtime/headers.h"

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document NATION("../datasets/tpch/nation.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document CUSTOMER("../datasets/tpch/customer.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document REGION("../datasets/tpch/region.tbl", NO_HEADERS, SEPARATOR);
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

struct Region {
  std::vector<long> r_regionkey;
  std::vector<VarChar<25>> r_name;
  std::vector<VarChar<152>> r_comment;
  static unsigned long size() { return REGION.GetRowCount(); }
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

const Region region{
    REGION.GetColumn<long>(0),
    strings_to_varchars<25>(REGION.GetColumn<std::string>(1)),
    strings_to_varchars<152>(REGION.GetColumn<std::string>(2)),
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

class RegionValues {
public:
  int operator[](const int i) const { return 0 <= i < REGION.GetRowCount(); }
};

int main() {
  auto steel = ConstantString("ECONOMY ANODIZED STEEL", 23);
  auto america = ConstantString("AMERICA", 8);
  auto brazil = ConstantString("BRAZIL", 7);
  auto r_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Region::size(); i++) {
    const auto &r = region;
    constexpr auto r_v = RegionValues();
    if (r.r_name[i] == america) {
      r_h.emplace(r.r_regionkey[i], std::tuple<long>(r.r_regionkey[i]));
    }
  }

  ;
  auto n_h = phmap::flat_hash_map<long, long>({});
  for (int i = 0; i < Nation::size(); i++) {
    const auto &n = nation;
    constexpr auto n_v = NationValues();
    if (r_h.contains(n.n_regionkey[i])) {
      n_h.emplace(n.n_nationkey[i], 1);
    }
  }

  ;
  auto nationkey_to_name = phmap::flat_hash_map<long, std::tuple<VarChar<25>>>({});
  for (int i = 0; i < Nation::size(); i++) {
    const auto &n = nation;
    constexpr auto n_v = NationValues();
    nationkey_to_name.emplace(n.n_nationkey[i], std::tuple<VarChar<25>>(n.n_name[i]));
  }

  ;
  auto s_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    s_h.emplace(s.s_suppkey[i], std::tuple<long>(s.s_nationkey[i]));
  }

  ;
  auto c_h = vector<long>(6000001);
  for (int i = 0; i < Customer::size(); i++) {
    const auto &c = customer;
    constexpr auto c_v = CustomerValues();
    c_h[c.c_custkey[i]] = c.c_nationkey[i];
  }

  ;
  auto p_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Part::size(); i++) {
    const auto &p = part;
    constexpr auto p_v = PartValues();
    if (p.p_type[i] == steel) {
      p_h.emplace(p.p_partkey[i], std::tuple<long>(p.p_partkey[i]));
    }
  }

  ;
  auto o_h = phmap::flat_hash_map<long, std::tuple<long, long>>({});
  for (int i = 0; i < Orders::size(); i++) {
    const auto &o = orders;
    constexpr auto o_v = OrdersValues();
    if ((19950101 <= o.o_orderdate[i] && o.o_orderdate[i] <= 19961231)) {
      o_h.emplace(o.o_orderkey[i], std::tuple<long, long>(o.o_custkey[i], o.o_orderdate[i]));
    }
  }

  ;
  auto l_h = phmap::flat_hash_map<long, std::tuple<double, double>>({});
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &l = lineitem;
    constexpr auto l_v = LineitemValues();
    if (((p_h.contains(l.l_partkey[i]) && o_h.contains(l.l_orderkey[i])) &&
         n_h.contains(c_h[std::get<0>(o_h[l.l_orderkey[i]])]))) {
      auto orderdate = std::get<1>(o_h[l.l_orderkey[i]]);
      auto orderyear = (orderdate / 10000);
      auto volume = (l.l_extendedprice[i] * (1.0 - l.l_discount[i]));
      auto brazil_volume = (std::get<0>(nationkey_to_name[std::get<0>(s_h[l.l_suppkey[i]])]) == brazil) ? volume : 0.0;
      l_h[orderyear] += std::tuple<double, double>(brazil_volume, volume);
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<long, double>, long>({});
  for (const auto &[k, v] : l_h) {
    result.emplace(std::tuple<long, double>(k, (std::get<0>(v) / std::get<1>(v))), 1);
  };

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}