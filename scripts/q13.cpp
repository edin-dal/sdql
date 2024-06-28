#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document ORDERS("../datasets/tpch/orders.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document CUSTOMER("../datasets/tpch/customer.tbl", NO_HEADERS, SEPARATOR);

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

class OrdersValues {
public:
  int operator[](const int i) const { return 0 <= i < ORDERS.GetRowCount(); }
};

class CustomerValues {
public:
  int operator[](const int i) const { return 0 <= i < CUSTOMER.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto special = ConstantString("special", 8);
  auto requests = ConstantString("requests", 9);
  const auto nchars_special = 7;
  auto o_h = phmap::flat_hash_map<long, long>({});
  for (int i = 0; i < Orders::size(); i++) {
    const auto &o = orders;
    constexpr auto o_v = OrdersValues();
    auto idx_special = o.o_comment[i].firstIndex(special);
    if ((idx_special == -1 || o.o_comment[i].lastIndex(requests) < (idx_special + nchars_special))) {
      o_h[o.o_custkey[i]] += 1;
    }
  }

  ;
  auto c_h = phmap::flat_hash_map<std::tuple<long>, std::tuple<long>>({});
  for (int i = 0; i < Customer::size(); i++) {
    const auto &c = customer;
    constexpr auto c_v = CustomerValues();
    c_h[std::tuple<long>((o_h.contains(c.c_custkey[i])) ? o_h[c.c_custkey[i]] : 0)] += std::tuple<long>(1);
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<long, long>, long>({});
  for (const auto &[k, v] : c_h) {
    result[std::tuple_cat(k, v)] += 1;
  };
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}