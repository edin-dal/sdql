#include "../runtime/headers.h"

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document CUSTOMER("../datasets/tpch/customer.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document ORDERS("../datasets/tpch/orders.tbl", NO_HEADERS, SEPARATOR);

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

class CustomerValues {
public:
  int operator[](const int i) const { return 0 <= i < CUSTOMER.GetRowCount(); }
};

class OrdersValues {
public:
  int operator[](const int i) const { return 0 <= i < ORDERS.GetRowCount(); }
};

int main() {
  auto _13 = ConstantString("13", 3);
  auto _31 = ConstantString("31", 3);
  auto _23 = ConstantString("23", 3);
  auto _29 = ConstantString("29", 3);
  auto _30 = ConstantString("30", 3);
  auto _18 = ConstantString("18", 3);
  auto _17 = ConstantString("17", 3);
  auto o_h = vector<long>(6000001);
  for (int i = 0; i < Orders::size(); i++) {
    const auto &o = orders;
    constexpr auto o_v = OrdersValues();
    o_h[o.o_custkey[i]] = 1;
  }

  ;
  auto fused = std::tuple<double, double>(0.0, 0.0);
  for (int i = 0; i < Customer::size(); i++) {
    const auto &c = customer;
    constexpr auto c_v = CustomerValues();
    auto cond = (0.0 < c.c_acctbal[i] &&
                 ((((((c.c_phone[i].startsWith(_13) || c.c_phone[i].startsWith(_31)) || c.c_phone[i].startsWith(_23)) ||
                     c.c_phone[i].startsWith(_29)) ||
                    c.c_phone[i].startsWith(_30)) ||
                   c.c_phone[i].startsWith(_18)) ||
                  c.c_phone[i].startsWith(_17)));
    if (true) {
      get<0>(fused) += (cond) ? c.c_acctbal[i] : 0.0;
      get<1>(fused) += (cond) ? 1.0 : 0.0;
    }
  }

  ;
  auto avg = (/* total */ std::get<0>(fused) / /* count */ std::get<1>(fused));
  auto res = phmap::flat_hash_map<std::tuple<VarChar<2>>, std::tuple<long, double>>({});
  for (int i = 0; i < Customer::size(); i++) {
    const auto &c = customer;
    constexpr auto c_v = CustomerValues();
    if (((avg < c.c_acctbal[i] && !(o_h)[c.c_custkey[i]]) &&
         ((((((c.c_phone[i].startsWith(_13) || c.c_phone[i].startsWith(_31)) || c.c_phone[i].startsWith(_23)) ||
             c.c_phone[i].startsWith(_29)) ||
            c.c_phone[i].startsWith(_30)) ||
           c.c_phone[i].startsWith(_18)) ||
          c.c_phone[i].startsWith(_17)))) {
      res[std::tuple<VarChar<2>>(c.c_phone[i].substr<2>(0, 2))] += std::tuple<long, double>(1, c.c_acctbal[i]);
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<VarChar<2>, long, double>, long>({});
  for (const auto &[k, v] : res) {
    result.emplace(std::tuple_cat(k, v), 1);
  };

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}