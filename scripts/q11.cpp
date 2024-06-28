#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document SUPPLIER("../datasets/tpch/supplier.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PARTSUPP("../datasets/tpch/partsupp.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document NATION("../datasets/tpch/nation.tbl", NO_HEADERS, SEPARATOR);

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

struct Partsupp {
  std::vector<long> ps_partkey;
  std::vector<long> ps_suppkey;
  std::vector<double> ps_availqty;
  std::vector<double> ps_supplycost;
  std::vector<VarChar<199>> ps_comment;
  static unsigned long size() { return PARTSUPP.GetRowCount(); }
};

struct Nation {
  std::vector<long> n_nationkey;
  std::vector<VarChar<25>> n_name;
  std::vector<long> n_regionkey;
  std::vector<VarChar<152>> r_comment;
  static unsigned long size() { return NATION.GetRowCount(); }
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

const Partsupp partsupp{
    PARTSUPP.GetColumn<long>(0),
    PARTSUPP.GetColumn<long>(1),
    PARTSUPP.GetColumn<double>(2),
    PARTSUPP.GetColumn<double>(3),
    strings_to_varchars<199>(PARTSUPP.GetColumn<std::string>(4)),
};

const Nation nation{
    NATION.GetColumn<long>(0),
    strings_to_varchars<25>(NATION.GetColumn<std::string>(1)),
    NATION.GetColumn<long>(2),
    strings_to_varchars<152>(NATION.GetColumn<std::string>(3)),
};

class SupplierValues {
public:
  int operator[](const int i) const { return 0 <= i < SUPPLIER.GetRowCount(); }
};

class PartsuppValues {
public:
  int operator[](const int i) const { return 0 <= i < PARTSUPP.GetRowCount(); }
};

class NationValues {
public:
  int operator[](const int i) const { return 0 <= i < NATION.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto germany = ConstantString("GERMANY", 8);
  auto n_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Nation::size(); i++) {
    const auto &n = nation;
    constexpr auto n_v = NationValues();
    if (n.n_name[i] == germany) {
      n_h.emplace(n.n_nationkey[i], std::tuple<long>(n.n_nationkey[i]));
    }
  }

  ;
  auto s_h = phmap::flat_hash_map<long, long>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    if (n_h.contains(s.s_nationkey[i])) {
      s_h.emplace(s.s_suppkey[i], 1);
    }
  }

  ;
  auto ps_t = std::tuple<double, phmap::flat_hash_map<long, double>>(0.0, {});
  for (int i = 0; i < Partsupp::size(); i++) {
    const auto &ps = partsupp;
    constexpr auto ps_v = PartsuppValues();
    if (s_h.contains(ps.ps_suppkey[i])) {
      get<0>(ps_t) += ((ps.ps_supplycost[i] * ps.ps_availqty[i]) * 1.0E-4);
      get<1>(ps_t) +=
          phmap::flat_hash_map<long, double>({{ps.ps_partkey[i], (ps.ps_supplycost[i] * ps.ps_availqty[i])}});
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<long, double>, long>({});
  for (const auto &[ps_partkey, ps_supplycost] : std::get<1>(ps_t)) {
    if (std::get<0>(ps_t) < ps_supplycost) {
      result[std::tuple<long, double>(ps_partkey, ps_supplycost)] += 1;
    }
  };
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}