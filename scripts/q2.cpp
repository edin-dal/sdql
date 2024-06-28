#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document NATION("../datasets/tpch/nation.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PARTSUPP("../datasets/tpch/partsupp.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document REGION("../datasets/tpch/region.tbl", NO_HEADERS, SEPARATOR);
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

struct Region {
  std::vector<long> r_regionkey;
  std::vector<VarChar<25>> r_name;
  std::vector<VarChar<152>> r_comment;
  static unsigned long size() { return REGION.GetRowCount(); }
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

const Partsupp partsupp{
    PARTSUPP.GetColumn<long>(0),
    PARTSUPP.GetColumn<long>(1),
    PARTSUPP.GetColumn<double>(2),
    PARTSUPP.GetColumn<double>(3),
    strings_to_varchars<199>(PARTSUPP.GetColumn<std::string>(4)),
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

class RegionValues {
public:
  int operator[](const int i) const { return 0 <= i < REGION.GetRowCount(); }
};

int main() {
  auto start = high_resolution_clock::now();

  auto brass = ConstantString("BRASS", 6);
  auto europe = ConstantString("EUROPE", 7);
  auto l_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Region::size(); i++) {
    const auto &r = region;
    constexpr auto r_v = RegionValues();
    if (r.r_name[i] == europe) {
      l_h.emplace(r.r_regionkey[i], std::tuple<long>(r.r_regionkey[i]));
    }
  }

  ;
  auto n_h = phmap::flat_hash_map<long, VarChar<25>>({});
  for (int i = 0; i < Nation::size(); i++) {
    const auto &n = nation;
    constexpr auto n_v = NationValues();
    if (l_h.contains(n.n_regionkey[i])) {
      n_h.emplace(n.n_nationkey[i], n.n_name[i]);
    }
  }

  ;
  auto s_h =
      phmap::flat_hash_map<long, std::tuple<double, VarChar<25>, VarChar<25>, VarChar<40>, VarChar<15>, VarChar<101>>>(
          {});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    if (n_h.contains(s.s_nationkey[i])) {
      s_h.emplace(s.s_suppkey[i], std::tuple<double, VarChar<25>, VarChar<25>, VarChar<40>, VarChar<15>, VarChar<101>>(
                                      s.s_acctbal[i], s.s_name[i], n_h[s.s_nationkey[i]], s.s_address[i], s.s_phone[i],
                                      s.s_comment[i]));
    }
  }

  ;
  auto p_h = phmap::flat_hash_map<long, std::tuple<VarChar<25>>>({});
  for (int i = 0; i < Part::size(); i++) {
    const auto &p = part;
    constexpr auto p_v = PartValues();
    if ((p.p_size[i] == 15 && p.p_type[i].endsWith(brass))) {
      p_h.emplace(p.p_partkey[i], std::tuple<VarChar<25>>(p.p_mfgr[i]));
    }
  }

  ;
  auto ps_h = phmap::flat_hash_map<long, double>({});
  for (int i = 0; i < Partsupp::size(); i++) {
    const auto &ps = partsupp;
    constexpr auto ps_v = PartsuppValues();
    if ((p_h.contains(ps.ps_partkey[i]) && s_h.contains(ps.ps_suppkey[i]))) {
      ps_h[ps.ps_partkey[i]] += ps.ps_supplycost[i];
    }
  }

  ;
  auto result = phmap::flat_hash_map<
      std::tuple<double, VarChar<25>, VarChar<25>, long, VarChar<25>, VarChar<15>, VarChar<40>, VarChar<101>>, long>(
      {});
  for (int i = 0; i < Partsupp::size(); i++) {
    const auto &ps = partsupp;
    constexpr auto ps_v = PartsuppValues();
    if (((ps_h.contains(ps.ps_partkey[i]) && ps_h[ps.ps_partkey[i]] == ps.ps_supplycost[i]) &&
         s_h.contains(ps.ps_suppkey[i]))) {
      result.emplace(
          std::tuple<double, VarChar<25>, VarChar<25>, long, VarChar<25>, VarChar<15>, VarChar<40>, VarChar<101>>(
              std::get<0>(s_h[ps.ps_suppkey[i]]), std::get<1>(s_h[ps.ps_suppkey[i]]),
              std::get<2>(s_h[ps.ps_suppkey[i]]), ps.ps_partkey[i], std::get<0>(p_h[ps.ps_partkey[i]]),
              std::get<4>(s_h[ps.ps_suppkey[i]]), std::get<3>(s_h[ps.ps_suppkey[i]]),
              std::get<5>(s_h[ps.ps_suppkey[i]])),
          1);
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