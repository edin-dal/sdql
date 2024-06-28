#include "../runtime/headers.h"
#include <chrono>
using namespace std::chrono;

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document PARTSUPP("../datasets/tpch/partsupp.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PART("../datasets/tpch/part.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document SUPPLIER("../datasets/tpch/supplier.tbl", NO_HEADERS, SEPARATOR);

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

const Partsupp partsupp{
    PARTSUPP.GetColumn<long>(0),
    PARTSUPP.GetColumn<long>(1),
    PARTSUPP.GetColumn<double>(2),
    PARTSUPP.GetColumn<double>(3),
    strings_to_varchars<199>(PARTSUPP.GetColumn<std::string>(4)),
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

const Supplier supplier{
    SUPPLIER.GetColumn<long>(0),
    strings_to_varchars<25>(SUPPLIER.GetColumn<std::string>(1)),
    strings_to_varchars<40>(SUPPLIER.GetColumn<std::string>(2)),
    SUPPLIER.GetColumn<long>(3),
    strings_to_varchars<15>(SUPPLIER.GetColumn<std::string>(4)),
    SUPPLIER.GetColumn<double>(5),
    strings_to_varchars<101>(SUPPLIER.GetColumn<std::string>(6)),
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

  auto brand = ConstantString("Brand#45", 9);
  auto medpol = ConstantString("MEDIUM POLISHED", 16);
  auto _customer = ConstantString("Customer", 9);
  auto _complaints = ConstantString("Complaints", 11);
  const auto nchars_customer = 8;
  auto p_h = phmap::flat_hash_map<long, std::tuple<VarChar<10>, VarChar<25>, long>>({});
  for (int i = 0; i < Part::size(); i++) {
    const auto &p = part;
    constexpr auto p_v = PartValues();
    if (((p.p_brand[i] != brand && !(p.p_type[i].startsWith(medpol))) &&
         (((((((p.p_size[i] == 49 || p.p_size[i] == 14) || p.p_size[i] == 23) || p.p_size[i] == 45) ||
             p.p_size[i] == 19) ||
            p.p_size[i] == 3) ||
           p.p_size[i] == 36) ||
          p.p_size[i] == 9))) {
      p_h.emplace(p.p_partkey[i], std::tuple<VarChar<10>, VarChar<25>, long>(p.p_brand[i], p.p_type[i], p.p_size[i]));
    }
  }

  ;
  auto s_h = phmap::flat_hash_map<long, std::tuple<long>>({});
  for (int i = 0; i < Supplier::size(); i++) {
    const auto &s = supplier;
    constexpr auto s_v = SupplierValues();
    auto idx_customer = s.s_comment[i].firstIndex(_customer);
    if ((idx_customer != -1 && (idx_customer + nchars_customer) <= s.s_comment[i].firstIndex(_complaints))) {
      s_h.emplace(s.s_suppkey[i], std::tuple<long>(s.s_suppkey[i]));
    }
  }

  ;
  auto ps_h = phmap::flat_hash_map<std::tuple<VarChar<10>, VarChar<25>, long>, phmap::flat_hash_map<long, long>>({});
  for (int i = 0; i < Partsupp::size(); i++) {
    const auto &ps = partsupp;
    constexpr auto ps_v = PartsuppValues();
    if ((p_h.contains(ps.ps_partkey[i]) && !(s_h.contains(ps.ps_suppkey[i])))) {
      ps_h[std::tuple<VarChar<10>, VarChar<25>, long>(
          std::get<0>(p_h[ps.ps_partkey[i]]), std::get<1>(p_h[ps.ps_partkey[i]]),
          std::get<2>(p_h[ps.ps_partkey[i]]))] += phmap::flat_hash_map<long, long>({{ps.ps_suppkey[i], 1}});
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<VarChar<10>, VarChar<25>, long, long>, long>({});
  for (const auto &[k, v] : ps_h) {
    result.emplace(std::tuple_cat(k, std::tuple<long>(v.size())), 1);
  };
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<milliseconds>(stop - start);
  cout << "Runtime (ms): " << duration.count() << endl;

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}