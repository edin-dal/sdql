#include "../runtime/headers.h"

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

const rapidcsv::Document LINEITEM("../datasets/tpch/lineitem.tbl", NO_HEADERS, SEPARATOR);
const rapidcsv::Document PART("../datasets/tpch/part.tbl", NO_HEADERS, SEPARATOR);

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

class LineitemValues {
public:
  int operator[](const int i) const { return 0 <= i < LINEITEM.GetRowCount(); }
};

class PartValues {
public:
  int operator[](const int i) const { return 0 <= i < PART.GetRowCount(); }
};

int main() {
  auto brand12 = ConstantString("Brand#12", 9);
  auto brand23 = ConstantString("Brand#23", 9);
  auto brand34 = ConstantString("Brand#34", 9);
  auto smcase = ConstantString("SM CASE", 8);
  auto smbox = ConstantString("SM BOX", 7);
  auto smpack = ConstantString("SM PACK", 8);
  auto smpkg = ConstantString("SM PKG", 7);
  auto mdbag = ConstantString("MED BAG", 8);
  auto mdbox = ConstantString("MED BOX", 8);
  auto mdpack = ConstantString("MED PACK", 9);
  auto mdpkg = ConstantString("MED PKG", 8);
  auto lgcase = ConstantString("LG CASE", 8);
  auto lgbox = ConstantString("LG BOX", 7);
  auto lgpack = ConstantString("LG PACK", 8);
  auto lgpkg = ConstantString("LG PKG", 7);
  auto air = ConstantString("AIR", 4);
  auto airreg = ConstantString("AIR REG", 8);
  auto deliverinperson = ConstantString("DELIVER IN PERSON", 18);
  auto p_h = phmap::flat_hash_map<long, std::tuple<VarChar<10>, long, VarChar<10>>>({});
  for (int i = 0; i < Part::size(); i++) {
    const auto &p = part;
    constexpr auto p_v = PartValues();
    if ((((((p.p_brand[i] == brand12 &&
             (((p.p_container[i] == smcase || p.p_container[i] == smbox) || p.p_container[i] == smpack) ||
              p.p_container[i] == smpkg)) &&
            1 <= p.p_size[i]) &&
           p.p_size[i] <= 5) ||
          (((p.p_brand[i] == brand23 &&
             (((p.p_container[i] == mdbag || p.p_container[i] == mdbox) || p.p_container[i] == mdpack) ||
              p.p_container[i] == mdpkg)) &&
            1 <= p.p_size[i]) &&
           p.p_size[i] <= 10)) ||
         (((p.p_brand[i] == brand34 &&
            (((p.p_container[i] == lgcase || p.p_container[i] == lgbox) || p.p_container[i] == lgpack) ||
             p.p_container[i] == lgpkg)) &&
           1 <= p.p_size[i]) &&
          p.p_size[i] <= 15))) {
      p_h.emplace(p.p_partkey[i],
                  std::tuple<VarChar<10>, long, VarChar<10>>(p.p_brand[i], p.p_size[i], p.p_container[i]));
    }
  }

  ;
  auto res = double(0.0);
  for (int i = 0; i < Lineitem::size(); i++) {
    const auto &l = lineitem;
    constexpr auto l_v = LineitemValues();
    auto p_brand = std::get<0>(p_h[l.l_partkey[i]]);
    if ((((p_h.contains(l.l_partkey[i]) && (l.l_shipmode[i] == air || l.l_shipmode[i] == airreg)) &&
          l.l_shipinstruct[i] == deliverinperson) &&
         ((((p_brand == brand12 && 1 <= l.l_quantity[i]) && l.l_quantity[i] <= 11) ||
           ((p_brand == brand23 && 10 <= l.l_quantity[i]) && l.l_quantity[i] <= 20)) ||
          ((p_brand == brand34 && 20 <= l.l_quantity[i]) && l.l_quantity[i] <= 30)))) {
      res += (l.l_extendedprice[i] * (1.0 - l.l_discount[i]));
    }
  }

  ;
  auto result = phmap::flat_hash_map<std::tuple<double>, long>({{std::tuple<double>(res), 1}});

  for (const auto &[key, val] : result) {
    std::cout << std::setprecision(std::numeric_limits<double>::digits10) << key << ":" << val << std::endl;
  }
}