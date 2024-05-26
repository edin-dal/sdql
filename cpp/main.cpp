// clang++ -std=c++20 $(pkg-config --cflags --libs  arrow-csv) main.cpp && ./a.out && rm a.out

#include <filesystem>
#include <iostream>
#include <regex>
#include "parallel_hashmap/phmap.h"
#include "rapidcsv.h"
#include "tuple_helper.h"

const auto DATASETS_DIR = std::filesystem::path("..") / "datasets" / "tpch";
const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');

phmap::flat_hash_map<std::tuple<std::string, std::string>, std::tuple<double, double, double, double, int> > q1() {
    const rapidcsv::Document lineitem(DATASETS_DIR / "lineitem.tbl", NO_HEADERS, SEPARATOR);
    const auto l_orderkey = lineitem.GetColumn<int>(0);
    const auto l_partkey = lineitem.GetColumn<int>(1);
    const auto l_suppkey = lineitem.GetColumn<int>(2);
    const auto l_linenumber = lineitem.GetColumn<int>(3);
    const auto l_quantity = lineitem.GetColumn<double>(4);
    const auto l_extendedprice = lineitem.GetColumn<double>(5);
    const auto l_discount = lineitem.GetColumn<double>(6);
    const auto l_tax = lineitem.GetColumn<double>(7);
    const auto l_returnflag = lineitem.GetColumn<std::string>(8);
    const auto l_linestatus = lineitem.GetColumn<std::string>(9);
    const auto l_shipdate = lineitem.GetColumn<std::string>(10);
    const auto l_commitdate = lineitem.GetColumn<std::string>(11);
    const auto l_receiptdate = lineitem.GetColumn<std::string>(12);
    const auto l_shipinstruct = lineitem.GetColumn<std::string>(13);
    const auto l_shipmode = lineitem.GetColumn<std::string>(14);
    const auto l_comment = lineitem.GetColumn<std::string>(15);

    phmap::flat_hash_map<std::tuple<std::string, std::string>, std::tuple<double, double, double, double, int> >
            map({});
    for (int i = 0; i < lineitem.GetRowCount(); i++) {
        if (l_shipdate[i] <= "1998-09-02") {
            std::tuple key(l_returnflag[i], l_linestatus[i]);
            std::tuple val(
                l_quantity[i],
                l_extendedprice[i],
                l_extendedprice[i] * (1.0 - l_discount[i]),
                l_extendedprice[i] * (1.0 - l_discount[i]) * (1.0 + l_tax[i]),
                1
            );
            map[key] += val;
        }
    }

    return map;
}

double q6() {
    const rapidcsv::Document lineitem(DATASETS_DIR / "lineitem.tbl", NO_HEADERS, SEPARATOR);
    const auto l_orderkey = lineitem.GetColumn<int>(0);
    const auto l_partkey = lineitem.GetColumn<int>(1);
    const auto l_suppkey = lineitem.GetColumn<int>(2);
    const auto l_linenumber = lineitem.GetColumn<int>(3);
    const auto l_quantity = lineitem.GetColumn<double>(4);
    const auto l_extendedprice = lineitem.GetColumn<double>(5);
    const auto l_discount = lineitem.GetColumn<double>(6);
    const auto l_tax = lineitem.GetColumn<double>(7);
    const auto l_returnflag = lineitem.GetColumn<std::string>(8);
    const auto l_linestatus = lineitem.GetColumn<std::string>(9);
    const auto l_shipdate = lineitem.GetColumn<std::string>(10);
    const auto l_commitdate = lineitem.GetColumn<std::string>(11);
    const auto l_receiptdate = lineitem.GetColumn<std::string>(12);
    const auto l_shipinstruct = lineitem.GetColumn<std::string>(13);
    const auto l_shipmode = lineitem.GetColumn<std::string>(14);
    const auto l_comment = lineitem.GetColumn<std::string>(15);

    auto scalar = 0.0;
    for (int i = 0; i < lineitem.GetRowCount(); i++) {
        if (
            l_shipdate[i] >= "1994-01-01" && l_shipdate[i] < "1995-01-01" &&
            l_discount[i] >= 0.05 && l_discount[i] <= 0.07 &&
            l_quantity[i] < 24.0
        ) {
            scalar += l_extendedprice[i] * l_discount[i];
        };
    }

    return scalar;
}

phmap::flat_hash_map<std::tuple<int>, std::tuple<int> > q13() {
    const rapidcsv::Document customer(DATASETS_DIR / "customer.tbl", NO_HEADERS, SEPARATOR);
    const auto c_custkey = customer.GetColumn<int>(0);
    const auto c_name = customer.GetColumn<std::string>(1);
    const auto c_address = customer.GetColumn<std::string>(2);
    const auto c_nationkey = customer.GetColumn<int>(3);
    const auto c_phone = customer.GetColumn<std::string>(4);
    const auto c_acctbal = customer.GetColumn<double>(5);
    const auto c_mktsegment = customer.GetColumn<std::string>(6);
    const auto c_comment = customer.GetColumn<std::string>(7);

    const rapidcsv::Document orders(DATASETS_DIR / "orders.tbl", NO_HEADERS, SEPARATOR);
    const auto o_orderkey = orders.GetColumn<int>(0);
    const auto o_custkey = orders.GetColumn<int>(1);
    const auto o_orderstatus = orders.GetColumn<std::string>(2);
    const auto o_totalprice = orders.GetColumn<double>(3);
    const auto o_orderdate = orders.GetColumn<std::string>(4);
    const auto o_orderpriority = orders.GetColumn<std::string>(5);
    const auto o_clerk = orders.GetColumn<std::string>(6);
    const auto o_shippriority = orders.GetColumn<int>(7);
    const auto o_comment = orders.GetColumn<std::string>(8);

    phmap::flat_hash_map<int, int> o_map({});
    const std::regex re(".*special.*requests.*");
    for (int i = 0; i < orders.GetRowCount(); i++) {
        if (!std::regex_match(o_comment[i], re)) {
            o_map[o_custkey[i]] += 1;
        }
    }

    phmap::flat_hash_map<std::tuple<int>, std::tuple<int> > c_map({});
    for (int i = 0; i < customer.GetRowCount(); i++) {
        std::tuple key(o_map.contains(c_custkey[i]) ? o_map.at(c_custkey[i]) : 0);
        c_map[key] += std::tuple(1);
    }

    return c_map;
}


int main() {
    for (const auto &[fst, snd]: q1()) {
        std::cout << fst << ':' << snd << std::endl;
    }

    std::cout << q6() << std::endl;

    for (const auto &[fst, snd]: q13()) {
        std::cout << fst << ':' << snd << std::endl;
    }
}


// #include <arrow/io/api.h>
// #include <arrow/csv/api.h>
// #include <arrow/scalar.h>
//
// int main() {
//     const auto &io_context = arrow::io::default_io_context();
//     const auto memory_pool = arrow::default_memory_pool();
//
//     const auto csv_file = DATASETS_DIR / "lineitem.tbl";
//     const auto input = *arrow::io::ReadableFile::Open(csv_file, memory_pool);
//
//     const auto read_options = arrow::csv::ReadOptions({.autogenerate_column_names = true});
//     const auto parse_options = arrow::csv::ParseOptions({.delimiter = '|'});
//     const auto convert_options = arrow::csv::ConvertOptions::Defaults();
//
//     const auto reader = *arrow::csv::TableReader::Make(
//         io_context, input, read_options, parse_options, convert_options);
//
//     const auto table = *reader->Read();
//     std::cout << "num_rows:" << table->num_rows() << std::endl;
//     std::cout << "schema:" << *table->schema() << std::endl;
//     std::cout << "table:" << table->ToString() << std::endl;
//
//     const auto column = table->column(0);
//     std::cout << "column:" << column->ToString() << std::endl;
//
//     const auto scalar = *column->GetScalar(0);
//     std::cout << "scalar:" << scalar->ToString() << std::endl;
// }


// # include "csv.h"
// // https://github.com/ben-strasser/fast-cpp-csv-parser
//
// int main() {
//     io::CSVReader<16, io::trim_chars<' '>, io::no_quote_escape<'|'> > in(DATASETS_DIR / "lineitem.tbl");
//     in.set_header(
//         "l_orderkey", "l_partkey", "l_suppkey", "l_linenumber", "l_quantity", "l_extendedprice", "l_discount", "l_tax",
//         "l_returnflag", "l_linestatus", "l_shipdate", "l_commitdate", "l_receiptdate", "l_shipinstruct", "l_shipmode",
//         "l_comment"
//     );
//     int _0;
//     int _1;
//     int _2;
//     int _3;
//     double _4;
//     double _5;
//     double _6;
//     double _7;
//     std::string _8;
//     std::string _9;
//     std::string _10;
//     std::string _11;
//     std::string _12;
//     std::string _13;
//     std::string _14;
//     std::string _15;
//     while (in.read_row(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15)
//     ) {
//         std::cout << _0 << "|" << _1 << "|" << _2 << "|" << _3 << "|" << _4 << "|" << _5 << "|" << _6 << "|" << _7 <<
//                 "|" << _8 << "|" << _9 << "|" << _10 << "|" << _11 << "|" << _12 << "|" << _13 << "|" << _14 << "|" <<
//                 _15 << std::endl;
//     }
// }


// # include "csv.hpp"
// // https://github.com/vincentlaucsb/csv-parser
//
// int main() {
//     const std::vector<std::string> colnames = {
//         "l_orderkey", "l_partkey", "l_suppkey", "l_linenumber", "l_quantity", "l_extendedprice", "l_discount", "l_tax",
//         "l_returnflag", "l_linestatus", "l_shipdate", "l_commitdate", "l_receiptdate", "l_shipinstruct", "l_shipmode",
//         "l_comment"
//     };
//     const auto format = csv::CSVFormat().delimiter('|').column_names(colnames);
//     const auto filename = DATASETS_DIR / "lineitem.tbl";
//
//     for (csv::CSVReader reader(filename.generic_string(), format); csv::CSVRow &row: reader) {
//         for (csv::CSVField &field: row) {
//             std::cout << field.get<csv::string_view>() << std::endl;
//         }
//     }
// }
