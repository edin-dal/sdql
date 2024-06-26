// this is the generated Q21 from sdqlpy - to compare against our codegen

static PyObject *q21(PyObject *self, PyObject *args) {

  PyObject *db_;
  if (!PyArg_ParseTuple(args, "O", &db_)) {
    PyErr_SetString(PyExc_ValueError, "Error while parsing the sum inputs.");
    return NULL;
  }

  DB dbobj;
  DB *db = &dbobj;

  const static int numpy_initialized = init_numpy();

  auto su_size = PyArray_Size(PyList_GetItem(PyList_GetItem(db_, 0), 0));
  db->su_dataset_size = su_size;
  auto s_suppkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 0));
  auto s_name = (VarChar<25> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 1));
  auto s_address = (VarChar<40> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 2));
  auto s_nationkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 3));
  auto s_phone = (VarChar<15> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 4));
  auto s_acctbal = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 5));
  auto s_comment = (VarChar<101> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 6));
  auto s_NA = (VarChar<1> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 0), 7));

  auto li_size = PyArray_Size(PyList_GetItem(PyList_GetItem(db_, 1), 0));
  db->li_dataset_size = li_size;
  auto l_orderkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 0));
  auto l_partkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 1));
  auto l_suppkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 2));
  auto l_linenumber = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 3));
  auto l_quantity = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 4));
  auto l_extendedprice = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 5));
  auto l_discount = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 6));
  auto l_tax = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 7));
  auto l_returnflag = (VarChar<1> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 8));
  auto l_linestatus = (VarChar<1> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 9));
  auto l_shipdate = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 10));
  auto l_commitdate = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 11));
  auto l_receiptdate = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 12));
  auto l_shipinstruct = (VarChar<25> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 13));
  auto l_shipmode = (VarChar<10> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 14));
  auto l_comment = (VarChar<44> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 15));
  auto l_NA = (VarChar<1> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 1), 16));

  auto ord_size = PyArray_Size(PyList_GetItem(PyList_GetItem(db_, 2), 0));
  db->ord_dataset_size = ord_size;
  auto o_orderkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 0));
  auto o_custkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 1));
  auto o_orderstatus = (VarChar<1> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 2));
  auto o_totalprice = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 3));
  auto o_orderdate = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 4));
  auto o_orderpriority = (VarChar<15> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 5));
  auto o_clerk = (VarChar<15> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 6));
  auto o_shippriority = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 7));
  auto o_comment = (VarChar<79> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 8));
  auto o_NA = (VarChar<1> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 2), 9));

  auto na_size = PyArray_Size(PyList_GetItem(PyList_GetItem(db_, 3), 0));
  db->na_dataset_size = na_size;
  auto n_nationkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 3), 0));
  auto n_name = (VarChar<25> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 3), 1));
  auto n_regionkey = (long *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 3), 2));
  auto n_comment = (VarChar<152> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 3), 3));
  auto n_NA = (VarChar<1> *)PyArray_DATA(PyList_GetItem(PyList_GetItem(db_, 3), 4));

  auto v505 = phmap::flat_hash_map<tuple<VarChar<25>, long>, bool>({});
  const auto &saudi = ConstantString("SAUDI ARABIA", 13);
  const auto &f = ConstantString("F", 2);
  auto v506 = db->na_dataset_size;
  auto v492 = phmap::flat_hash_map<long, tuple<long>>({});
  for (int v491 = 0; v491 < v506; v491++) {
    if ((n_name[v491] == saudi)) {
      v492.emplace(n_nationkey[v491], make_tuple(n_nationkey[v491]));
    };
  }
  const auto &nation_indexed = v492;
  const auto &v493 = nation_indexed;
  auto v507 = db->su_dataset_size;
  auto v495 = phmap::flat_hash_map<long, VarChar<25>>({});
  for (int v494 = 0; v494 < v507; v494++) {
    if (true) {
      if (((v493).contains(s_nationkey[v494]))) {
        v495.emplace(s_suppkey[v494], s_name[v494]);
      }
    };
  }
  const auto &su_probed = v495;
  auto v508 = db->ord_dataset_size;
  vector<bool> v497(6000001);
  for (int v496 = 0; v496 < v508; v496++) {
    if ((o_orderstatus[v496] == f)) {
      v497[o_orderkey[v496]] = true;
    };
  }
  const auto &ord_indexed = v497;
  auto v509 = db->li_dataset_size;
  vector<vector<long>> v499(6000001);
  for (int v498 = 0; v498 < v509; v498++) {
    v499[l_orderkey[v498]].emplace_back(l_suppkey[v498]);
  }
  const auto &l2_indexed = v499;
  auto v510 = db->li_dataset_size;
  vector<vector<long>> v501(6000001);
  for (int v500 = 0; v500 < v510; v500++) {
    if ((l_receiptdate[v500] > l_commitdate[v500])) {
      v501[l_orderkey[v500]].emplace_back(l_suppkey[v500]);
    };
  }
  const auto &l3_indexed = v501;
  auto v511 = db->li_dataset_size;
  auto v503 = phmap::flat_hash_map<tuple<VarChar<25>>, tuple<long>>({});
  for (int v502 = 0; v502 < v511; v502++) {
    if ((((((l_receiptdate[v502] > l_commitdate[v502]) && ((su_probed).contains(l_suppkey[v502]))) &&
           ((ord_indexed[l_orderkey[v502]] != false))) &&
          ((l2_indexed[l_orderkey[v502]].size()) > (long)1)) &&
         ((((l3_indexed[l_orderkey[v502]].size()) > (long)0) && ((l3_indexed[l_orderkey[v502]].size()) > (long)1)) ==
          false))) {
      v503[make_tuple((su_probed).at(l_suppkey[v502]))] += make_tuple((long)1);
    };
  }
  const auto &l1_probed = v503;

  for (auto &v504 : l1_probed) {
    v505[tuple_cat((v504.first), move((v504.second)))] = true;
  }
  const auto &results = v505;
  const auto &out = results;

  FastDict_s25i_b *result = (FastDict_s25i_b *)PyObject_CallObject(
      PyObject_CallObject(PyObject_GetAttrString(PyImport_Import(PyUnicode_FromString("test_all_fastdict_compiled")),
                                                 (char *)"new_FastDict_s25i_b"),
                          nullptr),
      nullptr);
  *(result->dict) = out;

  return (PyObject *)PyObject_CallObject(
      PyObject_GetAttrString(PyImport_Import(PyUnicode_FromString("sdqlpy.fastd")), "fastd"),
      Py_BuildValue("(OO)", result, PyUnicode_FromString("test_all")));
}
