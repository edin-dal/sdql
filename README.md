SDQL Query
----

An implementation of SDQL (Semi-ring Dictionary Query Language) in Scala for query processing.

## Syntax

* For more details, refer to the [OOPSLA'22 paper](https://dl.acm.org/doi/pdf/10.1145/3527333) or the more
  recent [A semi-ring dictionary query language for data science](https://www.research.ed.ac.uk/en/publications/a-semi-ring-dictionary-query-language-for-data-science).
* For comparison, `<` and `<=` are used instead of `>` and `>=`.
* The following syntactic sugar constructs are not supported: array construction `[| x1, ..., xn |]` and key-set of
  dictionary `dom`.

## TPCH datasets

Generate datasets as follows:

```sh
# or clone it elsewhere
git clone https://github.com/edin-dal/tpch-dbgen
cd tpch-dbgen  
make
./dbgen -s 1 -vf
# or move them elsewhere and create a symlink
# (careful: macOS aliases are not symlinks!)
mv *.tbl ../datasets/tpch
cd ..
```

_Note: for the interpreter you will want a smaller scale factor like `-s 0.01`._

### TPCH tables format

The data loader does not expect TPCH tables to have end-of-line `|` characters.

Strip them:

```sh
sed -i 's/|$//' datasets/tpch/*.tbl
```

On macOS:

```sh
sed -i '' 's/|$//' datasets/tpch/*.tbl
```

## Running tests

You can check everything works by running the tests:

```sh
sbt test
```

To automatically run `sbt test` before each push, configure the local git hooks in `hooks`:

```sh
git config core.hooksPath hooks
```

### Optional tests

These are slower end-to-end tests: they generate, compile/interpret, and check the results of full queries.

First, comment out the global `-l` options in `build.sbt`.

You can then run the optional tests using these commands:

```sh
# fast test (< 1 min)
sbt "testOnly * -- -n TestTPCH0_01"
```

```sh
# slower test (> 1 min)
sbt "testOnly * -- -n TestTPCH1"
```

#### Result files

These tests will compare their results against a ground truth we provide.

Set it up as follows:

```sh
# or clone it elsewhere
git clone https://github.com/edin-dal/sdql-benchmark
# create a symlink from the path expected by the tests
ln -s sdql-benchmark/results results  
```

_Note: make sure you also have the required files in your `datasets` folder._

## Running the Compiler

```sh
sbt
run compile <path> <sdql_files>*
```

For example, to run compiled TPCH Q1 and Q6:

```sh
sbt
run compile progs/tpch q1.sdql q6.sdql
```

_Note: compilation requires `clang++` and `clang-format` to be installed._

## Running the Interpreter

_⚠️ The interpreter is deprecated, support is limited to programs in the folder `progs/tpch-interpreter`_.

```sh
sbt
run interpret <path> <sdql_files>*
```

For example, to run TPCH Q6, first make sure that the folder `datasets/tpch` contains TPCH tables (with a small scale
factor). Then, run the following command:

```sh
sbt
run interpret progs/tpch-interpreter q6.sdql
```

Or as a one-liner: `sbt "run interpret progs/tpch-interpreter q6.sdql"`

## Citing SDQL

To cite SDQL, use the following BibTex:

```
@article{DBLP:journals/pacmpl/ShaikhhaHSO22,
  author       = {Amir Shaikhha and
                  Mathieu Huot and
                  Jaclyn Smith and
                  Dan Olteanu},
  title        = {Functional collection programming with semi-ring dictionaries},
  journal      = {Proc. {ACM} Program. Lang.},
  volume       = {6},
  number       = {{OOPSLA1}},
  pages        = {1--33},
  year         = {2022},
  url          = {https://doi.org/10.1145/3527333},
  doi          = {10.1145/3527333},
  timestamp    = {Tue, 10 Jan 2023 16:19:51 +0100},
  biburl       = {https://dblp.org/rec/journals/pacmpl/ShaikhhaHSO22.bib},
  bibsource    = {dblp computer science bibliography, https://dblp.org}
}
```

Depending on your usecase, the following papers are also relevant:

* [SDQLpy](https://github.com/edin-dal/sdqlpy), a python embedding of SDQL for query processing

```
@inproceedings{DBLP:conf/cc/ShahrokhiS23,
  author       = {Hesam Shahrokhi and
                  Amir Shaikhha},
  editor       = {Clark Verbrugge and
                  Ondrej Lhot{\'{a}}k and
                  Xipeng Shen},
  title        = {Building a Compiled Query Engine in Python},
  booktitle    = {Proceedings of the 32nd {ACM} {SIGPLAN} International Conference on
                  Compiler Construction, {CC} 2023, Montr{\'{e}}al, QC, Canada,
                  February 25-26, 2023},
  pages        = {180--190},
  publisher    = {{ACM}},
  year         = {2023},
  url          = {https://doi.org/10.1145/3578360.3580264},
  doi          = {10.1145/3578360.3580264},
  timestamp    = {Mon, 20 Feb 2023 14:39:08 +0100},
  biburl       = {https://dblp.org/rec/conf/cc/ShahrokhiS23.bib},
  bibsource    = {dblp computer science bibliography, https://dblp.org}
}
```

* SDQLite, a subset of SDQL for (sparse) tensor algebra

```
@article{DBLP:journals/pacmmod/SchleichSS23,
  author       = {Maximilian Schleich and
                  Amir Shaikhha and
                  Dan Suciu},
  title        = {Optimizing Tensor Programs on Flexible Storage},
  journal      = {Proc. {ACM} Manag. Data},
  volume       = {1},
  number       = {1},
  pages        = {37:1--37:27},
  year         = {2023},
  url          = {https://doi.org/10.1145/3588717},
  doi          = {10.1145/3588717},
  timestamp    = {Thu, 15 Jun 2023 21:57:49 +0200},
  biburl       = {https://dblp.org/rec/journals/pacmmod/SchleichSS23.bib},
  bibsource    = {dblp computer science bibliography, https://dblp.org}
}
```

* Forward-mode Automatic Differentiation for SDQLite

```
@inproceedings{DBLP:conf/cgo/ShaikhhaHH24,
  author       = {Amir Shaikhha and
                  Mathieu Huot and
                  Shideh Hashemian},
  editor       = {Tobias Grosser and
                  Christophe Dubach and
                  Michel Steuwer and
                  Jingling Xue and
                  Guilherme Ottoni and
                  ernando Magno Quint{\~{a}}o Pereira},
  title        = {A Tensor Algebra Compiler for Sparse Differentiation},
  booktitle    = {{IEEE/ACM} International Symposium on Code Generation and Optimization,
                  {CGO} 2024, Edinburgh, United Kingdom, March 2-6, 2024},
  pages        = {1--12},
  publisher    = {{IEEE}},
  year         = {2024},
  url          = {https://doi.org/10.1109/CGO57630.2024.10444787},
  doi          = {10.1109/CGO57630.2024.10444787},
  timestamp    = {Mon, 11 Mar 2024 13:45:28 +0100},
  biburl       = {https://dblp.org/rec/conf/cgo/ShaikhhaHH24.bib},
  bibsource    = {dblp computer science bibliography, https://dblp.org}
}
```
