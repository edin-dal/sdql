SDQL
----

An implementation of SDQL (Semi-ring Dictionary Query Language) in Scala.

# Syntax

* For more details, refer to the [OOPSLA'22 paper](https://dl.acm.org/doi/pdf/10.1145/3527333) or the more recent [A semi-ring dictionary query language for data science](https://www.research.ed.ac.uk/en/publications/a-semi-ring-dictionary-query-language-for-data-science).
* For comparison, `<` and `<=` are used instead of `>` and `>=`.
* The following syntactic sugar constructs are not supported: array `[| x1, ..., xn |]` construction and key-set of dictionary `dom`.

# TPCH datasets

Generate datasets as follows:

```sh
git clone https://github.com/edin-dal/tpch-dbgen
cd tpch-dbgen  
make
./dbgen -s 1 -vf
mv *.tbl ../datasets/tpch
cd ..
```

_Note: for the interpreter you will want a smaller scale factor like `-s 0.01`._

## TPCH tables format

The data loader does not expect TPCH tables to have end-of-line `|` characters.

Strip them:

```sh
sed -i 's/|$//' datasets/tpch/*.tbl
```

On macOS:

```sh
sed -i '' 's/|$//' datasets/tpch/*.tbl
```

# Running tests

You can check everything works by running the tests:

```sh
sbt test
```

To automatically run `sbt test` before each push, configure the local git hooks in `hooks`:

```sh
git config core.hooksPath hooks
```

# Running the Compiler

```sh
sbt
run compile <path> <sdql_files>*
```

For example, to run compiled TPCH Q1 and Q6:

```sh
sbt
run compile progs/tpch q1.sdql q6.sdql
```

Note: compilation requires `clang++` and `clang-format` to be installed.

# Running the Interpreter

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

# Running benchmarks

See README in the `benchmarks` directory.
