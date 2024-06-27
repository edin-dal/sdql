SDQL
----

A prototype implementation of SDQL in Scala.

# Syntax

* Refer to the [OOPSLA'22 paper](https://dl.acm.org/doi/pdf/10.1145/3527333) for more details.
* For comparison, `<` and `<=` are used instead of `>` and `>=`.
* The following syntactic sugar constructs are not supported: array `[| x1, ..., xn |]` and set `{ x1, ..., xn }` construction, key-set of dictionary `dom`, and one-branch conditional `if e0 then e1`.

# Tests

```sh
sbt test
```

To automatically run `sbt test` before each push, configure the local git hooks in `hooks`:

```sh
git config core.hooksPath hooks
```

# Running the Interpreter

```sh
sbt
run interpret <path> <sdql_files>*
```

For example, to run TPCH Q6, first make sure that the folder `datasets/tpch` contains TPCH tables (with a small scale factor). Then, run the following command:

```sh
sbt
run interpret progs/tpch q6.sdql
```

# Running the Compiler

Similar to the above:

```sh
sbt
run compile <path> <sdql_files>*
```

For example, to run compiled TPCH Q1 and Q6:

```sh
sbt
run compile progs/tpch q1.sdql q6.sdql
```

Note: compilation requires `clang++` and `clang-format` installed.

# Running benchmarks

See README in the `benchmarks` directory.

# TPCH tables format

The data loader does not expect TPCH tables to have end-of-line `|` characters.

Strip them as follows:

```sh
sed -i 's/|$//' datasets/tpch/*.tbl
```

On macOS:

```sh
sed -i '' 's/|$//' datasets/tpch/*.tbl
```
