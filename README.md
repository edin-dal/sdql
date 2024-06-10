SDQL
----

A prototype implementation of SDQL in Scala.

# Syntax

* Refer to the [OOPSLA'22 paper](https://dl.acm.org/doi/pdf/10.1145/3527333) for more details.
* For comparison, `<` and `<=` are used instead of `>` and `>=`.
* The following syntactic sugar constructs are not supported: array `[| x1, ..., xn |]` and set `{ x1, ..., xn }` construction, key-set of dictionary `dom`, and one-branch conditional `if e0 then e1`.

# Tests

```
sbt test
```

# Running the Interpreter

```
sbt
run interpret <path> <sdql_files>*
```

For example, to run TPCH Q6, first make sure that the folder `datasets/tpch` contains TPCH tables (with a small scale factor). Then, run the following command:

```
sbt
run interpret progs/tpch q6.sdql
```

# Running the Compiler

Similar to the above:

```
sbt
run compile <path> <sdql_files>*
```

For example, to run compiled TPCH Q1 and Q6:

```
sbt
run compile progs/tpch q1.sdql q6.sdql
```

Note: compilation requires `clang++` and `clang-format` installed.

## TPCH tables format

The data loader does not expect TPCH tables to have end-of-line `|` characters.

Strip them as follows:

```bash
sed -i 's/|$//' datasets/tpch/*.tbl
```

On OSX:

```bash
sed -i '' 's/|$//' datasets/tpch/*.tbl
```
