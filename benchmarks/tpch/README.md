The `verbatim` folder contains TPCH queries.

To check which files have been modified:

```sh
diff -q . verbatim
```

Handy commands to diff individual files:

```sh
export n="01"; diff --color q$n.sql verbatim/q$n.sql
```

```sh
export n="01"; vim -d q$n.sql verbatim/q$n.sql
```
