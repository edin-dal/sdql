cd ../..

for file in progs/job/gj/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  rm -f generated/$no_ext.cpp
done

# TODO codegen in batch like codegen_tpch.sh
for file in progs/job/gj/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  echo "Generating $no_ext"
  sbt "run benchmark $1 progs/job/gj $name"
done
