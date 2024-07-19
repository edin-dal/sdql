cd ../..

for file in progs/job/gj/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  rm -f generated/$no_ext.cpp
done

for file in progs/job/gj/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  echo "Generating $no_ext"
  sbt "run benchmark progs/job/gj $name"
done
