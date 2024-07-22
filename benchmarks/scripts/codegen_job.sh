cd ../..

for file in progs/job/gj/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  rm -f generated/$no_ext.cpp
done

qs=( $(ls progs/job/gj/ | grep '.sdql' ) )
sbt "run benchmark $1 progs/job/gj $(printf '%q ' "${qs[@]}")"
