cd ../..

for file in progs/job/$1/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  rm -f generated/$no_ext.cpp
done

qs=( $(ls progs/job/$1/ | grep '.sdql' ) )
sbt "run benchmark $2 progs/job/$1 $(printf '%q ' "${qs[@]}")"
