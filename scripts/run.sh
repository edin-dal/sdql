qs=(
  1
  2
  3
  4
  5
  6
  7
  8
  9
  10
  11
  12
  13
  14
  15
  16
  17
  18
  19
  20
# FIXME
# 21
  22
)

for i in "${qs[@]}"
do
  echo "q$i"
  for j in {1..5}
  do
     ./q$i.out | grep --text Runtime
  done
done
