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
  21
  22
)

cd ../../generated/

for i in "${qs[@]}"
do
  echo "q$i"
  for ((j = 0; j < $1; j++))
  do
     ./q$i.out | grep --text Runtime
  done
done
