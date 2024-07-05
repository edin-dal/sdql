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

cd ../generated/

for i in "${qs[@]}"
do
  rm -f q$i.out
done

for i in "${qs[@]}"
do
  echo "Compiling q$i"
  clang++ -std=c++17 -O3 -march=native -mtune=native -Wno-narrowing -ftree-vectorize -Wno-deprecated-builtins q$i.cpp -o q$i.out
done
