qs=( 1 3 5 9 18 )

for i in "${qs[@]}"
do
  clang++ -O3 -march=native -mtune=native -Wno-narrowing -ftree-vectorize -Wno-deprecated-builtins --std c++20 q$i.cpp -o q$i.out
done
