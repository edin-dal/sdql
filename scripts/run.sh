qs=( 1 3 5 9 18 )

for i in "${qs[@]}"
do
  echo "q$i"
  for j in {1..5}
  do
     ./q$i.out | grep --text Runtime
  done
done
