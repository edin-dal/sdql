cd ..
mkdir -p $1_results
echo "*" > "$1_results/.gitignore"

cd ../generated/
for file in ../progs/job/$1/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  echo $no_ext
  echo "Running $no_ext"
  ./$no_ext.out | grep --text " ms" > ../benchmarks/$1_results/$no_ext.result
done
