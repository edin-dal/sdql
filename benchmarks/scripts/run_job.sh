cd ..
mkdir -p job_results
echo "*" > "job_results/.gitignore"

cd ../generated/
for file in ../progs/job/gj/*.sdql; do
  name=${file##*/}
  no_ext="${name%.*}"
  echo $no_ext
  echo "Running $no_ext"
  ./$no_ext.out | grep --text " ms" > ../benchmarks/job_results/$no_ext.result
done
