#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort/`
do
    vaa3d -x resample_swc -f resample_swc -i 0401_gold163_all_soma_sort/$D/consensus.swc -o 0401_gold163_all_soma_sort/$D/consensus.eswc_resampled.swc -p 3
done
