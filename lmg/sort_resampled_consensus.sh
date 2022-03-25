#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort/`
do
    vaa3d -x sort_neuron_swc -f sort_swc -i 0401_gold163_all_soma_sort/$D/consensus.eswc_resampled.swc 
done
