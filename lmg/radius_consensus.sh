#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort/`
do
    vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd 0401_gold163_all_soma_sort/$D/consensus.eswc_resampled.swc_sorted.swc -o 0401_gold163_all_soma_sort/$D/consensus_radius.swc -p AUTO 0
done
