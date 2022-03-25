#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort_restim/`
do
    vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/resized_vols/$D.resized.v3draw.v3dpbd 0401_gold163_all_soma_sort_restim/$D/consensus.eswc_resampled.swc_sorted.swc -o 0401_gold163_all_soma_sort_restim/$D/consensus_radius.swc -p AUTO 0
done
