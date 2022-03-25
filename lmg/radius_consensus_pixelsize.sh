#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort_pixelsize/`
do
    vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd.tif 0401_gold163_all_soma_sort_pixelsize/$D/consensus.swc -o 0401_gold163_all_soma_sort_pixelsize/$D/consensus_radius.swc -p AUTO 0
done
