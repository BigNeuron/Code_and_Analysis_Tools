#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort/`
do
    vaa3d -x refine_swc -f refine_v2 -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd 0401_gold163_all_soma_sort/$D/consensus_radius.swc -o 0401_gold163_all_soma_sort/$D/consensus_radius_refined.swc
done
