#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort/`
do
    #vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd 0401_gold163_all_soma_sort/$D/consensus.eswc_resampled.swc_sorted.swc -o 0401_gold163_all_soma_sort/$D/missing_branches
    vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd 0401_gold163_all_soma_sort/$D/consensus.swc -o 0401_gold163_all_soma_sort/$D/consensus.eswc.missing_branches
done
