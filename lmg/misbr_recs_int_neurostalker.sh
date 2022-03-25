#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1_int/`
do
    I1="gold_163_all_soma_sort_s1_int/$D/"*NeuroStalker.swc
    for I2 in "gold_163_all_soma_sort_s1_int/$D/auto_recons/"*NeuroStalker.swc
    do
	vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd $I2 -o $I2.missing_branches
    done
    for I2 in "gold_163_all_soma_sort_s1_int/$D/processed/"*NeuroStalker.swc
    do
        vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd $I2 -o $I2.missing_branches
    done
    vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd $I1 -o $I1.missing_branches
done
