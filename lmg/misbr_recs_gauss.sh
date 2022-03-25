#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1/`
do
    I1="gold_163_all_soma_sort_s1/$D/"*resampled.swc_sorted.swc.radius.swc
    for I2 in "gold_163_all_soma_sort_s1/$D/auto_recons/"*resampled.swc_sorted.swc.radius.swc
    do
	vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.gauss.8bit.v3draw $I2 -o $I2.missing_branches_gauss
    done
    for I2 in "gold_163_all_soma_sort_s1/$D/processed/"*resampled.swc_sorted.swc.radius.swc
    do
        vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.gauss.8bit.v3draw $I2 -o $I2.missing_branches_gauss
    done
    vaa3d -x missing_branch_detection -f missing_branch_detection -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.gauss.8bit.v3draw $I1 -o $I1.missing_branches_gauss
done
