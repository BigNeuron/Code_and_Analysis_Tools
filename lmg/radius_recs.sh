#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1/`
do
    I1="gold_163_all_soma_sort_s1/$D/"*resampled.swc_sorted.swc
    for I2 in "gold_163_all_soma_sort_s1/$D/auto_recons/"*resampled.swc_sorted.swc
    do
	vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd $I2 -o $I2.radius.swc -p AUTO 0
    done
    for I2 in "gold_163_all_soma_sort_s1/$D/processed/"*resampled.swc_sorted.swc
    do
        vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd $I2 -o $I2.radius.swc -p AUTO 0
    done
    vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/gold166_wids_vols/$D/$D.v3dpbd $I1 -o $I1.radius.swc -p AUTO 0
done
