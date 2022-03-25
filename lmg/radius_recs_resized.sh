#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1_restim/`
do
    I1="gold_163_all_soma_sort_s1_restim/$D/"*resampled.swc
    for I2 in "gold_163_all_soma_sort_s1_restim/$D/auto_recons/"*resampled.swc_sorted.swc
    do
	vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/resized_vols/$D.resized.v3draw.v3dpbd $I2 -o $I2.radius.swc -p AUTO 0
    done
    for I2 in "gold_163_all_soma_sort_s1_restim/$D/processed/"*resampled.swc_sorted.swc
    do
        vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/resized_vols/$D.resized.v3draw.v3dpbd $I2 -o $I2.radius.swc -p AUTO 0
    done
#    vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/resized_vols/$D.resized.v3draw.v3dpbd $I1 -o $I1.radius.swc -p AUTO 0
done
