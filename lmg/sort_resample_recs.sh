#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1/`
do
    I1="gold_163_all_soma_sort_s1/$D/"*resampled.swc
    for I2 in "gold_163_all_soma_sort_s1/$D/auto_recons/"*resampled.swc
    do
	vaa3d -x sort_neuron_swc -f sort_swc -i $I2 &
    done
    for I2 in "gold_163_all_soma_sort_s1/$D/processed/"*resampled.swc
    do
        vaa3d --x sort_neuron_swc -f sort_swc -i $I2 &
    done
    vaa3d -x sort_neuron_swc -f sort_swc -i $I1
done
