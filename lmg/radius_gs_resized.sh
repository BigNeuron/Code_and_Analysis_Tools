#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1_restim/`
do
    I1="gold_163_all_soma_sort_s1_restim/$D/"*resampled.swc
    I2=`ls "gold_163_all_soma_sort_s1_restim/$D/"*resampled.swc`.radius.swc
    vaa3d -x neuron_radius -f neuron_radius -i ../BigNeurongit/Data/resized_vols/$D.resized.v3draw.v3dpbd $I1 -o $I2 -p AUTO 0
done
