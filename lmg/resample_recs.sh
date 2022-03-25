#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1/`
do
    I1="gold_163_all_soma_sort_s1/$D/"*.swc
    for I2 in "gold_163_all_soma_sort_s1/$D/auto_recons/"*.swc
    do
	vaa3d -x resample_swc -f resample_swc -i $I2 -p 3
    done
    for I2 in "gold_163_all_soma_sort_s1/$D/processed/"*.swc
    do
        vaa3d -x resample_swc -f resample_swc -i $I2 -p 3
    done
    vaa3d -x resample_swc -f resample_swc -i $I1 -p 3
done
