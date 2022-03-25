#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1_pixelsize/`
do
    I1="gold_163_all_soma_sort_s1_pixelsize/$D/"*.swc
    for I2 in "gold_163_all_soma_sort_s1_pixelsize/$D/auto_recons/"*sorted.swc
    do
	vaa3d -x resample_swc -f resample_swc -i $I2 -p 2
    done
    for I2 in "gold_163_all_soma_sort_s1_pixelsize/$D/processed/"*sorted.swc
    do
        vaa3d -x resample_swc -f resample_swc -i $I2 -p 2
    done
    vaa3d -x resample_swc -f resample_swc -i $I1 -p 2
done
