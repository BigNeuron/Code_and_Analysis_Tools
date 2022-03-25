#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1/`
do
    I1="gold_163_all_soma_sort_s1/$D/"*resampled.swc
    for I2 in "gold_163_all_soma_sort_s1/$D/auto_recons/"*resampled.swc
    do
	tail -n +3 "$I2" > "$I2.tmp" && mv "$I2.tmp" "$I2"	
	#vaa3d -x resample_swc -f resample_swc -i $I2 -p 3
    done
    for I2 in "gold_163_all_soma_sort_s1/$D/processed/"*resampled.swc
    do
	tail -n +3 "$I2" > "$I2.tmp" && mv "$I2.tmp" "$I2"
        #vaa3d -x resample_swc -f resample_swc -i $I2 -p 3
    done
    tail -n +3 "$I1" > "$I1.tmp" && mv "$I1.tmp" "$I1"
    #vaa3d -x resample_swc -f resample_swc -i $I1 -p 3
done
