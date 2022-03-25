#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort_pixelsize/`
do
    vaa3d -x resample_swc -f resample_swc -i 0401_gold163_all_soma_sort_pixelsize/$D/consensus.eswc_resampled.swc_sorted.swc -o 0401_gold163_all_soma_sort_pixelsize/$D/consensus.swc -p 2
done
