#for D in `find . -type d`
for D in `ls 0401_gold163_all_soma_sort/`
do
    vaa3d -x eswc_converter -f eswc_to_swc -i 0401_gold163_all_soma_sort/$D/consensus.eswc -o 0401_gold163_all_soma_sort/$D/consensus.swc    
done
