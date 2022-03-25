#for D in `find . -type d`
for D in `ls gold_163_all_soma_sort_s1/`
do
    #//Do whatever you need with D
    #echo $D
    I1="gold_163_all_soma_sort_s1/$D/"*.swc
    mkdir gold_163_all_soma_sort_s1/$D/dists_auto
    mkdir gold_163_all_soma_sort_s1/$D/dists_proc
    mkdir gold_163_all_soma_sort_s1/$D/dists_cons
    #echo $I1
    for I2 in "gold_163_all_soma_sort_s1/$D/auto_recons/"*.swc
    do
        #echo $I2
	O=$(basename $I2)
        #echo $O
	vaa3d -x neuron_distance -f neuron_distance -i $I1 $I2 -p 2 -o gold_163_all_soma_sort_s1/$D/dists_auto/$O.dist.txt
        #vaa3d -x blastneuron -f pre_processing -p "#i $I1 #o gold_163_all_soma_sort_s1/"$D"/dists/"$O"_prep.swc #s 2 #l 3 #r 1" 
        #vaa3d -x blastneuron -f pre_processing -p "#i $I2 #o "$I2"_prep.swc #s 2 #l 3 #r 1" 
	#vaa3d -x blastneuron -f 
        #java -jar ../DIADEM/DiademMetric.jar -G $I1 -T $I2 -D 0 | head -n1 | awk '{print $2;}' > gold_163_all_soma_sort_s1/$D/dists/$O.diadem.txt
        java -jar ../DIADEM/DiademMetric.jar -G $I1 -T $I2 -D 0 > gold_163_all_soma_sort_s1/$D/dists_auto/$O.diadem.txt
    done
    for I2 in "gold_163_all_soma_sort_s1/$D/processed/"*.swc
    do
        #echo $I2
	O=$(basename $I2)
        #echo $O
	vaa3d -x neuron_distance -f neuron_distance -i $I1 $I2 -p 2 -o gold_163_all_soma_sort_s1/$D/dists_proc/$O.dist.txt
        #vaa3d -x blastneuron -f pre_processing -p "#i $I1 #o gold_163_all_soma_sort_s1/"$D"/dists/"$O"_prep.swc #s 2 #l 3 #r 1" 
        #vaa3d -x blastneuron -f pre_processing -p "#i $I2 #o "$I2"_prep.swc #s 2 #l 3 #r 1" 
	#vaa3d -x blastneuron -f 
        #java -jar ../DIADEM/DiademMetric.jar -G $I1 -T $I2 -D 0 | head -n1 | awk '{print $2;}' > gold_163_all_soma_sort_s1/$D/dists/$O.diadem.txt
        java -jar ../DIADEM/DiademMetric.jar -G $I1 -T $I2 -D 0 > gold_163_all_soma_sort_s1/$D/dists_proc/$O.diadem.txt
    done
    O=$(basename $I1)
    vaa3d -x neuron_distance -f neuron_distance -i $I1 0401_gold163_all_soma_sort/$D/consensus.eswc -p 2 -o gold_163_all_soma_sort_s1/$D/dists_cons/$O.dist.txt
    java -jar ../DIADEM/DiademMetric.jar -G $I1 -T 0401_gold163_all_soma_sort/$D/consensus.eswc -D 0 > gold_163_all_soma_sort_s1/$D/dists_cons/$O.diadem.txt    
done
