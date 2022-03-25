avSNR = 237.9214; 
scalingdt = File.openAsRawString("/Users/linusmg/Desktop/bigneuron/Both_GS_and_auto/SWC2IMG_gold163/scaling_gold.csv",35000);
scalingdtarr = split(scalingdt, "\n");
print(scalingdtarr[0]);
print(scalingdtarr.length)
for (i = 1; i < scalingdtarr.length; i++) {
	filesplit = split(scalingdtarr[i],",");
	//print(filesplit[0]);
	swcfile = replace(filesplit[0], "Data", "Desktop");
	//swcfile = replace(swcfile, "gold_163_all_soma_sort_s1", "gold_163_all_soma_sort_s1_backup");
	swcfile = replace(swcfile, "gold_163_all_soma_sort_s1", "gold_163_all_soma_sort_s1_restim");
	swcfile = replace(swcfile, "home", "Users");
	swcfile = swcfile + "_resampled.swc.radius.swc";
	print(swcfile);
	print("x_y_pix");
	print(filesplit[2]);
	print("z_pix");
	print(filesplit[3]);
	
	print("Generated image will be saved as:");
	swcfilesplit = split(swcfile,"/");
	print(swcfilesplit[7] + ".swc2img.tif");
	File.makeDirectory("/Users/linusmg/Desktop/bigneuron/Both_GS_and_auto/SWC2IMG_gold163/" + filesplit[1]);
	File.copy(swcfile, "/Users/linusmg/Desktop/bigneuron/Both_GS_and_auto/SWC2IMG_gold163/" + filesplit[1] + "/" + swcfilesplit[7]);
	swcfile2 = "/Users/linusmg/Desktop/bigneuron/Both_GS_and_auto/SWC2IMG_gold163/" + filesplit[1] + "/" + swcfilesplit[7];
	run("SWC2IMG", "file=&swcfile2 lateral=1 axial=1 background=10 correlation=2 snr=" + avSNR + " rescale correct save");
	run("Collect Garbage");
}