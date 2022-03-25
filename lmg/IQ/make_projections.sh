for i in **/**/*.v3dpbd; do vaa3d -x maximum_intensity_projection_Z_Slices -f mip_zslices -i $i -p 1:1:e -o $i_proj.tif; done 
