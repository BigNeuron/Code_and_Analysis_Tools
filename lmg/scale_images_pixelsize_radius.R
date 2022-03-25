
psdf <- read.csv('scaling_gold.csv')

# psdf <- psdf[psdf$id!=126,]

for(i in psdf$id){
  
  infn <- paste0("../BigNeurongit/Data/gold166_wids_vols/",i,"/",i,".v3dpbd")
  outfn <- paste0("../BigNeurongit/Data/gold166_wids_vols/",i,"/",i,".resized.v3dpbd")
  
  xy <- psdf$x_y_pix_size[psdf$id==i]
  z <- psdf$z_pix_size[psdf$id==i]
  
  system(paste0("vaa3d -x image_resample -f up_sample -i ",infn," -o ",outfn," -p ",1," ",1," ",z/xy," 1"))
}

