library(foreach)

cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)

psdf <- read.csv('scaling_gold.csv')

dir.create("gold_163_all_soma_sort_s1_pixelsize_radius")
dir.create("0401_gold163_all_soma_sort_pixelsize_radius")

# psdf <- psdf[psdf$id!=126,]

for(i in psdf$id){
  goldfn <- list.files(paste0('gold_163_all_soma_sort_s1_pixelsize/',i),pattern='/*_resampled.swc$')
  consfn <- list.files(paste0('0401_gold163_all_soma_sort_pixelsize/',i),pattern='/*consensus.eswc_resampled.swc_sorted.swc$')
  autofn <- list.files(paste0('gold_163_all_soma_sort_s1_pixelsize/',i,'/auto_recons/'),pattern='/*_resampled.swc_sorted.swc$')
  procfn <- list.files(paste0('gold_163_all_soma_sort_s1_pixelsize/',i,'/processed/'),pattern='/*_resampled.swc_sorted.swc$')
  
  if(length(goldfn)>0){
    ingoldfn <- paste0('gold_163_all_soma_sort_s1_pixelsize/',i,'/',goldfn)
    outgoldfn <- paste0('gold_163_all_soma_sort_s1_pixelsize_radius/',i,'/',goldfn)
  }
  
  if(length(consfn)>0){
    inconsfn <- paste0('0401_gold163_all_soma_sort_pixelsize/',i,'/',consfn)
    outconsfn <- paste0('0401_gold163_all_soma_sort_pixelsize_radius/',i,'/',consfn)
  }
  
  if(length(autofn)>0){
    inautofn <- paste0('gold_163_all_soma_sort_s1_pixelsize/',i,'/auto_recons/',autofn)
    outautofn <- paste0('gold_163_all_soma_sort_s1_pixelsize_radius/',i,'/auto_recons/',autofn)
  }
  
  if(length(procfn)>0){
    inprocfn <- paste0('gold_163_all_soma_sort_s1_pixelsize/',i,'/processed/',procfn)
    outprocfn <- paste0('gold_163_all_soma_sort_s1_pixelsize_radius/',i,'/processed/',procfn)
  }
  
  dir.create(paste0('gold_163_all_soma_sort_s1_pixelsize_radius/',i))
  dir.create(paste0('gold_163_all_soma_sort_s1_pixelsize_radius/',i,'/processed'))
  dir.create(paste0('gold_163_all_soma_sort_s1_pixelsize_radius/',i,'/auto_recons'))
  dir.create(paste0('0401_gold163_all_soma_sort_pixelsize_radius/',i))
  
  xy <- psdf$x_y_pix_size[psdf$id==i]
  z <- psdf$z_pix_size[psdf$id==i]
  
  foreach(j = seq_along(ingoldfn)) %dopar% {
    outfn <- outgoldfn[j]
    tryCatch(system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",ingoldfn[j]," -o ",outfn," -p ",1/xy," ",1/xy," ",1/xy," 1")))
  }
  foreach(j = seq_along(inconsfn)) %dopar% {
    outfn <- outconsfn[j]
    tryCatch(system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",inconsfn[j]," -o ",outfn," -p ",1/xy," ",1/xy," ",1/xy," 1")))
  }
  foreach(j = seq_along(inautofn)) %dopar% {
    outfn <- outautofn[j]
    tryCatch(system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",inautofn[j]," -o ",outfn," -p ",1/xy," ",1/xy," ",1/xy," 1")))
  }
  foreach(j = seq_along(inprocfn)) %dopar% {
    outfn <- outprocfn[j]
    tryCatch(system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",inprocfn[j]," -o ",outfn," -p ",1/xy," ",1/xy," ",1/xy," 1")))
  }
}

parallel::stopCluster(cl)
