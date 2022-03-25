library(foreach)

cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)

psdf <- read.csv('scaling_gold.csv')

dir.create("gold_163_all_soma_sort_s1_restim")
dir.create("0401_gold163_all_soma_sort_restim")

# psdf <- psdf[psdf$id==99,]

for(i in psdf$id){
  goldfn <- list.files(paste0('gold_163_all_soma_sort_s1/',i),pattern='/*_resampled.swc$')
  consfn <- list.files(paste0('0401_gold163_all_soma_sort/',i),pattern='/*consensus.eswc_resampled.swc_sorted.swc$')
  autofn <- list.files(paste0('gold_163_all_soma_sort_s1/',i,'/auto_recons/'),pattern='/*_resampled.swc$')
  procfn <- list.files(paste0('gold_163_all_soma_sort_s1/',i,'/processed/'),pattern='/*_resampled.swc$')
  
  if(length(goldfn)>0){
    ingoldfn <- paste0('gold_163_all_soma_sort_s1/',i,'/',goldfn)
    outgoldfn <- paste0('gold_163_all_soma_sort_s1_restim/',i,'/',goldfn)
  }
  
  if(length(consfn)>0){
    inconsfn <- paste0('0401_gold163_all_soma_sort/',i,'/',consfn)
    outconsfn <- paste0('0401_gold163_all_soma_sort_restim/',i,'/',consfn)
  }
  
  if(length(autofn)>0){
    inautofn <- paste0('gold_163_all_soma_sort_s1/',i,'/auto_recons/',autofn)
    outautofn <- paste0('gold_163_all_soma_sort_s1_restim/',i,'/auto_recons/',autofn)
  }
  
  if(length(procfn)>0){
    inprocfn <- paste0('gold_163_all_soma_sort_s1/',i,'/processed/',procfn)
    outprocfn <- paste0('gold_163_all_soma_sort_s1_restim/',i,'/processed/',procfn)
  }
  
  dir.create(paste0('gold_163_all_soma_sort_s1_restim/',i))
  dir.create(paste0('gold_163_all_soma_sort_s1_restim/',i,'/processed'))
  dir.create(paste0('gold_163_all_soma_sort_s1_restim/',i,'/auto_recons'))
  dir.create(paste0('0401_gold163_all_soma_sort_restim/',i))
  
  xy <- psdf$x_y_pix_size[psdf$id==i]
  z <- psdf$z_pix_size[psdf$id==i]
  
  foreach(j = seq_along(ingoldfn)) %dopar% {
    outfn <- outgoldfn[j]
    header <- readLines(ingoldfn[j])[1:3]
    infile <- read.csv(ingoldfn[j],header=F,skip = 3,sep=" ")
    infile$V2 <- 3
    infile$V2[infile$V7==-1] <- 1
    infile$V5 <- infile$V5*z/xy
    writeLines(header,outfn)
    write.table(infile,file=outfn,append=T,col.names = F,row.names = F)
    # system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",ingoldfn[j]," -o ",outfn," -p 1 1 ",z/xy," 1"))
  }
  foreach(j = seq_along(inconsfn)) %dopar% {
    outfn <- outconsfn[j]
    header <- readLines(inconsfn[j])[1:3]
    infile <- read.csv(inconsfn[j],header=F,skip = 3,sep=" ")
    infile$V2 <- 3
    infile$V2[infile$V7==-1] <- 1
    infile$V5 <- infile$V5*z/xy
    writeLines(header,outfn)
    write.table(infile,file=outfn,append=T,col.names = F,row.names = F)
    # system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",inconsfn[j]," -o ",outfn," -p 1 1 ",z/xy," 1"))
  }
  foreach(j = seq_along(inautofn)) %dopar% {
    outfn <- outautofn[j]
    header <- readLines(inautofn[j])[1:3]
    infile <- read.csv(inautofn[j],header=F,skip = 3,sep=" ")
    infile$V2 <- 3
    infile$V2[infile$V7==-1] <- 1
    infile$V5 <- infile$V5*z/xy
    writeLines(header,outfn)
    write.table(infile,file=outfn,append=T,col.names = F,row.names = F)
    # system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",inautofn[j]," -o ",outfn," -p 1 1 ",z/xy," 1"))
  }
  foreach(j = seq_along(inprocfn)) %dopar% {
    outfn <- outprocfn[j]
    header <- readLines(inprocfn[j])[1:3]
    infile <- read.csv(inprocfn[j],header=F,skip = 3,sep=" ")
    infile$V2 <- 3
    infile$V2[infile$V7==-1] <- 1
    infile$V5 <- infile$V5*z/xy
    writeLines(header,outfn)
    write.table(infile,file=outfn,append=T,col.names = F,row.names = F)
    # system(paste0("vaa3d -x IVSCC_scaling -f scale -i ",inprocfn[j]," -o ",outfn," -p 1 1 ",z/xy," 1"))
  }
}

parallel::stopCluster(cl)
