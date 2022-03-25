library(nat)

for(i in unique(paste0(as.character(dftotgrp$swc_file),'_resampled.swc'))){
  print(i)
  nrnlisttesti <- read.neurons(i,neuronnames=i)
}
# nrnlisttest <- read.neurons(unique(paste0(as.character(dftotgrp$swc_file),'_resampled.swc')),neuronnames=unique(paste0(as.character(dftotgrp$swc_file),'_resampled.swc')))
# save(nrnlisttest,file="nrnlist.Rdata")

# write.neurons(nrnlisttest,dir='nrnlist.zip',format='swc')

load("nrnlist.Rdata")
load("dftotgrp.Rdata")

nmnrnlist <- subset(nrnlisttest,rval="names")
annrnlist <- subset(nrnlisttest, nmnrnlist %in% paste0(dftotgrp$swc_file[dftotgrp$algorithm=="Annotated"],'_resampled.swc'))

# save(annrnlist, file='annrnlist.Rdata')

nmannrnlist <- subset(annrnlist,rval="names")


## nmapply example
# flip first neuron in X, second in Y and 3rd in Z
#xform for transformation
xyzflip=nmapply(mirror, kcs20[1:3], mirrorAxis = c("X","Y","Z"),
                mirrorAxisSize=c(400,20,30))
open3d()
plot3d(kcs20[1:3])
plot3d(xyzflip)



library(nat.nblast)
library(dendroextras)

nb <- nblast_allbyall(annrnlist)
hclust <- nhclust(scoremat = nb)
# plot(hclust)
dend <- colour_clusters(hclust, k=6, groupLabels=T)
labels(dend)=sapply(strsplit(nmannrnlist,'/'), "[", 8)
dfdt <- data.frame(ids=dftotgrp$ids,dataset=dftotgrp$dataset)
dfdt <- dfdt[!duplicated(dfdt), ]
labels(dend)=paste0(labels(dend),'_',dfdt$dataset)
par(cex=0.3, mar=c(5, 4, 4, 30))
plot(dend,horiz=T)
# library(ggdendro)
# ggdendrogram(dend, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")

# if (!require("devtools")) install.packages("devtools")
# devtools::install_github("natverse/neuromorphr")
library(neuromorphr)
