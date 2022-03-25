library(nat)

pathsgs <- read.csv("pathsgs.csv",header=F)

annrnlist <- read.neurons(as.character(pathsgs$V1),neuronnames=gsub("_resampled.swc","",pathsgs$V1))
save(annrnlist, file='annrnlist.Rdata')


###############
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
