library(ggplot2)
library(plyr)
load('df_dists.Rdata')
load('df_blastneuron.Rdata')
load('df_iq.Rdata')

dfall$algorithm[dfall$algorithm=="consensus"] <- "Consensus"

df <- merge(dff,dfall, by=c("ids","algorithm"))#, no.pdups=T)
df_gs <- subset(dff,dff$group=="Gold_Standard")
df_gs$input1 <- 'GS'
df_gs$input2 <- 'GS'
df_gs$esa1t2 <- 0
df_gs$esa2t1 <- 0
df_gs$ab_esa <- 0
df_gs$dsa <- 0
df_gs$pds1t2 <- 0
df_gs$pds2t1 <- 0
df_gs$a_pds <- 0

df <- rbind(df,df_gs)

dftot <- merge(df,dfiq, by="ids")#, no.pdups=T)

dftot <- rename(dftot,c("esa1t2"="entire-structure-average (from neuron 1 to 2)"))
dftot <- rename(dftot,c("esa2t1"="entire-structure-average (from neuron 2 to 1)"))
dftot <- rename(dftot,c("ab_esa"="average of bi-directional entire-structure-averages"))
dftot <- rename(dftot,c("dsa"="different-structure-average"))
dftot <- rename(dftot,c("pds1t2"="percent of different-structure (from neuron 1 to 2)"))
dftot <- rename(dftot,c("pds2t1"="percent of different-structure (from neuron 2 to 1)"))
dftot <- rename(dftot,c("a_pds"="percent of different-structure"))

####
# dftot <- df 
# dfgroups <- data.frame(ids=dff$ids,group=dff$group,algorithm=dff$algorithm,dataset=dff$dataset)
# dftot <- merge(dftot,dfgroups,by="ids")

dftot$ImageNumber <-NULL
dftot$Width_DNA <-NULL
dftot$Series_DNA <-NULL
dftot$ModuleError_01Images <-NULL
dftot$ModuleError_02Metadata <-NULL
dftot$ModuleError_03NamesAndTypes <-NULL
dftot$ModuleError_04Groups <-NULL
dftot$ModuleError_05MeasureImageQuality <-NULL
dftot$Metadata_Series <-NULL
dftot$Metadata_Frame <-NULL
dftot$Channel_DNA <- NULL
dftot$ExecutionTime_01Images <- NULL
dftot$ExecutionTime_02Metadata <- NULL
dftot$ExecutionTime_03NamesAndTypes <- NULL
dftot$ExecutionTime_04Groups <- NULL
dftot$ExecutionTime_05MeasureImageQuality <- NULL
dftot$Frame_DNA <- NULL
dftot$Group_Index <- NULL
dftot$Group_Number <- NULL
dftot$Height_DNA <- NULL
dftot$Scaling_DNA <- NULL
dftot$Metadata_FileLocation <-NULL
dftot$Channel_DNA <- NULL


#dftotgs <- dftot[dftot$group=="Gold_Standard",]
dftot <- remove_missing(dftot)

#dftot <- rbind(dftot,dftotgs)


# dftot$moment2  <- NULL
# dftot$moment4 <- NULL
# dftot$moment6 <- NULL
# dftot$moment8 <- NULL
# dftot$moment10 <- NULL
# dftot$moment12 <- NULL
# dftot$esa1t2 <- NULL
# dftot$esa2t1 <- NULL
# dftot$pds1t2 <- NULL
# dftot$pds2t1 <- NULL

#dftot <- dftot[dftot$group!='processed',]

#load('outliers.Rdata')
#dftot <- dftot[-outliers,]

dftotgrp <- dftot
groupsdf <- data.frame(dataset=dftotgrp$dataset,group=dftotgrp$group,algorithm=dftotgrp$algorithm,paths=dftotgrp$swc_file)
save(groupsdf,file='shiny_app/groupsdf.Rdata')
dftot <- dftot[sapply(dftot,is.numeric)]

nnames <- list()
for(i in 1:58){
  nnames[i] = names(dftot)[i]
  if (i > 42){
    nnames[i] = strsplit(names(dftot)[i],'_')[[1]][2]
  }
  names(dftot)[i] <- nnames[i]
}

data <- dftot[,-1]
data <- data[, sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
save(data,file='shiny_app/alldata.Rdata')

#data <- data[,c(2,5,7,8,9,11,13.16,17,36,37,38,40,41,42)]
data <- data[,-c(21:33)]
data <- data[, sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
save(data,file='shiny_app/subsetdata.Rdata')

######################################
library(ggbiplot)
pca <- prcomp(data,
              center = TRUE,
              scale. = TRUE)

print(pca)
plot(pca)
summary(pca)

################
groups <- dftotgrp$dataset
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              var.axes = FALSE,
              circle = FALSE)
#g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'vertical',
               legend.position = 'none') +
  theme_minimal() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=17)) +
  theme(axis.title.y = element_text(size=17)) +
  theme(title = element_text(size=20)) +
  #ylim(-10,10) +
  #xlim(-15,5) +
  labs(title = 'Gold166 - SWC features + IQ + Distance')
print(g)
library(plotly)
ggplotly(g)

#################################
groups <- dftotgrp$algorithm
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              var.axes = FALSE,
              circle = FALSE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'vertical',
               legend.position = 'right') +
  theme_minimal() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=17)) +
  theme(axis.title.y = element_text(size=17)) +
  theme(title = element_text(size=20)) +
  #xlim(-7,5) +
  labs(title = 'Gold166 - SWC features + IQ + Distance')
print(g)

#################################
groups <- dftotgrp$group
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              var.axes = FALSE,
              circle = FALSE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'vertical',
               legend.position = 'right') +
  theme_minimal() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=17)) +
  theme(axis.title.y = element_text(size=17)) +
  theme(title = element_text(size=20)) +
  #xlim(-7,5) +
  labs(title = 'Gold166 - SWC features + IQ + Distance')
print(g)

#################################
library(ggfortify)
library(plotly)
#dftotgrp$ImageQuality_FocusScore_IQ

g <- autoplot(pca, data = dftotgrp, colour = 'group',#'ImageQuality_FocusScore_IQ',#'ImageQuality_PercentMaximal_IQ',
         loadings = TRUE,
         loadings.label = TRUE,
         ellipse = TRUE) +
  labs(title = 'Gold166 - SWC features + IQ + Distance')

g
ggplotly(g)

##########################################################################################
library(Rtsne)
groups <- dftotgrp$dataset
## Curating the database for analysis with both t-SNE and PCA
## for plotting## Executing the algorithm on curated data

colors = rainbow(length(unique(groups)))
names(colors) = unique(groups)

## Executing the algorithm on curated data
tsne <- Rtsne(data, dims = 2, perplexity=40, verbose=TRUE, max_iter = 500, check_duplicates=FALSE, num_threads=6)
#save(tsne,file='tsne_all.Rdata')

tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = groups)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col)) +
  #xlim(-.03,.025) +
  #ylim(-0.05,0.05) +
  theme_minimal()

################
groups <- dftotgrp$algorithm
tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = groups)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col)) +
  #xlim(-.03,.025) +
  #ylim(-0.05,0.05) +
  theme_minimal()

################
groups <- dftotgrp$group
tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = groups)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col)) +
  #xlim(-.03,.025) +
  #ylim(-0.05,0.05) +
  theme_minimal()
