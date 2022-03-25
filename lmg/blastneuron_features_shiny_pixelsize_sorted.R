library(ggplot2)

# load('shiny_app/alldata.Rdata')
# load('shiny_app/subsetdata.Rdata')
# data <- my_data
# load('shiny_app/dftotgrp.Rdata')
# load('shiny_app/groupsdf.Rdata')
# ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)

# QCdata <- read.csv('../BigNeurongit/Data/gold166/QC_Image.csv')
load('df_iq2.Rdata')
dfiqdatasets <- data.frame(ids=dfiq$ids,dataset=sapply(strsplit(as.character(dfiq$Metadata_FileLocation),'/'), "[", 9))
load('df_iq.Rdata')
dfiq <- merge(dfiq,dfiqdatasets,by='ids')


load('df_distsall.Rdata') 

gold <- read.csv('features_gold_pixelsize_sorted.csv')
consensus <- read.csv('features_consensus_pixelsize_sorted.csv')
auto <- read.csv('features_auto_pixelsize_sorted.csv')
processed <- read.csv('features_processed_pixelsize_sorted.csv')

datanew <- rbind(gold,consensus,auto,processed)

# goldswcfile <- character()
# for(i in 1:length(gold$swc_file))
# {
#   goldswcfile[i] <- gsub("_pixelsize","",gold$swc_file[i])
#   goldswcfile[i] <- gsub("_resampled.swc","", goldswcfile[i])
# }
# goldids <- sapply(strsplit(as.character(gold$swc_file),'/'), "[", 8)

psswcfile <- character()
for(i in 1:length(datanew$swc_file))
{
  psswcfile[i] <- gsub("_pixelsize","",datanew$swc_file[i])
  psswcfile[i] <- gsub("_resampled.swc_sorted.swc","", psswcfile[i])
  psswcfile[i] <- gsub("consensus.swc","consensus.eswc", psswcfile[i])
}

datanew$swc_file <- as.factor(psswcfile)

dfall$swc_file <- as.factor(paste0('/home/linusmg/Data/bigneuron/Both_GS_and_auto/',substring(dfall$input2,2)))

bn_dists <- merge(datanew,dfall,by='swc_file',all=T)
bn_dists[is.na(bn_dists)] <- 0

bn_dists$ids <- sapply(strsplit(as.character(bn_dists$swc_file),'/'), "[", 8)
bn_dists$algorithm[bn_dists$algorithm=="0"] <- "Annotated"
bn_dists$algorithm[bn_dists$algorithm=='consensus'] <- "Consensus"

bn_dists <- do.call(data.frame,                      # Replace Inf in data by NA
                   lapply(bn_dists,
                          function(x) replace(x, is.infinite(x), NA)))

bn_dists_qc <- merge(bn_dists,dfiq,by='ids')

bn_dists_qc$paths <- bn_dists_qc$swc_file
# bn_dists_qc$dataset <- sapply(strsplit(as.character(bn_dists_qc$Metadata_FileLocation),'/'), "[", 9)
bn_dists_qc$group <- sapply(strsplit(as.character(bn_dists_qc$paths),'/'), "[", 9)
bn_dists_qc$group[bn_dists_qc$group=="processed"] <- "Processed"
bn_dists_qc$group[bn_dists_qc$group=="auto_recons"] <- "Auto"
bn_dists_qc$group[bn_dists_qc$group=="consensus.eswc"] <- "Consensus"
bn_dists_qc$group[bn_dists_qc$group!="Consensus" & bn_dists_qc$group!="Auto" & bn_dists_qc$group!="Processed"] <- "Gold_Standard"

# dftotgrp <- dftotgrp[unique(dftotgrp$swc_file),]
# bn_dists_groups <- merge(bn_dists,dftotgrp,by='swc_file')
  
# df <- gold
# df$num_nodes <- NULL
# datanew <- df[,2:35]
# datanew <- remove_missing(datanew)

sum(complete.cases(bn_dists_qc))

groupsdf <- data.frame(dataset=bn_dists_qc$dataset,
                       group=bn_dists_qc$group,
                       algorithm=bn_dists_qc$algorithm,
                       paths=bn_dists_qc$paths)

for (i in 1:length(groupsdf$paths)){
  groupsdf$algorithm[i] <- gsub(".*?_app1", "app1", groupsdf$algorithm[i])
  groupsdf$algorithm[i] <- gsub(".*?_app2", "app2", groupsdf$algorithm[i])
}

data <- bn_dists_qc[,c(4:37,41:47,60:70,72:75)]

data <- plyr::rename(data,c("esa1t2"="entire-structure-average (from neuron 1 to 2)"))
data <- plyr::rename(data,c("esa2t1"="entire-structure-average (from neuron 2 to 1)"))
data <- plyr::rename(data,c("ab_esa"="average of bi-directional entire-structure-averages"))
data <- plyr::rename(data,c("dsa"="different-structure-average"))
data <- plyr::rename(data,c("pds1t2"="percent of different-structure (from neuron 1 to 2)"))
data <- plyr::rename(data,c("pds2t1"="percent of different-structure (from neuron 2 to 1)"))
data <- plyr::rename(data,c("a_pds"="percent of different-structure"))

data <- plyr::rename(data,c("ImageQuality_Correlation_DNA_20"="Correlation"))
data <- plyr::rename(data,c("ImageQuality_FocusScore_DNA"="FocusScore"))
data <- plyr::rename(data,c("ImageQuality_LocalFocusScore_DNA_20"="LocalFocusScore"))
data <- plyr::rename(data,c("ImageQuality_MADIntensity_DNA"="MADIntensity"))
data <- plyr::rename(data,c("ImageQuality_MaxIntensity_DNA"="MaxIntensity"))
data <- plyr::rename(data,c("ImageQuality_MeanIntensity_DNA"="MeanIntensity"))
data <- plyr::rename(data,c("ImageQuality_MedianIntensity_DNA"="MedianIntensity"))
data <- plyr::rename(data,c("ImageQuality_MinIntensity_DNA"="MinIntensity"))
data <- plyr::rename(data,c("ImageQuality_PercentMaximal_DNA"="PercentMaximal"))
data <- plyr::rename(data,c("ImageQuality_PercentMinimal_DNA"="PercentMinimal"))
data <- plyr::rename(data,c("ImageQuality_PowerLogLogSlope_DNA"="PowerLogLogSlope"))
data <- plyr::rename(data,c("ImageQuality_StdIntensity_DNA"="StdIntensity"))
data <- plyr::rename(data,c("ImageQuality_ThresholdOtsu_DNA_2W"="ThresholdOtsu"))
data <- plyr::rename(data,c("ImageQuality_TotalArea_DNA"="TotalArea"))
data <- plyr::rename(data,c("ImageQuality_TotalIntensity_DNA"="TotalIntensity"))


ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)

data$Correlation_swc <- NA
data$FocusScore_swc <- NA
data$LocalFocusScore_swc <- NA
data$MADIntensity_swc <- NA
data$MaxIntensity_swc <- NA
data$MeanIntensity_swc <- NA
data$MedianIntensity_swc <- NA
data$MinIntensity_swc <- NA
data$PercentMaximal_swc <- NA
data$PercentMinimal_swc <- NA
data$PowerLogLogSlope_swc <- NA
data$StdIntensity_swc <- NA
data$ThresholdOtsu_swc <- NA
data$TotalArea_swc <- NA
data$TotalIntensity_swc <- NA

library(TTR)
library(EBImage)

for(i in 1:length(groupsdf$paths))
{
  print(i)
  # if(is.na(groupsdf$paths[i])==FALSE){
  if(file.exists(paste0(gsub("gold_163_all_soma_sort_s1","gold_163_all_soma_sort_s1_int",groupsdf$paths[i]),'.missing_branches_int.eswc'))){
    swc <- read.csv2(paste0(gsub("gold_163_all_soma_sort_s1","gold_163_all_soma_sort_s1_int",groupsdf$paths[i]),'.missing_branches_int.eswc'),sep=' ',header=F)
    try(data$Correlation_swc[i] <- mean(na.omit(as.numeric(unlist(acf(na.omit(as.numeric(swc$V12)),plot=F,na.action=na.pass)[4:8]))[1:5])))
    data$FocusScore_swc[i] <- sd(na.omit(as.numeric(swc$V12)))/mean(na.omit(as.numeric(swc$V12)))
    try(data$LocalFocusScore_swc[i] <- mean(runSD(na.omit(as.numeric(swc$V12)))/runMean(na.omit(as.numeric(swc$V12))),na.rm=T))
    data$MADIntensity_swc[i] <- mad(na.omit(as.numeric(swc$V12)))
    data$MaxIntensity_swc[i] <- max(na.omit(as.numeric(swc$V12)))
    data$MeanIntensity_swc[i] <- mean(na.omit(as.numeric(swc$V12)))
    data$MedianIntensity_swc[i] <- median(na.omit(as.numeric(swc$V12)))
    data$MinIntensity_swc[i] <- min(na.omit(as.numeric(swc$V12)))
    data$PercentMaximal_swc[i] <- sum(na.omit(as.numeric(swc$V12))==max(na.omit(as.numeric(swc$V12))))/length(na.omit(as.numeric(swc$V12)))
    data$PercentMinimal_swc[i] <- sum(na.omit(as.numeric(swc$V12))==min(na.omit(as.numeric(swc$V12))))/length(na.omit(as.numeric(swc$V12)))
    try(spect <- spectrum(na.omit(as.numeric(swc$V12)),plot=F,na.action=na.pass))
    try(logm1 <- lm(log(spect$spec) ~ log(spect$freq)))
    try(data$PowerLogLogSlope_swc[i] <- as.numeric(coef(logm1)[2]))
    try(data$StdIntensity_swc[i] <-sd(na.omit(as.numeric(swc$V12))))
    try(data$ThresholdOtsu_swc[i] <- otsu(as.Image(as.numeric(swc$V12,na.rm=T)),range=c(0,max(na.omit(as.numeric(swc$V12)),na.rm=T))))
    data$TotalArea_swc[i] <- length(na.omit(as.numeric(swc$V12)))
    data$TotalIntensity_swc[i] <- sum(na.omit(as.numeric(swc$V12)))
  }
}

databak <- data
data <- data[,c(1:20,34:71)]

groupsdf$ids <- ids
pixelsize <- read.csv("scaling_gold.csv")
pixelsize <- dplyr::rename(pixelsize,ids=id)
pixeldf <- merge(groupsdf,pixelsize,by="ids")
data$xy_pixel_size <- pixeldf$x_y_pix_size
data$z_pixel_size <- pixeldf$z_pix_size

## Add centripetal bias and SIP scale
# pathsgs <- read.csv("pathsgs.csv",header=F)
# SIP_scale <- read.csv("SIP_scale.csv",header=F)
# Centrip_bias_k <- read.csv("k.csv",header=F)
# L <- read.csv("L.csv",header=F)
# psswcfilegs <- character()
# for(i in 1:length(pathsgs$V1))
# {
#   psswcfilegs[i] <- gsub("_pixelsize","",pathsgs$V1[i])
#   psswcfilegs[i] <- gsub("_resampled.swc","", psswcfilegs[i])
#   psswcfilegs[i] <- gsub("consensus.swc","consensus.eswc", psswcfilegs[i])
#   data$SIP_scale[groupsdf$paths == psswcfilegs[i]] <- SIP_scale$V1[i] 
#   data$Centrip_bias_k[groupsdf$paths == psswcfilegs[i]] <- Centrip_bias_k$V1[i] 
#   data$L[groupsdf$paths == psswcfilegs[i]] <- L$V1[i] 
#   # print(data$total_length[groupsdf$paths == psswcfilegs[i]] - L$V1[i])
#   # print(data$total_length[groupsdf$paths == psswcfilegs[i]])
#   # print(L$V1[i])
# }


my_data <- do.call(data.frame,                      # Replace Inf in data by NA
                   lapply(data,
                          function(x) replace(x, is.infinite(x), NA)))



save(my_data,file='shiny_app/subsetdata.Rdata')

metadata <- read.csv("groups_metadata.csv",na.strings="")
library(plyr)
groupsdf <- join(groupsdf, metadata, by="dataset")

# groupsdf$algorithm[groupsdf$algorithm=="0"] <- "Annotated"

save(groupsdf,file='shiny_app/groupsdf.Rdata')

ugroups <- groupsdf[groupsdf$group=="Gold_Standard",]
ugroups$paths <- sapply(strsplit(as.character(ugroups$paths),'/'), "[", 9)
ugroups$paths <- paste0(ugroups$paths,'_resampled.swc.radius.swc_sorted.swc')
# ugroups$paths <- paste0('isotropic_swcs/',ugroups$paths)
Pllist <- list.files("isotropic_2D_swcs/")
ugroups$Planar <- '3D'
ugroups$Planar[ugroups$paths %in% Pllist] <- '2D'  
write.csv(ugroups,file='metadatags.csv')

