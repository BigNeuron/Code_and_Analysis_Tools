library(TTR)
library(EBImage)

load('shiny_app/subsetdata_v2_3d.Rdata')
load('shiny_app/groupsdf.Rdata')
ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)

data <- my_data

data$Correlation_swc <- NULL
data$FocusScore_swc <- NA
data$LocalFocusScore_swc <- NULL
data$MADIntensity_swc <- NA
data$MaxIntensity_swc <- NA
data$MeanIntensity_swc <- NA
data$MedianIntensity_swc <- NA
data$MinIntensity_swc <- NA
data$PercentMaximal_swc <- NA
data$PercentMinimal_swc <- NA
data$PowerLogLogSlope_swc <- NULL
data$StdIntensity_swc <- NA
data$ThresholdOtsu_swc <- NA
data$TotalArea_swc <- NULL
data$TotalIntensity_swc <- NULL
data$SNR_mean_swc <- NA
data$CNR_mean_swc <- NA
data$SNR_otsu_swc <- NA
data$CNR_otsu_swc <- NA

for(i in 1:length(groupsdf$paths))
{
  print(i)
  if(is.na(groupsdf$paths[i])==FALSE){
    filename<- gsub("/home/linusmg/Data/","/Users/linusmg/Desktop/",as.character(groupsdf$paths[i]))
    filename <- gsub("gold_163_all_soma_sort_s1","gold_163_all_soma_sort_s1_int",filename)
    swc <- read.csv2(paste0(filename,'.missing_branches_int.eswc'),sep=' ',header=F)
  # try(data$Correlation_swc[i] <- mean(as.numeric(unlist(acf(as.numeric(swc$V12),plot=F)[4:8]))[1:5]))
    data$FocusScore_swc[i] <- sd(as.numeric(swc$V12),na.rm=T)^2/mean(as.numeric(swc$V12),na.rm=T)
    # try(data$LocalFocusScore_swc[i] <- mean(runSD(as.numeric(swc$V12))/runMean(as.numeric(swc$V12)),na.rm=T))
    data$MADIntensity_swc[i] <- mad(as.numeric(swc$V12),na.rm=T)
    data$MaxIntensity_swc[i] <- max(as.numeric(swc$V12),na.rm=T)
    data$MeanIntensity_swc[i] <- mean(as.numeric(swc$V12),na.rm=T)
    data$MedianIntensity_swc[i] <- median(as.numeric(swc$V12),na.rm=T)
    data$MinIntensity_swc[i] <- min(as.numeric(swc$V12),na.rm=T)
    data$PercentMaximal_swc[i] <- sum(as.numeric(swc$V12)==max(as.numeric(swc$V12),na.rm=T),na.rm=T)/length(as.numeric(swc$V12))
    data$PercentMinimal_swc[i] <- sum(as.numeric(swc$V12)==min(as.numeric(swc$V12),na.rm=T),na.rm=T)/length(as.numeric(swc$V12))
    # try(spect <- spectrum(as.numeric(swc$V12,na.rm=T),plot=F))
    # logm1 <- lm(log(spect$spec) ~ log(spect$freq))
    # data$PowerLogLogSlope_swc[i] <- as.numeric(coef(logm1)[2])
    data$StdIntensity_swc[i] <-sd(as.numeric(swc$V12),na.rm=T)
    data$SNR_mean_swc[i] <- mean(as.numeric(swc$V12)[as.numeric(swc$V12) > data$MeanIntensity_swc[i]],na.rm=T)/sd(as.numeric(swc$V12)[as.numeric(swc$V12) < data$MeanIntensity_swc[i]],na.rm=T)
    data$CNR_mean_swc[i] <- abs(data$MaxIntensity_swc[i]-data$MeanIntensity_swc[i])/sd(as.numeric(swc$V12)[as.numeric(swc$V12) < data$MeanIntensity_swc[i]],na.rm=T)
    try(data$ThresholdOtsu_swc[i] <- otsu(as.Image(as.numeric(swc$V12,na.rm=T)),range=c(0,255)))
    data$SNR_otsu_swc[i] <- mean(as.numeric(swc$V12)[as.numeric(swc$V12) > data$ThresholdOtsu_swc[i]],na.rm=T)/sd(as.numeric(swc$V12)[as.numeric(swc$V12) < data$ThresholdOtsu_swc[i]],na.rm=T)
    data$CNR_otsu_swc[i] <- abs(data$MaxIntensity_swc[i]-data$ThresholdOtsu_swc[i])/sd(as.numeric(swc$V12)[as.numeric(swc$V12) < data$ThresholdOtsu_swc[i]],na.rm=T)
    # data$TotalArea_swc[i] <- length(as.numeric(swc$V12))
    # data$TotalIntensity_swc[i] <- sum(as.numeric(swc$V12))
  }
}

sum(complete.cases(data))

my_data <- do.call(data.frame,                      # Replace Inf in data by NA
                   lapply(data,
                          function(x) replace(x, is.infinite(x), NA)))

save(my_data,file='shiny_app/subsetdata_v3.Rdata')
