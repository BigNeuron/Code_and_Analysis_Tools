library(TTR)
library(EBImage)

load('shiny_app/subsetdata_v2.Rdata')
load('shiny_app/groupsdf.Rdata')
ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)

data <- my_data

data$Correlation <- NULL
data$FocusScore <- NA
data$LocalFocusScore <- NULL
data$MADIntensity <- NA
data$MaxIntensity <- NA
data$MeanIntensity <- NA
data$MedianIntensity <- NA
data$MinIntensity <- NA
data$PercentMaximal <- NA
data$PercentMinimal <- NA
data$PowerLogLogSlope <- NULL
data$StdIntensity <- NA
data$ThresholdOtsu <- NA
data$TotalArea <- NULL
data$TotalIntensity <- NULL
data$SNR_mean <- NA
data$CNR_mean <- NA
data$SNR_otsu <- NA
data$CNR_otsu <- NA

for(i in unique(ids))
{
  print(i)
  # if(is.na(groupsdf$paths[ids==i])==FALSE){
    IQ <- read.csv(paste0("../BigNeurongit/Data/gold163_IQ/",i,"/",i,".v3dpbd.ImageQuality.csv"),header=T)
  # try(data$Correlation[ids==i] <- mean(as.numeric(unlist(acf(as.numeric(swc$V12),plot=F)[4:8]))[1:5]))
    data$FocusScore[ids==i] <- IQ$FocusScore
    # try(data$LocalFocusScore[ids==i] <- mean(runSD(as.numeric(swc$V12))/runMean(as.numeric(swc$V12)),na.rm=T))
    data$MADIntensity[ids==i] <- IQ$MADIntensity
    data$MaxIntensity[ids==i] <- IQ$MaxIntensity
    data$MeanIntensity[ids==i] <- IQ$MeanIntensity
    data$MedianIntensity[ids==i] <- IQ$MedianIntensity
    data$MinIntensity[ids==i] <- IQ$MinIntensity
    data$PercentMaximal[ids==i] <- IQ$PercentMaximal
    data$PercentMinimal[ids==i] <- IQ$PercentMinimal
    # try(spect <- spectrum(as.numeric(swc$V12,na.rm=T),plot=F))
    # logm1 <- lm(log(spect$spec) ~ log(spect$freq))
    # data$PowerLogLogSlope[ids==i] <- as.numeric(coef(logm1)[2])
    data$StdIntensity[ids==i] <-IQ$StdIntensity
    data$SNR_mean[ids==i] <- IQ$SNR_mean
    data$CNR_mean[ids==i] <- IQ$CNR_mean
    data$ThresholdOtsu[ids==i] <- IQ$ThresholdOtsu
    data$SNR_otsu[ids==i] <- IQ$SNR_otsu
    data$CNR_otsu[ids==i] <- IQ$CNR_otsu
    # data$TotalArea[ids==i] <- length(as.numeric(swc$V12))
    # data$TotalIntensity[ids==i] <- sum(as.numeric(swc$V12))
  # }
}

sum(complete.cases(data))

my_data <- do.call(data.frame,                      # Replace Inf in data by NA
                   lapply(data,
                          function(x) replace(x, is.infinite(x), NA)))

save(my_data,file='shiny_app/subsetdata_v2_3d.Rdata')
