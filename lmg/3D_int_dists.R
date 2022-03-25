library(TTR)
library(EBImage)

load('shiny_app/subsetdata.Rdata')
load('shiny_app/groupsdf.Rdata')
ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)

data <- my_data

data$Correlation <- NULL
data$FocusScore <- NA
data$LocalFocusScore <- NULL
data$MADIntensity <- NA
data$MaxIntensity <- NA
data$MeanIntensity <- NA
data$SNR_mean <- NA
data$CNR_mean <- NA
data$MedianIntensity <- NA
data$MinIntensity <- NA
data$PercentMaximal <- NA
data$PercentMinimal <- NA
data$PowerLogLogSlope <- NULL
data$StdIntensity <- NA
data$ThresholdOtsu <- NULL
data$SNR_otsu <- NA
data$CNR_otsu <- NA
data$TotalArea <- NULL
data$TotalIntensity <- NULL
  
for(i in unique(ids))
{
  print(i)
  im <- readImage(paste0("../BigNeurongit/Data/gold166_wids_vols_tiff/",i,"/",i,".v3dpbd.tiff"))
  # swc <- read.csv2(paste0(gsub("gold_163_all_soma_sort_s1","gold_163_all_soma_sort_s1_int",groupsdf$paths[ids==i]),'.missing_branches_int.eswc'),sep=' ',header=F)
  # try(data$Correlation[ids==i] <- mean(as.numeric(unlist(acf(im,plot=F)[4:8]))[1:5]))
  # "Correlation."
  # multiplied = np.dot(self.levels[:, np.newaxis] + 1, self.levels[np.newaxis] + 1)
  # repeated = np.tile(multiplied[np.newaxis], (self.nobjects, 1, 1))
  # summed = (repeated * self.P).sum(2).sum(1)
  # h3 = (summed - self.mux * self.muy) / (self.sigmax * self.sigmay)
  # h3[np.isinf(h3)] = 0
  data$FocusScore[ids==i] <- sd(im)/mean(im)
  # try(data$LocalFocusScore[ids==i] <- mean(runSD(im)/runMean(im),na.rm=T))
  data$MADIntensity[ids==i] <- mad(im)
  im_max <- max(im)
  data$MaxIntensity[ids==i] <- im_max
  im_mean <- mean(im)
  noise_sd <- sd(im[im < im_mean])
  data$MeanIntensity[ids==i] <- im_mean
  data$SNR_mean[ids==i] <- mean(im[im > im_mean])/noise_sd
  data$CNR_mean[ids==i] <- abs(im_max-im_mean)/noise_sd
  data$MedianIntensity[ids==i] <- median(im)
  data$MinIntensity[ids==i] <- min(im)
  data$PercentMaximal[ids==i] <- sum(im==max(im))/length(im)
  data$PercentMinimal[ids==i] <- sum(im==min(im))/length(im)
  # spect <- spectrum(im,plot=F)
  # logm1 <- lm(log(spect$spec) ~ log(spect$freq))
  # data$PowerLogLogSlope[ids==i] <- as.numeric(coef(logm1)[2])
  data$StdIntensity[ids==i] <-sd(im)
  im_otsu <- otsu(as.Image(as.numeric(im)))
  otsu_noise_sd <- sd(im[im < im_otsu])
  data$ThresholdOtsu[ids==i] <- im_otsu
  data$SNR_otsu[ids==i] <- mean(im[im > im_otsu])/otsu_noise_sd
  data$CNR_otsu[ids==i] <- abs(im_max-im_otsu)/otsu_noise_sd
  # data$TotalArea[ids==i] <- length(im)
  # data$TotalIntensity[ids==i] <- sum(im)
}

sum(complete.cases(data))

my_data <- do.call(data.frame,                      # Replace Inf in data by NA
                   lapply(data,
                          function(x) replace(x, is.infinite(x), NA)))

save(my_data,file='shiny_app/subsetdata3D.Rdata')
