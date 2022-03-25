scaling <- read.csv("scaling_gold.csv")


for(i in scaling$id){
  iq <- read.csv(paste0("../../BigNeurongit/Data/gold163_IQ_3/",i,"/",i,".v3dpbd.ImageQuality.csv"))
  scaling$SNR_mean[scaling$id == i] <- iq$SNR_mean
  scaling$mean[scaling$id == i] <- iq$MeanIntensity
  scaling$SNR_otsu[scaling$id == i] <- iq$SNR_otsu
  scaling$otsu[scaling$id == i] <- iq$ThresholdOtsu
  scaling$maxint[scaling$id == i] <- iq$MinIntensity
}

write.csv(scaling, file="scaling_gold_SNR.csv")
