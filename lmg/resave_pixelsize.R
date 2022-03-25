
load('df_iq.Rdata')

# dir.create('../BigNeurongit/Data/gold166_wids')
for(i in 1:length(dfiq$ids)){
  print(substring(as.character(dfiq$Metadata_FileLocation[i]),9))
  strfrom <- substring(as.character(dfiq$Metadata_FileLocation[i]),9)
  strfrom1 <- strsplit(strfrom,'/')[7]
  strfrom2 <- strsplit(strfrom,'/')[8]  
  # file.copy(from=paste0('../BigNeurongit/Data/gold166/',strfrom1,'/',strfrom2),
            # to=paste0('../BigNeurongit/Data/gold166_wids/',dfiq$ids[i],'/',dfiq$ids[i],'.tif'))
  
}
