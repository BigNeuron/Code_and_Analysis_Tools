load('shiny_app/subsetdata.Rdata')
load('shiny_app/groupsdf.Rdata')

alldata <- cbind(groupsdf,my_data)

alldata$paths <- sub("gold_163_all_soma_sort_s1","gold_163_all_soma_sort_s1_pixelsize",alldata$paths)
alldata$paths <- sub("0401_gold163_all_soma_sort","0401_gold163_all_soma_sort_pixelsize",alldata$paths)
alldata$paths <- sub(".swc$",".swc_resampled.swc",alldata$paths)
alldata$paths <- sub("consensus..swc_resampled.swc","consensus.eswc_resampled.swc_sorted.swc",alldata$paths)

write.csv(alldata,file='alldata.csv')
write.csv(alldata$paths,file='paths.csv',row.names = F)
