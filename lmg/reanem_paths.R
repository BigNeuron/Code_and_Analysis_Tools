df <- read.csv("paths.csv",header=F)

df$V1 <- gsub("consensus.eswc_resampled.swc_sorted.swc","consensus.swc",df$V1)
df$V1[grep("processed",df$V1)] <- gsub("resampled.swc","resampled.swc_sorted.swc",df$V1[grep("processed",df$V1)])
df$V1[grep("auto_recons",df$V1)] <- gsub("resampled.swc","resampled.swc_sorted.swc",df$V1[grep("auto_recons",df$V1)])

write.table(df, file="paths2.csv",row.names=F,col.names=F)

# df[grep("resampled.swc$",df$V1),] <- gsub("resampled.swc","resampled.swc_sorted.swc",df[grep("resampled.swc$",df$V1),])
write.table(df[grep("resampled.swc$",df$V1),], file="pathsgs.csv",row.names=F,col.names=F)
