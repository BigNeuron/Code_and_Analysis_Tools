
# for (i in dir('gold_163_all_soma_sort_s1/')){
#   text <- read.csv(paste0('gold_163_all_soma_sort_s1/',i,'/dists_auto/*.dist.txt'))
# }

dataFiles_proc <- lapply(Sys.glob("gold_163_all_soma_sort_s1/*/dists_proc/*.dist.txt"), readLines)
dataFiles_auto <- lapply(Sys.glob("gold_163_all_soma_sort_s1/*/dists_auto/*.dist.txt"), readLines)
dataFiles_cons <- lapply(Sys.glob("gold_163_all_soma_sort_s1/*/dists_cons/*.dist.txt"), readLines)

# input_auto <- lapply(as.character(dataFiles_auto), strsplit, split = "=")
# input_cons <- lapply(as.character(dataFiles_cons), strsplit, split = "=")
dfauto <- data.frame()[1:length(dataFiles_auto),]
for (i in 1:length(dataFiles_auto)){
  dfauto$input1[i] <- strsplit(dataFiles_auto[i][[1]][1],'=')[[1]][2] 
  dfauto$input2[i] <- strsplit(dataFiles_auto[i][[1]][2],'=')[[1]][2]
  dfauto$ids[i] <- as.numeric(strsplit(dfauto$input1[i],'/')[[1]][2])
  dfauto$algorithm[i] <- substring(strsplit(dfauto$input2[i],".",fixed = T)[[1]][2],first=8)
  dfauto$algorithm[i] <- gsub(".*?_app1", "app1", dfauto$algorithm[i])
  dfauto$algorithm[i] <- gsub(".*?_app2", "app2", dfauto$algorithm[i])
  dfauto$esa1t2[i] <- as.numeric(strsplit(dataFiles_auto[i][[1]][3],'=')[[1]][2])
  dfauto$esa2t1[i] <- as.numeric(strsplit(dataFiles_auto[i][[1]][4],'=')[[1]][2])
  dfauto$ab_esa[i] <- as.numeric(strsplit(dataFiles_auto[i][[1]][5],'=')[[1]][2])
  dfauto$dsa[i] <- as.numeric(strsplit(dataFiles_auto[i][[1]][6],'=')[[1]][2])
  dfauto$pds1t2[i] <- as.numeric(strsplit(dataFiles_auto[i][[1]][7],'=')[[1]][2])
  dfauto$pds2t1[i] <- as.numeric(strsplit(dataFiles_auto[i][[1]][8],'=')[[1]][2])
  dfauto$a_pds[i] <- as.numeric(strsplit(dataFiles_auto[i][[1]][9],'=')[[1]][2])
}

dfproc <- data.frame()[1:length(dataFiles_proc),]
for (i in 1:length(dataFiles_proc)){
  dfproc$input1[i] <- strsplit(dataFiles_proc[i][[1]][1],'=')[[1]][2] 
  dfproc$input2[i] <- strsplit(dataFiles_proc[i][[1]][2],'=')[[1]][2]
  dfproc$ids[i] <- as.numeric(strsplit(dfproc$input1[i],'/')[[1]][2])
  dfproc$algorithm[i] <- substring(strsplit(dfproc$input2[i],".",fixed = T)[[1]][2],first=8)
  dfproc$algorithm[i] <- gsub(".*?_app1", "app1", dfproc$algorithm[i])
  dfproc$algorithm[i] <- gsub(".*?_app2", "app2", dfproc$algorithm[i])
  dfproc$esa1t2[i] <- as.numeric(strsplit(dataFiles_proc[i][[1]][3],'=')[[1]][2])
  dfproc$esa2t1[i] <- as.numeric(strsplit(dataFiles_proc[i][[1]][4],'=')[[1]][2])
  dfproc$ab_esa[i] <- as.numeric(strsplit(dataFiles_proc[i][[1]][5],'=')[[1]][2])
  dfproc$dsa[i] <- as.numeric(strsplit(dataFiles_proc[i][[1]][6],'=')[[1]][2])
  dfproc$pds1t2[i] <- as.numeric(strsplit(dataFiles_proc[i][[1]][7],'=')[[1]][2])
  dfproc$pds2t1[i] <- as.numeric(strsplit(dataFiles_proc[i][[1]][8],'=')[[1]][2])
  dfproc$a_pds[i] <- as.numeric(strsplit(dataFiles_proc[i][[1]][9],'=')[[1]][2])
}

dfcons <- data.frame()[1:length(dataFiles_cons),]
for (i in 1:length(dataFiles_cons)){
  dfcons$input1[i] <- strsplit(dataFiles_cons[i][[1]][1],'=')[[1]][2] 
  dfcons$input2[i] <- strsplit(dataFiles_cons[i][[1]][2],'=')[[1]][2]
  dfcons$ids[i] <- as.numeric(strsplit(dfcons$input1[i],'/')[[1]][2])
  dfcons$algorithm[i] <- strsplit(strsplit(dfcons$input2[i],'/')[[1]][3],'\\.')[[1]][1]
  dfcons$esa1t2[i] <- as.numeric(strsplit(dataFiles_cons[i][[1]][3],'=')[[1]][2])
  dfcons$esa2t1[i] <- as.numeric(strsplit(dataFiles_cons[i][[1]][4],'=')[[1]][2])
  dfcons$ab_esa[i] <- as.numeric(strsplit(dataFiles_cons[i][[1]][5],'=')[[1]][2])
  dfcons$dsa[i] <- as.numeric(strsplit(dataFiles_cons[i][[1]][6],'=')[[1]][2])
  dfcons$pds1t2[i] <- as.numeric(strsplit(dataFiles_cons[i][[1]][7],'=')[[1]][2])
  dfcons$pds2t1[i] <- as.numeric(strsplit(dataFiles_cons[i][[1]][8],'=')[[1]][2])
  dfcons$a_pds[i] <- as.numeric(strsplit(dataFiles_cons[i][[1]][9],'=')[[1]][2])
}

dfall <- rbind(dfproc,dfauto,dfcons)

save(dfall,file='df_distsall.Rdata')
