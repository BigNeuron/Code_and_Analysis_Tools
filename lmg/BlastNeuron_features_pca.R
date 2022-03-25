library(ggbiplot)
library(pcaMethods)

df <- read.csv('features_gold.csv')
df$num_nodes <- NULL
data <- df[,2:35]
data <- remove_missing(data)

paths <- df$swc_file

names <- list()
for (i in 1:length(paths)){
  df$ids[i] <- as.numeric(strsplit(as.character(paths[i]),"/")[[1]][8])
  #df$names[i] <- strsplit(as.character(paths[i]),"/")[[1]][9]
  names[i] <- strsplit(as.character(paths[i]),"/")[[1]][9]
}
names <- unlist(names)

df2 <- read.csv('../BigNeurongit/Data/gold166/QC_Image.csv')
paths2 <- df2$Metadata_FileLocation
shortnames <- list()
for (i in 1:length(paths)){
  dotname  <- strsplit(names[i],'.',fixed=T)[[1]][1]
  #shortnames[i] <- substring(dotname,first=11)
  #print(i)
  #print(shortnames[i][[1]])
  if(i==127){
    df$Metadata_FileLocation[i] <- NA
    df$dataset[i]<-"p_checked6_mouse_ugoettingen"
    }
  else{
    df$Metadata_FileLocation[i] <- grep(substring(dotname,first=11),paths2,value=T)
    df$dataset[i] <- strsplit(df$Metadata_FileLocation[i],'/')[[1]][9]
    }
}
#shortnames <- unlist(shortnames)

#df <- subset(df,df$dataset!="p_checked6_human_allen_confocal")
df$group <- 'Gold_Standard'
df$algorithm <- 'Annotated'

groups <- df$dataset

data <- df[,2:35]

pca <- prcomp(data,
              center = TRUE,
              scale. = TRUE)

print(pca)
plot(pca)
summary(pca)

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              #var.axes = FALSE,
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
  labs(title = 'BlastNeuron metrics Gold 166 - GS')
print(g)

########################################################################################
df2 <- read.csv('features_auto.csv')
df2$num_nodes <- NULL
data2 <- df2[,2:35]
data2 <- remove_missing(data2)
data2 <- data2[data2$num_branches<1e100,]
data2 <- data2[data2$moment10>-500,]

dfgroups <- data.frame(ids=df$ids,dataset=df$dataset)

paths <- df2$swc_file

for (i in 1:length(paths)){
  df2$ids[i] <- as.numeric(strsplit(as.character(paths[i]),"/")[[1]][8])
  df2$algorithm[i] <- substring(strsplit(as.character(paths[i]),".",fixed = T)[[1]][2],first=8)
  df2$algorithm[i] <- gsub(".*?_app1", "app1", df2$algorithm[i])
  df2$algorithm[i] <- gsub(".*?_app2", "app2", df2$algorithm[i])
}

df2 <- merge(df2,dfgroups, by="ids")

df2$group <- 'Auto'

df2 <- remove_missing(df2)
df2 <- df2[df2$num_branches<1e100,]
df2 <- df2[df2$moment10>-500,]

groups <- df2$dataset

pca <- prcomp(data2,
              center = TRUE,
              scale. = TRUE)

print(pca)
plot(pca)
summary(pca)

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              #var.axes = FALSE,
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
  labs(title = 'BlastNeuron metrics Gold 166 - Auto')
print(g)


########################################################################################
df3 <- read.csv('features_processed.csv')
df3$num_nodes <- NULL
data3 <- df3[,2:35]
data3 <- remove_missing(data3)
data3 <- data3[data3$num_branches<1e100,]
data3 <- data3[data3$moment10>-500,]

paths <- df3$swc_file

for (i in 1:length(paths)){
  df3$ids[i] <- as.numeric(strsplit(as.character(paths[i]),"/")[[1]][8])
  df3$algorithm[i] <- substring(strsplit(as.character(paths[i]),".",fixed = T)[[1]][2],first=8)
  df3$algorithm[i] <- gsub(".*?_app1", "app1", df3$algorithm[i])
  df3$algorithm[i] <- gsub(".*?_app2", "app2", df3$algorithm[i])
}

df3 <- merge(df3,dfgroups, by="ids")

df3$group <- 'Processed'


df3 <- remove_missing(df3)
df3 <- df3[df3$num_branches<1e100,]
df3 <- df3[df3$moment10>-500,]

groups <- df3$dataset

pca <- prcomp(data3,
              center = TRUE,
              scale. = TRUE)

print(pca)
plot(pca)
summary(pca)

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              #var.axes = FALSE,
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
  labs(title = 'BlastNeuron metrics Gold 166 - Processed')
print(g)

########################################################################################
df4 <- read.csv('features_consensus.csv')
df4$num_nodes <- NULL
data4 <- df4[,2:35]
data4 <- remove_missing(data4)

paths <- df4$swc_file

for (i in 1:length(paths)){
  df4$ids[i] <- as.numeric(strsplit(as.character(paths[i]),"/")[[1]][8])
}

df4 <- merge(df4,dfgroups, by="ids")

df4$group <- 'Consensus'
df4$algorithm <- 'Consensus'

df4 <- remove_missing(df4)

groups <- df4$dataset

pca <- prcomp(data4,
              center = TRUE,
              scale. = TRUE)

print(pca)
plot(pca)
summary(pca)

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              #var.axes = FALSE,
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
  labs(title = 'BlastNeuron metrics Gold 166 - Consensus')
print(g)

#########################################################################################

dfids <- data.frame(ids=df$ids,Metadata_FileLocation=df$Metadata_FileLocation)
save(dfids, file='df_ids.Rdata')

df$Metadata_FileLocation <- NULL

dataf <- rbind(data,data2,data3,data4)
dff <- rbind(df,df2,df3,df4)

dataf <- dff[,2:35]
#dataf <- remove_missing(dataf)
#dff <- remove_missing(dff)
groups <- dff$group

pca <- prcomp(dataf,
              center = TRUE,
              scale. = TRUE)

print(pca)
plot(pca)
summary(pca)

g2 <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              labels.size = 15,
              varname.size = 4,
              groups = groups,
              ellipse = TRUE,
              #var.axes = FALSE,
              circle = FALSE)
g2 <- g2 + scale_color_discrete(name = '')
g2 <- g2 + theme(legend.direction = 'vertical',
               legend.position = 'right') +
  theme_minimal() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=17)) +
  theme(axis.title.y = element_text(size=17)) +
  theme(title = element_text(size=20)) +
  #xlim(-2,1) +
  #ylim(-1,1) +
  labs(title = 'BlastNeuron metrics Gold 166 - Group comparison')
print(g2)

save(dff,file='df_blastneuron.Rdata')
