library(Rtsne)
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
  if(i==127){df$dataset[i]<-"p_checked6_mouse_ugoettingen"}
  else{
    df$dataset[i] <- grep(substring(dotname,first=11),paths2,value=T)
    df$dataset[i] <- strsplit(df$dataset[i],'/')[[1]][9]
  }
}
#shortnames <- unlist(shortnames)

#df <- subset(df,df$dataset!="p_checked6_human_allen_confocal")
df$group <- 'Gold_Standard'
df$algorithm <- 'Annotated'

groups <- df$dataset

## Curating the database for analysis with both t-SNE and PCA
## for plotting
colors = rainbow(length(unique(groups)))
names(colors) = unique(groups)

## Executing the algorithm on curated data
tsne <- Rtsne(data, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500, check_duplicates=FALSE)
exeTimeTsne<- system.time(Rtsne(data, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500, check_duplicates=FALSE))

## Plotting
plot(tsne$Y, t='n', main="tsne")
points(tsne$Y, labels=groups, col=colors[groups])
#text(tsne$Y, labels=groups, col=colors[groups])


########################################################################################
df2 <- read.csv('features_auto.csv')
df2$num_nodes <- NULL
data2 <- df2[,2:35]
data2 <- remove_missing(data2)

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


########################################################################################
df3 <- read.csv('features_processed.csv')
df3$num_nodes <- NULL
data3 <- df3[,2:35]
data3 <- remove_missing(data3)

paths <- df3$swc_file

for (i in 1:length(paths)){
  df3$ids[i] <- as.numeric(strsplit(as.character(paths[i]),"/")[[1]][8])
  df3$algorithm[i] <- substring(strsplit(as.character(paths[i]),".",fixed = T)[[1]][2],first=8)
  df3$algorithm[i] <- gsub(".*?_app1", "app1", df3$algorithm[i])
  df3$algorithm[i] <- gsub(".*?_app2", "app2", df3$algorithm[i])
}

df3 <- merge(df3,dfgroups, by="ids")

df3$group <- 'Processed'


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

#########################################################################################

dataf <- rbind(data,data2,data3,data4)
dff <- rbind(df,df2,df3,df4)

dataf <- dff[,2:35]
dataf <- remove_missing(dataf)
dff <- remove_missing(dff)
groups <- dff$group

## Curating the database for analysis with both t-SNE and PCA
## for plotting
colors = rainbow(length(unique(groups)))
names(colors) = unique(groups)

## Executing the algorithm on curated data
tsne <- Rtsne(dataf, dims = 2, perplexity=50, verbose=TRUE, max_iter = 500, check_duplicates=FALSE, num_threads=4)
#exeTimeTsne<- system.time(Rtsne(dataf, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500, check_duplicates=FALSE))

## Plotting
#plot(tsne$Y, t='n', main="tsne")
#points(tsne$Y, labels=groups, col=colors[groups])
#text(tsne$Y, labels=groups, col=colors[groups])

groups <- dff$group
tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = groups)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col)) +
  theme_minimal()

########
groups <- dff$algorithm
tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = groups)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col)) +
  theme_minimal()

########
groups <- dff$dataset
tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = groups)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col)) +
  theme_minimal()
