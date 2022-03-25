library(ggplot2)
library(Rfast)
library(ggpubr)

load('shiny_app/groupsdf.Rdata')
load('shiny_app/subsetdata.Rdata')

groupsdf$algorithm[groupsdf$algorithm=="app2new1"] <- "app2"
groupsdf$algorithm[groupsdf$algorithm=="app2new2"] <- "app2"
groupsdf$algorithm[groupsdf$algorithm=="app2new3"] <- "app2"
groupsdf$algorithm[groupsdf$algorithm=="Advantra_updated"] <- "Advantra"
groupsdf$algorithm[groupsdf$algorithm=="neutube_updated"] <- "neutube"
groupsdf$algorithm[groupsdf$algorithm=="pyzh_updated"] <- "pyzh"
groupsdf$algorithm[groupsdf$algorithm=="LCMboost_updated"] <- "LCMboost"
groupsdf$algorithm[groupsdf$algorithm=="LCMboost_3"] <- "LCMboost"
groupsdf$algorithm[groupsdf$algorithm=="fastmarching_spanningtree_updated"] <- "fastmarching_spanningtree"
groupsdf$algorithm[groupsdf$algorithm=="axis_analyzer_updated"] <- "axis_analyzer"
groupsdf$algorithm[groupsdf$algorithm=="NeuronChaser_updated"] <- "NeuronChaser"
groupsdf$algorithm[groupsdf$algorithm=="meanshift_updated"] <- "meanshift"
groupsdf$algorithm[groupsdf$algorithm=="NeuroGPSTree_updated"] <- "NeuroGPSTree"
groupsdf$algorithm[groupsdf$algorithm=="ENT_updated"] <- "EnsembleNeuronTracerBasic"

# total_length <- my_data$total_length[groupsdf$group=="Gold_Standard"]
# my_data <- my_data[groupsdf$dataset=="p_checked7_janelia_flylight_part1"|groupsdf$dataset=="p_checked7_janelia_flylight_part2",]
# groupsdf <- groupsdf[groupsdf$dataset=="p_checked7_janelia_flylight_part1"|groupsdf$dataset=="p_checked7_janelia_flylight_part2",]

# my_data <- my_data[groupsdf$dataset=="p_checked7_taiwan_flycirciut",]
# groupsdf <- groupsdf[groupsdf$dataset=="p_checked7_taiwan_flycirciut",]

my_data <- my_data[groupsdf$group=="Processed" | groupsdf$group=="Gold_Standard",]
groupsdf <- groupsdf[groupsdf$group=="Processed" | groupsdf$group=="Gold_Standard",]

my_data <- my_data[groupsdf$algorithm %in% c("Annotated","Advantra","app1","app2","nctuTW","neutube","smartTracing"),]
groupsdf <- groupsdf[groupsdf$algorithm %in% c("Annotated","Advantra","app1","app2","nctuTW","neutube","smartTracing"),]

my_data <- my_data[,names(my_data) %in% c("num_stems","overall_x_span","num_of_tips","total_length","max_path_distance",
                   "max_branch_order","average_contraction","average_fragmentation","bifurcation_angle_remote")]

mmetrics <- c("num_stems","overall_x_span","num_of_tips","total_length","max_path_distance",
              "max_branch_order","average_contraction","average_fragmentation","bifurcation_angle_remote")

# normalized <- function(x) (x- min(x))/(max(x) - min(x))
# my_data[] <- lapply(my_data, normalized) 

df <- data.frame(error=double(),metric=character(),algorithm=character(),id=character())

for(i in mmetrics){
  for(j in unique(groupsdf$ids))
  {
    for(k in unique(groupsdf$algorithm))
    {
      if(length(my_data[groupsdf$algorithm==k & groupsdf$ids==j,names(my_data)==i])==0){
        err <- NA
      }
      else{
        err <- abs((my_data[groupsdf$algorithm==k & groupsdf$ids==j,names(my_data)==i]-my_data[groupsdf$group=="Gold_Standard" & groupsdf$ids==j,names(my_data)==i])*100/my_data[groupsdf$group=="Gold_Standard" & groupsdf$ids==j,names(my_data)==i]) 
      }
      metr <- i
      alg <-k
      id <-j
      dfi <- data.frame(error=err,metric=i,algorithm=k,id=j)
      df <- rbind(df,dfi)
    }
  }
}

# df <- cbind(my_data,groupsdf)

# df <- ddply(df, .(algorithm, metric), summarize, error = error)
# # df$algorithm = factor(df$algorithm, levels=df[order(df$error), 'algorithm'])
# df$n = as.numeric(factor(df$algorithm))
# df = ddply(df,.(algorithm,metric),transform, x=paste(c(rep(' ',n-1), metric), collapse=''))
# df$x = factor(df$x, levels=df[order(df$error), 'x'])

# p <- ggbarplot(df,x="algorithm",y="error",fill="algorithm", color="algorithm",position = position_dodge(0.8),sort.val="asc") +
#   xlab('Algorithm') +
#   ylab('Error') +
#   theme(axis.text.x = element_text(angle = 45,hjust=1)) 
# 
# facet(p + theme_pubr(), facet.by="metric", scale='free')

reorder_within <- function(x, by, within, fun = median, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}


# df$value <- df$error
df <- df[!is.na(df$error),]
df <- df[df$algorithm!="Annotated",]
df <- df[df$metric!="num_stems",]
library(tidyverse)
# reorder_within and scale_x_reordered work.
# (Note that you need to set scales = "free_x" in the facet)
p <- ggplot(df, aes(reorder_within(algorithm, error, metric), error,fill=algorithm)) +
  # geom_bar(stat = 'mean') +
  geom_boxplot() +
  scale_x_reordered() +
  # scale_fill_brewer() +
  scale_y_log10() +
  scale_fill_brewer(palette="Set2") +
  facet_wrap(~ metric, scales = "free", nrow = 2) + 
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45,hjust=1)) 
p + stat_compare_means(method = "kruskal.test")

source("compare_means_LMG.R")
compare_means_LMG(error ~ algorithm,df,group.by="metric",method = "kruskal.test")
compare_means(error ~ algorithm,df,group.by="metric",method = "kruskal.test")

library(plotly)
ggplotly(p)


p2 <- ggboxplot(df, x="algorithm",y="error",
                fill="algorithm",palette="Set2") +
  # scale_x_reordered() +
  scale_y_log10() 
  
p2 <- facet(p2, facet.by = "metric",scales="free",nrow=2) + stat_compare_means() +scale_x_reordered()
p2

