library(ggplot2)
library(Rfast)
library(ggpubr)

load('shiny_app/groupsdf.Rdata')
load('shiny_app/subsetdata.Rdata')
# total_length <- my_data$total_length[groupsdf$group=="Gold_Standard"]
my_data <- my_data[groupsdf$group=="Gold_Standard",]

my_data <- my_data[,c(10,5,6,7,8,19,20,15,14)]

gs <- round(colcvs(as.matrix(my_data)),2)

neuromorpho <- c(0.57,1.82,0.43,0.51,0.65,0.17,0.18,0.85,0.57)

CV <- c(gs,neuromorpho)

dataset <- c(rep("Gold 162",9),rep("Neuromorpho",9))

# my_data$dataset <- 'Gold_Standard'
metric <- rep(names(my_data),2)

df <- data.frame(metric=metric,CV=CV,dataset=dataset)

  
ggbarplot(df,x="metric",y="CV",fill="dataset", color="dataset",palette = "Paired",label=T,position = position_dodge(0.8)) +
  xlab('Metric') +
  ylab('CV') +
  theme(axis.text.x = element_text(angle = 45,hjust=1))



