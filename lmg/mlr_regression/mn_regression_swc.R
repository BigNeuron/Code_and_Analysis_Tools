# require(foreign)
# require(nnet)
# require(ggplot2)
# require(reshape2)
# # https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# ml_b <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

require(tidyr)

plotCM <- function(cm){
  cmdf <- as.data.frame(cm[["table"]])
  cmdf[["color"]] <- ifelse(cmdf[[1]] == cmdf[[2]], "green", "red")
  
  alluvial::alluvial(cmdf[,1:2]
                     , freq = cmdf$Freq
                     , col = cmdf[["color"]]
                     , alpha = 0.5
                     , hide  = cmdf$Freq == 0
  )
}

load('subsetdata.Rdata')
data <- my_data
load('groupsdf.Rdata')

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

ml <- cbind(data,groupsdf)
ml <- ml[,c(1:28,44:61)]
# ml <- melt(ml,id=44:46)

ml$ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)

ml <- ml[ml$algorithm!="Annotated",]
##
ml$bestalg <- ml$algorithm

for(i in unique(ml$ids)){
  # bestalg <- ml[ml$`average of bi-directional entire-structure-averages` == min(ml[ml$ids==i,]$`average of bi-directional entire-structure-averages`),]$algorithm
  bestalg <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg <- bestalg[1]
}

# ml <- ml[ml$algorithm==ml$bestalg,]
# ml$algorithm <- NULL
##

# levels(ml$variable)[levels(ml$variable)=='average of bi-directional entire-structure-averages'] <- 'av_bid_ent_str_av'
# names(ml)[names(ml) == 'average of bi-directional entire-structure-averages'] <- 'av_bid_ent_str_av'
names(ml)[names(ml) == 'average.of.bi.directional.entire.structure.averages'] <- 'av_bid_ent_str_av'

with(ml, table(variable, algorithm))
with(ml, do.call(rbind, tapply(av_bid_ent_str_av, algorithm, function(x) c(M = mean(x), SD = sd(x)))))

# https://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
library(caret)
library(rpart.plot)

# Normalize dataset
# ml[,1:43] <- scale(ml[,1:43])
ml <- ml[complete.cases(ml)==T,]
anyNA(ml)

ml$group <- NULL
ml$dataset <- NULL
ml$ids <- NULL
# ml$`entire-structure-average (from neuron 1 to 2)`<- NULL
# ml$`entire-structure-average (from neuron 2 to 1)`<- NULL
# ml$av_bid_ent_str_av <- NULL
# ml$`different-structure-average`<- NULL
# ml$`percent of different-structure (from neuron 1 to 2)`<- NULL
# ml$`percent of different-structure (from neuron 2 to 1)`<- NULL
# ml$`percent of different-structure`<- NULL

ml$`entire.structure.average..from.neuron.1.to.2.`<- NULL
ml$`entire.structure.average..from.neuron.2.to.1.`<- NULL
ml$av_bid_ent_str_av <- NULL
ml$`different.structure.average`<- NULL
ml$`percent.of.different.structure..from.neuron.1.to.2.`<- NULL
ml$`percent.of.different.structure..from.neuron.2.to.1.`<- NULL
ml$`percent.of.different.structure`<- NULL

ml <- droplevels(ml)

# ml[,1:43] <- scale(ml[,1:43])

# Create training dataset
# set.seed(3033)
intrain <- createDataPartition(y = ml$bestalg, p= 0.7, list = FALSE)
training <- ml[intrain,]
testing <- ml[-intrain,]
training <- droplevels(training)

#check dimensions of train & test set
dim(training); dim(testing);

# Training
trctrl <- trainControl(method = "repeatedcv", number = 20, repeats = 5)
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       summaryFunction = multiClassSummary,
                       classProbs = TRUE)
# preProcessInTrain<-c("center", "scale")
# metric_used<-"algorithm"

# set.seed(3333)
dtree_fit <- train(bestalg ~., data = training, method = "rpart",
                   parms = list(split = "gini"),
                   # trControl=trainControl(method="none"),
                   trControl=trctrl,
                   # metric=metric_used,
                   # preProc = preProcessInTrain,
                   tuneLength = 20)

# dtree_fit <- train(bestalg ~., data = training, method = "treebag")

# dtree_fit <- train(bestalg ~., data = training, method = "LogitBoost")# 85% Acc

# Plot decision tree
prp(dtree_fit$finalModel, box.palette = "Blues", tweak = 1.2)

library(rattle)	
fancyRpartPlot(dtree_fit$finalModel)

# Predict
predict(dtree_fit, newdata = testing[1,])
test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$bestalg) %>% plotCM() #check accuracy

confusionMatrix(test_pred, testing$bestalg)


