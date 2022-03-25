library(mlr)
# library(mlr3viz)
# library("mlr3verse")
# remotes::install_github("mlr-org/mlr3extralearners")
# library(mlr3extralearners)
library(GGally)
library(plotly)
library(ggpubr)
library(RColorBrewer)
library(e1071)

# RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion")
RNGversion('4.0.3')

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


load('../shiny_app/subsetdata_v3.Rdata')
load('../shiny_app/groupsdf.Rdata')

data <- my_data 

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

ml <- ml[,names(ml) %in% c(
  # "num_stems","num_of_tips","average_diameter","total_length","max_path_distance",
  # "max_branch_order","bifurcation_angle_remote",
  "soma_surface","num_stems","num_bifurcations","num_branches","num_of_tips",
  "overall_x_span","overall_y_span","overall_z_span","average_diameter",
  "total_length","total_surface","total_volume","max_euclidean_distance",
  "max_path_distance","max_branch_order","average_contraction",
  "average_fragmentation","parent_daughter_ratio","bifurcation_angle_local",
  "bifurcation_angle_remote","ave_R",
  "xy_pixel_size","z_pixel_size",
  # "Correlation","FocusScore","MedianIntensity","PercentMinimal","StdIntensity",
  # "Correlation_swc","FocusScore_swc","MedianIntensity_swc","PercentMinimal_swc","StdIntensity_swc",
  "FocusScore","MADIntensity","MaxIntensity",
  "MeanIntensity","MedianIntensity","PercentMaximal",#"MinIntensity",
  "PercentMinimal","StdIntensity","ThresholdOtsu","SNR_otsu","CNR_otsu","SNR_mean","CNR_mean",
  "FocusScore_swc","MADIntensity_swc","MaxIntensity_swc",
  "MeanIntensity_swc","MedianIntensity_swc","MinIntensity_swc","PercentMaximal_swc",
  "PercentMinimal_swc","StdIntensity_swc","ThresholdOtsu_swc","SNR_otsu_swc","CNR_otsu_swc","SNR_mean_swc","CNR_mean_swc",
  # "LocalFocusScore_swc","overall_z_span","MADIntensity_swc","overall_y_span",
  # "overall_x_span","max_euclidean_distance","FocusScore_swc","PercentMinimal_swc",
  # "PowerLogLogSlope_swc","MedianIntensity_swc","MeanIntensity_swc",
  "group","algorithm","ids",
  "entire.structure.average..from.neuron.1.to.2.","entire.structure.average..from.neuron.2.to.1.",      
  "average.of.bi.directional.entire.structure.averages","different.structure.average",                        
  "percent.of.different.structure..from.neuron.1.to.2.","percent.of.different.structure..from.neuron.2.to.1.",
  "percent.of.different.structure")]

# ml <- melt(ml,id=44:46)

ml$ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)

ml <- ml[ml$num_of_tips>1,]
ml <- ml[ml$algorithm!="Annotated",]
ml$SNR_mean[is.na(ml$SNR_mean)] <- 1e12
ml$CNR_mean[is.na(ml$CNR_mean)] <- 1e12
ml$SNR_mean_swc[is.na(ml$SNR_mean_swc)] <- 1e12
ml$CNR_mean_swc[is.na(ml$CNR_mean_swc)] <- 1e12

ml$SNR_otsu[is.na(ml$SNR_otsu)] <- 1e12
ml$SNR_otsu[ml$SNR_otsu>1e10] <- 1e12
ml$CNR_otsu[is.na(ml$CNR_otsu)] <- 1e12
ml$SNR_otsu_swc[is.na(ml$SNR_otsu_swc)] <- 1e12
ml$CNR_otsu_swc[is.na(ml$CNR_otsu_swc)] <- 1e12
# ml <- ml[ml$group=="Auto" | ml$group=="Consensus",]

# ml <- na.omit(ml)

load("clusters_both.Rdata")
# ml <- ml[ml$ids %in% idsclusts$ids[idsclusts$clusters_both==1],]
##
ml$bestalg <- ml$algorithm
ml$bestalg2 <- ml$algorithm
ml$bestalg3 <- ml$algorithm
ml$bestalg4 <- ml$algorithm
ml$bestalg5 <- ml$algorithm
ml$bestalg6 <- ml$algorithm
ml$bestalg7 <- ml$algorithm
ml$bestalg8 <- ml$algorithm

ml$worstalg <- ml$algorithm
ml$worstalg2 <- ml$algorithm
ml$worstalg3 <- ml$algorithm
ml$worstalg4 <- ml$algorithm
ml$worstalg5 <- ml$algorithm

for(i in unique(ml$ids)){
  bestalg <- ml[ml$ids==i & ml$`percent.of.different.structure` == min(ml[ml$ids==i,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg <- bestalg[1]
  bestalg2 <- ml[ml$ids==i & ml$`percent.of.different.structure` == min(ml[ml$ids==i & ml$algorithm!=bestalg,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg2 <- bestalg2[1]
  bestalg3 <- ml[ml$ids==i & ml$`percent.of.different.structure` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg3 <- bestalg3[1]
  bestalg4 <- ml[ml$ids==i & ml$`percent.of.different.structure` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg4 <- bestalg4[1]
  bestalg5 <- ml[ml$ids==i & ml$`percent.of.different.structure` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg5 <- bestalg5[1]
  bestalg6 <- ml[ml$`percent.of.different.structure` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4 & ml$algorithm!=bestalg5,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg6 <- bestalg6[1]
  bestalg7 <- ml[ml$`percent.of.different.structure` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4 & ml$algorithm!=bestalg5 & ml$algorithm!=bestalg6,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg7 <- bestalg7[1]
  bestalg8 <- ml[ml$`percent.of.different.structure` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4 & ml$algorithm!=bestalg5 & ml$algorithm!=bestalg6 & ml$algorithm!=bestalg7,]$`percent.of.different.structure`),]$algorithm
  ml[ml$ids == i,]$bestalg8 <- bestalg8[1]
  
  worstalg <- ml[ml$ids==i & ml$`percent.of.different.structure` == max(ml[ml$ids==i,]$`percent.of.different.structure`),]$algorithm[1]
  ml[ml$ids == i,]$worstalg <- worstalg[1]
  worstalg2 <- ml[ml$ids==i & ml$`percent.of.different.structure` == max(ml[ml$ids==i & ml$algorithm!=worstalg,]$`percent.of.different.structure`),]$algorithm[1]
  ml[ml$ids == i,]$worstalg2 <- worstalg2[1]
  worstalg3 <- ml[ml$ids==i & ml$`percent.of.different.structure` == max(ml[ml$ids==i & ml$algorithm!=worstalg & ml$algorithm!=worstalg2,]$`percent.of.different.structure`),]$algorithm[1]
  ml[ml$ids == i,]$worstalg3 <- worstalg3[1]
  worstalg4 <- ml[ml$ids==i & ml$`percent.of.different.structure` == max(ml[ml$ids==i & ml$algorithm!=worstalg & ml$algorithm!=worstalg2 & ml$algorithm!=worstalg3,]$`percent.of.different.structure`),]$algorithm[1]
  ml[ml$ids == i,]$worstalg4 <- worstalg4[1]
  worstalg5 <- ml[ml$ids==i & ml$`percent.of.different.structure` == max(ml[ml$ids==i & ml$algorithm!=worstalg & ml$algorithm!=worstalg2 & ml$algorithm!=worstalg3 & ml$algorithm!=worstalg4,]$`percent.of.different.structure`),]$algorithm[1]
  ml[ml$ids == i,]$worstalg5 <- worstalg5[1]
}

ml$bestalg <- as.factor(ml$bestalg)
ml$bestalg2 <- as.factor(ml$bestalg2)
ml$bestalg3 <- as.factor(ml$bestalg3)
ml$bestalg4 <- as.factor(ml$bestalg4)
ml$bestalg5 <- as.factor(ml$bestalg5)
ml$bestalg6 <- as.factor(ml$bestalg6)
ml$bestalg7 <- as.factor(ml$bestalg7)
ml$bestalg8 <- as.factor(ml$bestalg8)

ml$worstalg <- as.factor(ml$worstalg)
ml$worstalg2 <- as.factor(ml$worstalg2)
ml$worstalg3 <- as.factor(ml$worstalg3)
ml$worstalg4 <- as.factor(ml$worstalg4)
ml$worstalg5 <- as.factor(ml$worstalg5)

bestalg <- ml$bestalg
bestalg2 <- ml$bestalg2
bestalg3 <- ml$bestalg3
bestalg4 <- ml$bestalg4
bestalg5 <- ml$bestalg5
bestalg6 <- ml$bestalg6
bestalg7 <- ml$bestalg7
bestalg8 <- ml$bestalg8

worstalg <- ml$worstalg
worstalg2 <- ml$worstalg2
worstalg3 <- ml$worstalg3
worstalg4 <- ml$worstalg4
worstalg5 <- ml$worstalg5

p <- ggplot(ml[ml$algorithm == ml$bestalg,], aes(x=percent.of.different.structure, fill=algorithm)) + geom_density(alpha=0.4)
# ggplotly(p)
p <- ggplot(ml, aes(x=percent.of.different.structure, fill=ids)) + geom_density(alpha=0.4)
# ggplotly(p)
plot(density(ml$percent.of.different.structure),main="Percentage of difference to GS for all algorithms")
pddf <- data.frame(pcdiff=ml$percent.of.different.structure, set = "All algorithms",ids=ml$ids)
plot(density(ml$percent.of.different.structure[ml$algorithm == ml$bestalg]),main="Percentage of difference to GS for true best algorithms")
pddf2 <- data.frame(pcdiff=ml$percent.of.different.structure[ml$algorithm == ml$bestalg], set = "True best algorithms")
# plot(density(preddf$pctdiffstr_pred),main="Percentage of difference to GS for predicted algorithm")


ml$bestalg2 <- NULL
ml$bestalg3 <- NULL
ml$bestalg4 <- NULL
ml$bestalg5 <- NULL
ml$bestalg6 <- NULL
ml$bestalg7 <- NULL
ml$bestalg8 <- NULL

ml$worstalg <- NULL
ml$worstalg2 <- NULL
ml$worstalg3 <- NULL
ml$worstalg4 <- NULL
ml$worstalg5 <- NULL

dbwalg <- data.frame(ids = ml$ids,
                     bestalg = as.factor(bestalg), 
                     bestalg2 = as.factor(bestalg2), 
                     bestalg3 = as.factor(bestalg3), 
                     bestalg4 = as.factor(bestalg4), 
                     bestalg5 = as.factor(bestalg5), 
                     bestalg6 = as.factor(bestalg6), 
                     bestalg7 = as.factor(bestalg7), 
                     bestalg8 = as.factor(bestalg8), 
                     worstalg = as.factor(worstalg), 
                     worstalg2 = as.factor(worstalg2), 
                     worstalg3 = as.factor(worstalg3), 
                     worstalg4 = as.factor(worstalg4), 
                     worstalg5 = as.factor(worstalg5))

dbwalg <- unique(dbwalg)
##

# levels(ml$variable)[levels(ml$variable)=='average.of.bi.directional.entire.structure.averages'] <- 'av_bid_ent_str_av'
names(ml)[names(ml) == 'average.of.bi.directional.entire.structure.averages'] <- 'av_bid_ent_str_av'

# https://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
library(caret)
library(rpart.plot)

anyNA(ml)

ml$group <- NULL
ml$dataset <- NULL
ids <- unique(ml$ids)

ml$soma_surface <- NULL

# only average features
ml <- unique(ml)

ml$percent.of.different.structure <- as.factor(ml$percent.of.different.structure)

#' Function which wraps preProcess to exclude factors from the model.matrix
ppWrapper <- function( x, excludeClasses=c("factor"), ... ) {
  whichToExclude <- sapply( x, function(y) any(sapply(excludeClasses, function(excludeClass) is(y,excludeClass) )) )
  processedMat <- predict( preProcess( x[!whichToExclude], ...), newdata=x[!whichToExclude], method='BoxCox')
  x[!whichToExclude] <- processedMat
  x
}
ml <- ppWrapper(ml)
anyNA(ml)

ml$percent.of.different.structure <- as.numeric(as.character(ml$percent.of.different.structure))


# Create training dataset
set.seed(15, kind = "Mersenne-Twister", normal.kind = "Inversion") 
ml <- ml[sample(1:nrow(ml)), ]
######################
######################
ml$entire.structure.average..from.neuron.1.to.2. <- NULL
ml$entire.structure.average..from.neuron.2.to.1. <- NULL
ml$different.structure.average <- NULL
ml$percent.of.different.structure..from.neuron.1.to.2. <- NULL
ml$percent.of.different.structure..from.neuron.2.to.1. <- NULL
# ml$percent.of.different.structure <- NULL
ml$av_bid_ent_str_av <- NULL
# ml$bestalg <- NULL
ml$bestalg <- as.factor(ml$bestalg)

ml <- na.omit(ml)
idstrain <- sample(ids, 0.85*length(ids), replace=F)
training <- ml[ml$ids %in% idstrain,]
testing <-  ml[!(ml$ids %in% idstrain),]

training <- droplevels(training)

#check dimensions of train & test set
dim(training); dim(testing);

idsml <- as.factor(ml$ids)
ml$ids <- as.factor(ml$ids)
# ml$ids <- NULL

# autoplot(task, type="pairs", cardinality_threshold=156)

# # subset task to only use the 10 first features
# task$select(head(task$feature_names, 10))
# # default plot: class frequencies
# autoplot(task, cardinality_threshold=21)
# # pairs plot (requires package GGally)
# autoplot(task, type = "pairs", cardinality_threshold=21)
# # duo plot (requires package GGally)
# autoplot(task, type = "duo", cardinality_threshold=21)


#################  Regression
bestalgs <- ml$bestalg
ml$bestalg <- NULL
algs <- ml$algorithm
ml$algorithm <- NULL
idsml <- ml$ids
ml$ids <- NULL

tsk = makeRegrTask(data = ml, target = "percent.of.different.structure", blocking = idsml)

lrn = makeLearner("regr.svm")
rdesc = makeResampleDesc("Subsample", iters = 10, blocking.cv = TRUE)

LCdata <- generateLearningCurveData(lrn,
                                    tsk,
                                    percs = seq(0.05, 0.9, by = 0.1),
                                    measures=list(
                                      expvar, kendalltau, mae, mape, medae,
                                      medse, mse, msle, rae, rmse, rmsle, rrse,
                                      rsq, sae, spearmanrho, sse), 
                                    resampling = rdesc)
plotLearningCurve(LCdata) + theme_pubr() 
