library(mlr)

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
load('groupsdf.Rdata')

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
  "soma_surface",
  # "num_stems","num_bifurcations","num_branches",
  # "num_of_tips",
  "overall_x_span",
  "overall_y_span",
  # "overall_z_span",
  "average_diameter",
  # "total_length",
  # "total_surface",
  # "total_volume",
  "max_euclidean_distance",
  # "max_path_distance",
  # "max_branch_order",
  "average_contraction",
  "average_fragmentation",
  "parent_daughter_ratio",
  "bifurcation_angle_local",
  "bifurcation_angle_remote",
  "ave_R",
  # "xy_pixel_size",
  # "z_pixel_size",
  # "Correlation","FocusScore","MedianIntensity","PercentMinimal","StdIntensity",
  # "Correlation_swc","FocusScore_swc","MedianIntensity_swc","PercentMinimal_swc","StdIntensity_swc",
  # "Correlation",
  "FocusScore",
  "LocalFocusScore",
  "MADIntensity",
  "MaxIntensity",
  # "MeanIntensity",
  "MedianIntensity",
  "MinIntensity",
  "PercentMaximal",
  "PercentMinimal",
  "PowerLogLogSlope",
  "StdIntensity",
  "ThresholdOtsu",
  "TotalArea",
  "TotalIntensity",
  # "Correlation_swc",
  "FocusScore_swc",
  "LocalFocusScore_swc",
  "MADIntensity_swc",
  "MaxIntensity_swc",
  "MeanIntensity_swc",
  "MedianIntensity_swc",
  "MinIntensity_swc",
  "PercentMaximal_swc",
  "PercentMinimal_swc",
  "PowerLogLogSlope_swc",
  "StdIntensity_swc",
  "ThresholdOtsu_swc",
  # "TotalArea_swc",
  # "TotalIntensity_swc",
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

ml <- ml[ml$algorithm!="Annotated",]
ml <- ml[ml$group=="Auto" || ml$group=="Consensus",]

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

for(i in unique(ml$ids)){
  bestalg <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg <- bestalg[1]
  bestalg2 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i & ml$algorithm!=bestalg,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg2 <- bestalg2[1]
  bestalg3 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg3 <- bestalg3[1]
  bestalg4 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg4 <- bestalg4[1]
  bestalg5 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg5 <- bestalg5[1]
  bestalg6 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4 & ml$algorithm!=bestalg5,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg6 <- bestalg6[1]
}

algs <- ml$algorithm
ml$algorithm <- NULL
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

#' Function which wraps preProcess to exclude factors from the model.matrix
# ppWrapper <- function( x, excludeClasses=c("factor"), ... ) {
#   whichToExclude <- sapply( x, function(y) any(sapply(excludeClasses, function(excludeClass) is(y,excludeClass) )) )
#   processedMat <- predict( preProcess( x[!whichToExclude], ..., method=c('BoxCox','corr'), cutoff=0.6), newdata=x[!whichToExclude])
#   x[!whichToExclude] <- processedMat
#   x
# }
# ml <- ppWrapper(ml)
# ml <- predict(preProcess(ml, method=c('BoxCox','corr'), cutoff=0.3),newdata=ml)
anyNA(ml)



# Create training dataset
# set.seed(303)
ml <- ml[sample(1:nrow(ml)), ]
######################
######################
ml$entire.structure.average..from.neuron.1.to.2. <- NULL
ml$entire.structure.average..from.neuron.2.to.1. <- NULL
ml$different.structure.average <- NULL
ml$percent.of.different.structure..from.neuron.1.to.2. <- NULL
ml$percent.of.different.structure..from.neuron.2.to.1. <- NULL
ml$percent.of.different.structure <- NULL
# ml$av_bid_ent_str_av <- NULL
# ml$bestalg <- NULL
ml$bestalg <- as.factor(ml$bestalg)
ml$bestalg2 <- as.factor(ml$bestalg2)
ml$bestalg3 <- as.factor(ml$bestalg3)
ml$bestalg4 <- as.factor(ml$bestalg4)
ml$bestalg5 <- as.factor(ml$bestalg5)
ml$bestalg6 <- as.factor(ml$bestalg6)

ml <- na.omit(ml)
idstrain <- sample(ids, 0.3*length(ids), replace=F)
training <- ml[ml$ids %in% idstrain,]
algstrain <- algs[ml$ids %in% idstrain]
testing <-  ml[!(ml$ids %in% idstrain),]
algstest <- algs[!(ml$ids %in% idstrain)]

training <- droplevels(training)

#check dimensions of train & test set
dim(training); dim(testing);

idsml <- as.factor(ml$ids)
ml$ids <- as.factor(ml$ids)
ml$ids <- NULL

bestalg <- ml$bestalg
bestalg2 <- ml$bestalg2
bestalg3 <- ml$bestalg3
bestalg4 <- ml$bestalg4
bestalg5 <- ml$bestalg5
bestalg6 <- ml$bestalg6
ml$bestalg <- NULL
ml$bestalg2 <- NULL
ml$bestalg3 <- NULL
ml$bestalg4 <- NULL
ml$bestalg5 <- NULL
ml$bestalg6 <- NULL

###################  Regression
# tsk = makeRegrTask(data = ml, target = "av_bid_ent_str_av", blocking = idsml)
# rdesc = makeResampleDesc("Subsample", iters = 5, split = 0.8, blocking.cv = TRUE)
# # rdesc = makeResampleDesc("CV", iters = 10, blocking.cv = TRUE)
# # rdesc = makeResampleDesc("Bootstrap", iters = 10, blocking.cv = TRUE)
# # options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
# # gc()
# res = resample(makePreprocWrapperCaret("regr.ctree", ppc.expoTrans = TRUE, ppc.scale = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.6),
# # res = resample(makeLearner("regr.ctree"),
#                tsk,
#                # parms = list(split = "information"),
#                measures = list(mape,rmse),
#                resampling = rdesc)

################ Benchmarking 
# rdesc = makeResampleDesc("Subsample", iters = 5, split = 0.85, blocking.cv = TRUE)
# # Vector of strings
# lrns = list(#makePreprocWrapperCaret( makeLearner("regr.svm"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             # makePreprocWrapperCaret( makeLearner("classif.ksvm"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             makePreprocWrapperCaret( makeLearner("regr.ctree"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             # makePreprocWrapperCaret( makeLearner("classif.nnet"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             makePreprocWrapperCaret( makeLearner("regr.rpart"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          # makePreprocWrapperCaret( makeLearner("regr.randomForest"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          # makePreprocWrapperCaret( makeLearner("classif.PART"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          # makePreprocWrapperCaret( makeLearner("regr.extraTrees"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          # makePreprocWrapperCaret( makeLearner("regr.glmboost"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          # makePreprocWrapperCaret( makeLearner("classif.glmnet"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          makePreprocWrapperCaret( makeLearner("regr.bst"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7))
# 
# # A mixed list of Learner objects and strings works, too
# # lrns = list(makeLearner("classif.lda", predict.type = "prob"), "classif.rpart")
# 
# bmr = benchmark(lrns, tsk, rdesc, models = TRUE,measures = list(mape,rmse))
# plotBMRBoxplots(bmr)


################# Predict
lrn = makeLearner("regr.bst")
lrn = makePreprocWrapperCaret(lrn, ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7)
m = mlr::train(lrn, tsk, subset = which(idsml %in% idstrain))
reference <- testing$av_bid_ent_str_av
test_pred <- predict(m, task = tsk, subset =  which(!(idsml %in% idstrain)))
bestalg_perid <- NULL
ref_perid <- NULL
predids <- NULL
pbestalg2 <- NULL
pbestalg3 <- NULL
pbestalg4 <- NULL
pbestalg5 <- NULL
pbestalg6 <- NULL

for(i in unique(testing$ids)){
  predids[i] <- i
  bestalg_perid[i] <- as.character(algstest[which(test_pred$data$response==min(test_pred$data$response[which(testing$ids==i)]))])
  ref_perid[i] <- as.character(unique(testing$bestalg[testing$id==i]))
  pbestalg2[i] <- as.character(unique(testing$bestalg2[testing$id==i]))
  pbestalg3[i] <- as.character(unique(testing$bestalg3[testing$id==i]))
  pbestalg4[i] <- as.character(unique(testing$bestalg4[testing$id==i]))
  pbestalg5[i] <- as.character(unique(testing$bestalg5[testing$id==i]))
  pbestalg6[i] <- as.character(unique(testing$bestalg6[testing$id==i]))
}

pred <- data.frame(id = predids,pred=bestalg_perid,ref=ref_perid,
                   bestalg2 = pbestalg2,
                   bestalg3 = pbestalg3,
                   bestalg4 = pbestalg4,
                   bestalg5 = pbestalg5,
                   bestalg6 = pbestalg6
)

pred$in3 <- 0
for( i in 1:length(pred$id)){
  pred$in3[i] <- (as.character(pred$pred[i]) == as.character(pred$ref[i]) | 
                       as.character(pred$pred[i]) == as.character(pred$bestalg2[i]) |
                       as.character(pred$pred[i]) == as.character(pred$bestalg3[i]) |
                       as.character(pred$pred[i]) == as.character(pred$bestalg4[i]) |
                       as.character(pred$pred[i]) == as.character(pred$bestalg5[i]) |
                       as.character(pred$pred[i]) == as.character(pred$bestalg6[i])
  ) 
}

sum(pred$in3, na.rm=T)/length(pred$id)
