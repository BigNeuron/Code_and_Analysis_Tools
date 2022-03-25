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
ml$bestalg7 <- ml$algorithm
ml$bestalg8 <- ml$algorithm

ml$worstalg <- ml$algorithm
ml$worstalg2 <- ml$algorithm
ml$worstalg3 <- ml$algorithm
ml$worstalg4 <- ml$algorithm
ml$worstalg5 <- ml$algorithm

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
  bestalg7 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4 & ml$algorithm!=bestalg5 & ml$algorithm!=bestalg6,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg7 <- bestalg7[1]
  bestalg8 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i & ml$algorithm!=bestalg & ml$algorithm!=bestalg2 & ml$algorithm!=bestalg3 & ml$algorithm!=bestalg4 & ml$algorithm!=bestalg5 & ml$algorithm!=bestalg6 & ml$algorithm!=bestalg7,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg8 <- bestalg8[1]
  
  worstalg <- ml[ml$`average.of.bi.directional.entire.structure.averages` == max(ml[ml$ids==i,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$worstalg <- worstalg[1]
  worstalg2 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == max(ml[ml$ids==i & ml$algorithm!=worstalg,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$worstalg2 <- worstalg2[1]
  worstalg3 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == max(ml[ml$ids==i & ml$algorithm!=worstalg & ml$algorithm!=worstalg2,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$worstalg3 <- worstalg3[1]
  worstalg4 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == max(ml[ml$ids==i & ml$algorithm!=worstalg & ml$algorithm!=worstalg2 & ml$algorithm!=worstalg3,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$worstalg4 <- worstalg4[1]
  worstalg5 <- ml[ml$`average.of.bi.directional.entire.structure.averages` == max(ml[ml$ids==i & ml$algorithm!=worstalg & ml$algorithm!=worstalg2 & ml$algorithm!=worstalg3 & ml$algorithm!=worstalg4,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$worstalg5 <- worstalg5[1]
}

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
set.seed(7) 
ml <- ml[sample(1:nrow(ml)), ]
######################
######################
ml$entire.structure.average..from.neuron.1.to.2. <- NULL
ml$entire.structure.average..from.neuron.2.to.1. <- NULL
ml$different.structure.average <- NULL
ml$percent.of.different.structure..from.neuron.1.to.2. <- NULL
ml$percent.of.different.structure..from.neuron.2.to.1. <- NULL
ml$percent.of.different.structure <- NULL
ml$av_bid_ent_str_av <- NULL
# ml$bestalg <- NULL
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

ml <- na.omit(ml)
idstrain <- sample(ids, 0.8*length(ids), replace=F)
training <- ml[ml$ids %in% idstrain,]
testing <-  ml[!(ml$ids %in% idstrain),]

training <- droplevels(training)

#check dimensions of train & test set
dim(training); dim(testing);

idsml <- as.factor(ml$ids)
ml$ids <- as.factor(ml$ids)
ml$ids <- NULL

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

###################  Classification
tsk = makeClassifTask(data = ml, target = "bestalg", blocking = idsml)
# rdesc = makeResampleDesc("Subsample", iters = 5, split = 0.85, blocking.cv = TRUE)
# # rdesc = makeResampleDesc("CV", iters = 10, blocking.cv = TRUE)
# # rdesc = makeResampleDesc("Bootstrap", iters = 10, blocking.cv = TRUE)
# res = resample(makePreprocWrapperCaret("classif.ctree", ppc.expoTrans = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.6), 
#                tsk,
#                # parms = list(split = "information"),
#                measures = list(mmce, acc),
#                resampling = rdesc)

# lrn = makeLearner("classif.rpart")
# bag.lrn = makeBaggingWrapper(lrn, bw.iters = 50, bw.replace = TRUE, bw.size = 0.8, bw.feats = 3/4)
# res = resample(bag.lrn, tsk,
#                # parms = list(split = "information"),
#                measures = list(mmce, acc),
#                resampling = rdesc)

################### Tuning
resamp = makeResampleDesc("CV", iters = 4L, blocking.cv = TRUE)
lrn = makeLearner("classif.svm") #PART rpart svm
# control.grid = makeTuneControlGrid()
# ps = makeParamSet(
#   makeDiscreteParam("cp", values = seq(0.005, 0.013, 0.002)),
#   makeDiscreteParam("minsplit", values = c(1, 2, 3, 4, 5)),
#   makeDiscreteParam("maxdepth", values = c(5, 10, 20, 30))
# )
# 
# kernels <- c("polynomial", "radial", "sigmoid")
# ps<- makeParamSet(
#   makeDiscreteParam("kernel", values = kernels),
#   makeIntegerParam("degree", lower = 1, upper = 3),
#   makeNumericParam("cost", lower = 0.1, upper = 10),
#   makeNumericParam("gamma", lower = 0.1, 10))
# # ps = makeParamSet(
# #   makeDiscreteParam("C", values = seq(0.05, 0.35, 0.05)),
# #   makeDiscreteParam("M", values = c(1, 2, 3, 4, 5))
# # )
# # # ps <- makeParamSet(makeIntegerParam("mtry",lower = 0,upper = 10),makeIntegerParam("nodesize",lower = 0,upper = 50))
# # Actual tuning, with accuracy as evaluation metric
# library(parallelMap)
# library(parallel)
# 
# parallelStartSocket(cpus = detectCores()-2)
# tuned = tuneParams(lrn, task = tsk,
#                    resampling = resamp,
#                    # resampling = makeFixedHoldoutInstance(which(idsml %in% idstrain), which(!(idsml %in% idstrain)), nrow(ml)),
#                    # control = control.grid,
#                    control = makeTuneControlRandom(maxit = 20),
#                    par.set = ps,
#                    measures = acc)
# parallelStop()
# 
# # lrn = setHyperPars(makeLearner("classif.rpart"), cp = tuned$x$cp, minsplit = tuned$x$minsplit, maxdepth = tuned$x$maxdepth)
# lrn = setHyperPars(makeLearner("classif.svm"), kernel = tuned$x$kernel,
#                    degree = tuned$x$degree, cost = tuned$x$cost, gamma = tuned$x$cost)


################# 5 best algorithms
# lrn = makeLearner("classif.rpart") #svm ctree 
# lrn = setHyperPars(makeLearner("classif.treebag"), maxit = 1500)
# lrn = setHyperPars(makeLearner("classif.ctree"), mtry = tuned$x$mtry, maxdepth = tuned$x$maxdepth)
# lrn = setHyperPars(makeLearner("classif.ctree"), mtry = 0, maxdepth = 20)
# lrn = setHyperPars(makeLearner("classif.rpart"), cp = 0.009, minsplit = 3, maxdepth = 20)
# lrn = makeBaggingWrapper(lrn, bw.iters = 50, bw.replace = TRUE, bw.size = 0.8, bw.feats = 3/4)
lrn = makePreprocWrapperCaret(lrn, ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7)
# base.learners = list(
#   # lrn1,
#   # makePreprocWrapperCaret( makeLearner("classif.PART"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#   # makePreprocWrapperCaret( makeLearner("classif.ksvm"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#   makePreprocWrapperCaret( makeLearner("classif.randomForest"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#   makePreprocWrapperCaret( makeLearner("classif.ctree"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#   # makePreprocWrapperCaret( makeLearner("classif.nnet"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#   makePreprocWrapperCaret( makeLearner("classif.svm"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#   makePreprocWrapperCaret( makeLearner("classif.kknn"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#   makePreprocWrapperCaret( makeLearner("classif.extraTrees"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7)
# )
# lrn = makeStackedLearner(base.learners, super.learner = NULL, predict.type = "response",
#                          method = "average", use.feat = FALSE, resampling = NULL,
#                          parset = list())

m = mlr::train(lrn, tsk, subset = which(idsml %in% idstrain))
# pred = predict(m, task = tsk, subset =  which(!(idsml %in% idstrain)))
# head(getPredictionProbabilities(pred))
reference <- data.frame(bestalg = as.factor(testing$bestalg), 
                        bestalg2 = testing$bestalg2, 
                        bestalg3 = testing$bestalg3, 
                        bestalg4 = testing$bestalg4, 
                        bestalg5 = testing$bestalg5, 
                        bestalg6 = testing$bestalg6, 
                        bestalg7 = testing$bestalg7, 
                        bestalg8 = testing$bestalg8,
                        worstalg = testing$worstalg,
                        worstalg2 = testing$worstalg2, 
                        worstalg3 = testing$worstalg3, 
                        worstalg4 = testing$worstalg4, 
                        worstalg5 = testing$worstalg5)
test_pred <- predict(m, task = tsk, subset =  which(!(idsml %in% idstrain)))
# test_pred <- test_pred$data$response

# testref <- cbind(test_pred$data,reference)
response <- test_pred
testref <- cbind(response,reference)
testref$in3 <- 0
testref$worst5 <- 0
for( i in 1:length(testref$id)){
  testref$in3[i] <- (as.character(testref$response[i]) == as.character(testref$bestalg[i]) | 
                       as.character(testref$response[i]) == as.character(testref$bestalg2[i]) |
                       as.character(testref$response[i]) == as.character(testref$bestalg3[i]) |
                       as.character(testref$response[i]) == as.character(testref$bestalg4[i]) |
                       as.character(testref$response[i]) == as.character(testref$bestalg5[i]) #|
                       # as.character(testref$response[i]) == as.character(testref$bestalg6[i]) |
                       # as.character(testref$response[i]) == as.character(testref$bestalg7[i]) | 
                       # as.character(testref$response[i]) == as.character(testref$bestalg8[i]) 
                     ) 
  testref$worst5[i] <-(as.character(testref$response[i]) == as.character(testref$worstalg[i]) |
                        as.character(testref$response[i]) == as.character(testref$worstalg2[i]) |
                        as.character(testref$response[i]) == as.character(testref$worstalg3[i]) |
                        as.character(testref$response[i]) == as.character(testref$worstalg4[i]) |
                        as.character(testref$response[i]) == as.character(testref$worstalg5[i])
                      )
}

print("Probability prediction in 5 best algorithms:")
sum(testref$in3, na.rm=T)/length(testref$id)

print("Probability prediction in 5 worst algorithms:")
sum(testref$worst5, na.rm=T)/length(testref$id)

# confusionMatrix(test_pred, reference) %>% plotCM() #check accuracy


################ Benchmarking 
# rdesc = makeResampleDesc("Subsample", iters = 5, split = 0.85, blocking.cv = TRUE)
# # Vector of strings
# lrns = list(makePreprocWrapperCaret( makeLearner("classif.svm"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             # makePreprocWrapperCaret( makeLearner("classif.ksvm"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             makePreprocWrapperCaret( makeLearner("classif.ctree"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             # makePreprocWrapperCaret( makeLearner("classif.nnet"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#             # makePreprocWrapperCaret( makeLearner("classif.rpart"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          makePreprocWrapperCaret( makeLearner("classif.randomForest"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          # makePreprocWrapperCaret( makeLearner("classif.PART"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          makePreprocWrapperCaret( makeLearner("classif.extraTrees"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          # makePreprocWrapperCaret( makeLearner("classif.glmnet"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7),
#          makePreprocWrapperCaret( makeLearner("classif.kknn"), ppc.BoxCox = TRUE, ppc.corr = TRUE, ppc.cutoff = 0.7))
# 
# # A mixed list of Learner objects and strings works, too
# # lrns = list(makeLearner("classif.lda", predict.type = "prob"), "classif.rpart")
# 
# bmr = benchmark(lrns, tsk, rdesc, models = TRUE, measures = acc)
