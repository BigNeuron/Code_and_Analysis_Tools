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
  "soma_surface","num_stems","num_bifurcations","num_branches","num_of_tips",
  "overall_x_span","overall_y_span","overall_z_span","average_diameter",
  "total_length","total_surface","total_volume","max_euclidean_distance",
  "max_path_distance","max_branch_order","average_contraction",
  "average_fragmentation","parent_daughter_ratio","bifurcation_angle_local",
  "bifurcation_angle_remote","ave_R",
  "xy_pixel_size","z_pixel_size",
  # "Correlation","FocusScore","MedianIntensity","PercentMinimal","StdIntensity",
  # "Correlation_swc","FocusScore_swc","MedianIntensity_swc","PercentMinimal_swc","StdIntensity_swc",
  "Correlation","FocusScore","LocalFocusScore","MADIntensity","MaxIntensity",
  "MeanIntensity","MedianIntensity","MinIntensity","PercentMaximal",
  "PercentMinimal","PowerLogLogSlope","StdIntensity","ThresholdOtsu",
  "TotalArea","TotalIntensity",
  "Correlation_swc","FocusScore_swc","LocalFocusScore_swc","MADIntensity_swc","MaxIntensity_swc",
  "MeanIntensity_swc","MedianIntensity_swc","MinIntensity_swc","PercentMaximal_swc",
  "PercentMinimal_swc","PowerLogLogSlope_swc","StdIntensity_swc","ThresholdOtsu_swc",
  "TotalArea_swc","TotalIntensity_swc",
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

for(i in unique(ml$ids)){
  bestalg <- ml[ml$`average.of.bi.directional.entire.structure.averages` == min(ml[ml$ids==i,]$`average.of.bi.directional.entire.structure.averages`),]$algorithm
  ml[ml$ids == i,]$bestalg <- bestalg[1]
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
ppWrapper <- function( x, excludeClasses=c("factor"), ... ) {
  whichToExclude <- sapply( x, function(y) any(sapply(excludeClasses, function(excludeClass) is(y,excludeClass) )) )
  processedMat <- predict( preProcess( x[!whichToExclude], ...), newdata=x[!whichToExclude], method='BoxCox')
  x[!whichToExclude] <- processedMat
  x
}
ml <- ppWrapper(ml)
anyNA(ml)



# Create training dataset
set.seed(303)
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

ml <- na.omit(ml)
idstrain <- sample(ids, 0.7*length(ids), replace=F)
training <- ml[ml$ids %in% idstrain,]
testing <-  ml[!(ml$ids %in% idstrain),]

training <- droplevels(training)

#check dimensions of train & test set
dim(training); dim(testing);

idsml <- as.factor(ml$ids)
ml$ids <- as.factor(ml$ids)
ml$ids <- NULL

###################  Classification
tsk = makeClassifTask(data = ml, target = "bestalg", blocking = idsml)
rdesc = makeResampleDesc("Subsample", iters = 5, split = 0.8, blocking.cv = TRUE)
# rdesc = makeResampleDesc("CV", iters = 10, blocking.cv = TRUE)
# rdesc = makeResampleDesc("Bootstrap", iters = 10, blocking.cv = TRUE)
res = resample("classif.rpart", tsk,
               # parms = list(split = "information"),
               measures = list(mmce, acc),
               resampling = rdesc)

# lrn = makeLearner("classif.rpart")
# bag.lrn = makeBaggingWrapper(lrn, bw.iters = 50, bw.replace = TRUE, bw.size = 0.8, bw.feats = 3/4)
# res = resample(bag.lrn, tsk,
#                # parms = list(split = "information"),
#                measures = list(mmce, acc),
#                resampling = rdesc)

################### Tuning
# resamp = makeResampleDesc("CV", iters = 4L)
# lrn = makeLearner("classif.rpart")
# control.grid = makeTuneControlGrid() 
# ps = makeParamSet(
#   makeDiscreteParam("cp", values = seq(0.001, 0.006, 0.002)),
#   makeDiscreteParam("minsplit", values = c(1, 5, 10, 20)),
#   makeDiscreteParam("maxdepth", values = c(20, 30))
# )
# # Actual tuning, with accuracy as evaluation metric
# tuned = tuneParams(lrn, task = tsk, 
#                    # resampling = resamp, 
#                    resampling = makeFixedHoldoutInstance(which(idsml %in% idstrain), which(!(idsml %in% idstrain)), nrow(ml)),
#                    control = control.grid, 
#                    par.set = ps, measures = acc)
# 
# lrn = setHyperPars(makeLearner("classif.rpart"), cp = tuned$x$cp, minsplit = tuned$x$minsplit, maxdepth = tuned$x$maxdepth)
# 
lrn = makeLearner("classif.ksvm")
m = mlr::train(lrn, tsk, subset = which(idsml %in% idstrain))
# pred = predict(m, task = tsk, subset =  which(!(idsml %in% idstrain)))
# head(getPredictionProbabilities(pred))
reference <- as.factor(testing$bestalg)
test_pred <- predict(m, task = tsk, subset =  which(!(idsml %in% idstrain)))
test_pred <- test_pred$data$response

confusionMatrix(test_pred, reference) %>% plotCM() #check accuracy


################ Benchmarking 
# 
# # Vector of strings
# lrns = c("classif.lda", "classif.rpart", "classif.randomForest")
# 
# # A mixed list of Learner objects and strings works, too
# # lrns = list(makeLearner("classif.lda", predict.type = "prob"), "classif.rpart")
# 
# bmr = benchmark(lrns, tsk, rdesc, models = TRUE)
