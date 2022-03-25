# require(foreign)
# require(nnet)
# require(ggplot2)
# require(reshape2)
# # https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# ml_b <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

require(tidyr)
require(timeDate)
require(ggpubr)

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

# data <- na.omit(data)

# data[is.na(data)]<-0
# data[data=='-Inf']<-0
# 
# for(i in 1:length(data)){
#   if(skewness(data[,i]) > 1){
#     data[,i] <- log10(data[,i])
#   }
#   else if(skewness(data[,i]) < (-1)){
#     data[,i] <- log10(max(data[,i]+1) - data[,i])
#   }
# }
# 
# data[is.na(data)]<-0
# data[data=='-Inf']<-0
# data <- data[,apply(data, 2, var, na.rm=TRUE) != 0]

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

# ml <- ml[,c(1:46,63,65)]
# ml <- ml[,c(1:56,62,63,65)]
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

# ml <- ml[ml$algorithm==ml$bestalg,]
ml$algorithm <- NULL
##

# levels(ml$variable)[levels(ml$variable)=='average.of.bi.directional.entire.structure.averages'] <- 'av_bid_ent_str_av'
names(ml)[names(ml) == 'average.of.bi.directional.entire.structure.averages'] <- 'av_bid_ent_str_av'


#################
# ml$soma_surface_av <- 0
# ml$num_stems_av <- 0
# ml$num_bifurcations_av <- 0
# ml$num_branches_av <- 0
# ml$num_of_tips_av <- 0
# ml$overall_x_span_av <- 0
# ml$overall_y_span_av <- 0
# ml$overall_z_span_av <- 0
# ml$average_diameter_av <- 0
# ml$total_length_av <- 0
# ml$total_surface_av <- 0
# ml$total_volume_av <- 0
# ml$max_euclidean_distance_av <- 0
# ml$max_path_distance_av <- 0
# ml$max_branch_order_av <- 0
# ml$average_contraction_av <- 0
# ml$average_fragmentation_av <- 0
# ml$parent_daughter_ratio_av <- 0
# ml$bifurcation_angle_local_av <- 0
# ml$bifurcation_angle_remote_av <- 0
# ml$ave_R_av <- 0
# 
# for(i in unique(ml$ids)){
#   ml[ml$ids==i,]$soma_surface_av <- rep(mean(ml[ml$ids==i,]$soma_surface),length(ml[ml$ids==i,]$soma_surface))
#   ml[ml$ids==i,]$num_stems_av <- rep(mean(ml[ml$ids==i,]$num_stems),length(ml[ml$ids==i,]$num_stems))
#   ml[ml$ids==i,]$num_bifurcations_av <- rep(mean(ml[ml$ids==i,]$num_bifurcations),length(ml[ml$ids==i,]$num_bifurcations))
#   ml[ml$ids==i,]$num_branches_av <- rep(mean(ml[ml$ids==i,]$num_branches),length(ml[ml$ids==i,]$num_branches))
#   ml[ml$ids==i,]$num_of_tips_av <- rep(mean(ml[ml$ids==i,]$num_of_tips),length(ml[ml$ids==i,]$num_of_tips))
#   ml[ml$ids==i,]$overall_x_span_av <- rep(mean(ml[ml$ids==i,]$overall_x_span),length(ml[ml$ids==i,]$overall_x_span))
#   ml[ml$ids==i,]$overall_y_span_av <- rep(mean(ml[ml$ids==i,]$overall_y_span),length(ml[ml$ids==i,]$overall_y_span))
#   ml[ml$ids==i,]$overall_z_span_av <- rep(mean(ml[ml$ids==i,]$overall_z_span),length(ml[ml$ids==i,]$overall_z_span))
#   ml[ml$ids==i,]$average_diameter_av <- rep(mean(ml[ml$ids==i,]$average_diameter),length(ml[ml$ids==i,]$average_diameter))
#   ml[ml$ids==i,]$total_length_av <- rep(mean(ml[ml$ids==i,]$total_length),length(ml[ml$ids==i,]$total_length))
#   ml[ml$ids==i,]$total_surface_av <- rep(mean(ml[ml$ids==i,]$total_surface),length(ml[ml$ids==i,]$total_surface))
#   ml[ml$ids==i,]$total_volume_av <- rep(mean(ml[ml$ids==i,]$total_volume),length(ml[ml$ids==i,]$total_volume))
#   ml[ml$ids==i,]$max_euclidean_distance_av <- rep(mean(ml[ml$ids==i,]$max_euclidean_distance),length(ml[ml$ids==i,]$max_euclidean_distance))
#   ml[ml$ids==i,]$max_path_distance_av <- rep(mean(ml[ml$ids==i,]$max_path_distance),length(ml[ml$ids==i,]$max_path_distance))
#   ml[ml$ids==i,]$max_branch_order_av <- rep(mean(ml[ml$ids==i,]$max_branch_order),length(ml[ml$ids==i,]$max_branch_order))
#   ml[ml$ids==i,]$average_contraction_av <- rep(mean(ml[ml$ids==i,]$average_contraction),length(ml[ml$ids==i,]$average_contraction))
#   ml[ml$ids==i,]$average_fragmentation_av <- rep(mean(ml[ml$ids==i,]$average_fragmentation),length(ml[ml$ids==i,]$average_fragmentation))
#   ml[ml$ids==i,]$parent_daughter_ratio_av <- rep(mean(ml[ml$ids==i,]$parent_daughter_ratio),length(ml[ml$ids==i,]$parent_daughter_ratio))
#   ml[ml$ids==i,]$bifurcation_angle_local_av <- rep(mean(ml[ml$ids==i,]$bifurcation_angle_local),length(ml[ml$ids==i,]$bifurcation_angle_local))
#   ml[ml$ids==i,]$bifurcation_angle_remote_av <- rep(mean(ml[ml$ids==i,]$bifurcation_angle_remote),length(ml[ml$ids==i,]$bifurcation_angle_remote))
#   ml[ml$ids==i,]$ave_R_av <- rep(mean(ml[ml$ids==i,]$ave_R),length(ml[ml$ids==i,]$ave_R))
# }
# 
# 
# ml$soma_surface <- NULL
# ml$num_stems <- NULL
# ml$num_bifurcations <- NULL
# ml$num_branches <- NULL
# ml$num_of_tips <- NULL
# ml$overall_x_span <- NULL
# ml$overall_y_span <- NULL
# ml$overall_z_span <- NULL
# ml$average_diameter <- NULL
# ml$total_length <- NULL
# ml$total_surface <- NULL
# ml$total_volume <- NULL
# ml$max_euclidean_distance <- NULL
# ml$max_path_distance <- NULL
# ml$max_branch_order <- NULL
# ml$average_contraction <- NULL
# ml$average_fragmentation <- NULL
# ml$parent_daughter_ratio <- NULL
# ml$bifurcation_angle_local <- NULL
# ml$bifurcation_angle_remote <- NULL
# ml$ave_R <- NULL

##################

# with(ml, table(variable, algorithm))
# with(ml, do.call(rbind, tapply(av_bid_ent_str_av, algorithm, function(x) c(M = mean(x), SD = sd(x)))))

# https://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
library(caret)
library(rpart.plot)

# Normalize dataset
# ml[,1:43] <- scale(ml[,1:43])
anyNA(ml)

ml$group <- NULL
ml$dataset <- NULL
# ml$algorithm <- NULL
ids <- unique(ml$ids)
# ml$ids <- NULL
# ml$entire.structure.average..from.neuron.1.to.2.<- NULL
# ml$entire.structure.average..from.neuron.2.to.1.<- NULL
# ml$av_bid_ent_str_av <- NULL
# ml$different.structure.average<- NULL
# ml$percent.of.different.structure..from.neuron.1.to.2.<- NULL
# ml$percent.of.different.structure..from.neuron.2.to.1.<- NULL
# ml$percent.of.different.structure<- NULL

ml$soma_surface <- NULL
ml$soma_surface_av <- NULL

# only average features
ml <- unique(ml)

# all features
# ml[,c(1:52,54)]

# ml <- droplevels(ml)
#' Function which wraps preProcess to exclude factors from the model.matrix
ppWrapper <- function( x, excludeClasses=c("factor"), ... ) {
  whichToExclude <- sapply( x, function(y) any(sapply(excludeClasses, function(excludeClass) is(y,excludeClass) )) )
  processedMat <- predict( preProcess( x[!whichToExclude], ...), newdata=x[!whichToExclude], method='BoxCox')
  x[!whichToExclude] <- processedMat
  x
}
ml <- ppWrapper(ml)
anyNA(ml)

# ml[,1:43] <- scale(ml[,1:43])

# Create training dataset
set.seed(303)
ml <- ml[sample(1:nrow(ml)), ]
######################
######################
# ml[,c(21,22,24:27)]<-NULL
ml$entire.structure.average..from.neuron.1.to.2. <- NULL
ml$entire.structure.average..from.neuron.2.to.1. <- NULL
ml$different.structure.average <- NULL
ml$percent.of.different.structure..from.neuron.1.to.2. <- NULL
ml$percent.of.different.structure..from.neuron.2.to.1. <- NULL
ml$percent.of.different.structure <- NULL
ml$av_bid_ent_str_av <- NULL
# ml$bestalg <- NULL

ml <- na.omit(ml)
idstrain <- sample(ids, 0.7*length(ids), replace=F)
training <- ml[ml$ids %in% idstrain,]
testing <-  ml[!(ml$ids %in% idstrain),]

# intrain <- createDataPartition(y = ml$bestalg, p= 0.5, list = FALSE)
# training <- ml[intrain,]
# testing <- ml[-intrain,]
# training$ids<-NULL
# testing$ids<-NULL


training <- droplevels(training)

#check dimensions of train & test set
dim(training); dim(testing);
# 
# 
# bestalg <- NULL
# training$bestalg <- as.factor(training$bestalg)
# 
# ## Imputation model
# # preProcess_missingdata_model <- preProcess(training, method='knnImpute')
# # library(RANN)  # required for knnInpute
# # training <- predict(preProcess_missingdata_model, newdata = training)
# # anyNA(training)
# 
# # Training
# library(caretEnsemble)
# 
# # Stacking Algorithms - Run multiple algos in one call.
# trainControl <- trainControl(method="repeatedcv",
#                              number=10,
#                              repeats=3,
#                              savePredictions=TRUE,
#                              classProbs=TRUE)
# 
# # algorithmList <- c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial','rpart', 'rpart2', 'knn', 'treebag','nnet')
# algorithmList <- c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial')
# # algorithmList <- c('rpart', 'rpart2', 'knn', 'rf', 'treebag','nnet')
# #
# 
# set.seed(3033)
# models <- caretList(av_bid_ent_str_av ~ ., data=training, trControl=trainControl, methodList=algorithmList,continue_on_fail=T)
# results <- resamples(models)
# summary(results)
# 
# # Box plots to compare models
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# bwplot(results, scales=scales)
# 
# 
# 
# # Create the trainControl
# set.seed(101)
# stackControl <- trainControl(method="repeatedcv",
#                              number=10,
#                              repeats=3,
#                              savePredictions=TRUE,
#                              classProbs=TRUE)
# 
# # Ensemble the predictions of `models` to form a new combined prediction based on glm
# stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
# print(stack.glm)
# 
# 
# 
# # Plot decision tree
# # prp(dtree_fit$finalModel, box.palette = "Blues", tweak = 1.2)
# 
# # dtree_fit <- caret::train(bestalg ~., data = training, method = "rf")
# # dtree_fit <- caret::train(bestalg ~., data = training, method = "treebag")
# 
# # Predict
# # predict(dtree_fit, newdata = testing[1,])
# test_pred <- predict(models$earth, newdata = na.omit(testing))
# # test_pred <- predict(models$treebag, newdata = na.omit(testing))
# # test_pred <- predict(models$knn, newdata = na.omit(testing))
# # test_pred <- predict(nnetFit, newdata = testing)
# 
# reference <- testing$av_bid_ent_str_av
# 
# levels(test_pred)
# if(length(levels(test_pred))>length(levels(reference))){
#   levels(reference) <- levels(test_pred)
# }else{
#   levels(test_pred) <- levels(reference)
# }
# confusionMatrix(test_pred, reference) %>% plotCM() #check accuracy
# 
# confusionMatrix(data = test_pred[,1], reference = reference)
# CM <- confusionMatrix(test_pred, reference)
# CM$overall[1]
# 
# 
# 
# ##################################################
# library(dplyr)
# df <- as.data.frame(test_pred) %>%
#   group_by(test_pred) %>%
#   summarise(counts = n())
# # df
# df$pred <- "Prediction"
# 
# df2 <- as.data.frame(testing$bestalg) %>%
#   group_by(testing$bestalg) %>%
#   summarise(counts = n())
# # df2
# names(df2)[1]<-"test_pred"
# df2$pred <- "Reference"
# 
# # test_pred_table <- table(test_pred)
# # test_pred_levels <- names(test_pred_table)[order(test_pred_table)]
# # test_pred <- factor(test_pred, levels = test_pred_levels)
# # dfplot <- data.frame(test_pred=test_pred)
# 
# dfplot <- rbind(df,df2)
#   
# ggbarplot(dfplot,x="test_pred",y="counts",fill="pred",position = position_dodge(0.9))+
#   theme_pubr()

######
# trainset <- c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7)
# accuracy <- c(0.778,0.869,0.959,0.962,0.940,0.939,0.949,0.966)
# dfplot <-  data.frame(trainset,accuracy)
# 
# ggpubr::ggdotplot(dfplot,x='trainset',y='accuracy')
#########################
#########################

###################################################################
# library(mlr)
# library(FSelector)
# 
# task=makeClassifTask(data=training,target="bestalg")
# 
# learner = makeLearner("classif.nnet", predict.type = "prob")
# 
# my_theme <- function(base_size =8, base_family = "sans"){
#   theme_minimal(base_size = base_size, base_family = base_family) +
#     theme(
#       axis.text = element_text(size =8),
#       axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
#       axis.title = element_text(size =8),
#       panel.grid.major = element_line(color = "gray"),
#       panel.grid.minor = element_blank(),
#       panel.background = element_rect(fill = "#fef5f9"),
#       strip.background = element_rect(fill = "#290029", color = "#290029", size =0.5),
#       strip.text = element_text(face = "bold", size = 8, color = "white"),
#       legend.position = "bottom",
#       legend.justification = "center",
#       legend.background = element_blank(),
#       panel.border = element_rect(color = "grey5", fill = NA, size = 0.5)
#     )
# }
# theme_set(my_theme())
# 
# ig=generateFilterValuesData(task,method="FSelector_information.gain")%>%.$data%>%ggplot(aes(x=reorder(name,value),y=value,fill=reorder(name,value)))+
#   geom_bar(stat="identity",color="black",show.legend=F)+
#   scale_x_discrete("Features")+coord_flip()
# 
# # mr=generateFilterValuesData(task,method="mrmr")%>%.$data%>%ggplot(aes(x=reorder(name,mrmr),y=mrmr,fill=reorder(name,mrmr)))+geom_bar(stat="identity",color="black",show.legend=F)+scale_fill_manual(values=myfillcolors,name="mrmr")+scale_x_discrete("Features")+coord_flip()
# 
# pmi=generateFilterValuesData(task,method="permutation.importance",imp.learner=learner)%>%.$data%>%ggplot(aes(x=reorder(name,value),y=value,fill=reorder(name,value)))+
#   geom_bar(stat="identity",color="black",show.legend=F)+
#   scale_x_discrete("Features")+coord_flip()
# 
# gt=generateFilterValuesData(task,method="FSelector_gain.ratio")%>%.$data%>%ggplot(aes(x=reorder(name,value),y=value,fill=reorder(name,value)))+
#   geom_bar(stat="identity",color="black",show.legend=F)+
#   scale_x_discrete("Features")+coord_flip()
# 
# grid.arrange(ig,pmi,gt)


require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

# ml$algorithm2 <- relevel(ml$algorithm, ref = "Annotated")

idsml <- as.factor(ml$ids)
# ml$ids <- as.numeric(ml$ids)
# ml$ids <-NULL
# ml$av_bid_ent_str_av <- NULL
# test <- train(bestalg ~ ., data=ml, maxit=1500, method = "nnet")

library(doMC)
registerDoMC(6)
t.grid=expand.grid(size=5,decay=1e-4)
# model <- train(bestalg ~ ., data=training, maxit=1000, method = "nnet",tuneGrid=t.grid)

library(mlr)
mltrain <- ml
# mltrain$ids <-NULL
ml$ids <- NULL
resDesc <- makeResampleDesc("CV",iters=10,blocking.cv = TRUE)
tsk = makeClassifTask(data = ml, target = "bestalg", blocking = idsml)
# tsk = makeRegrTask(data = ml, target = "av_bid_ent_str_av", blocking = idsml)
learners <- listLearners(tsk, check.packages = FALSE)
head(learners[,c('class', 'package')])

learner = makeLearner(cl = "classif.cvglmnet",  # class of learner
                         predict.type = "prob", # prediction output type
                         par.vals = list(), # set hyperparameters
                         predict.threshold = NULL, # threshold for prediction
                         fix.factors.prediction = TRUE,
                         id = 'c rpart') # add a factor level for missing data in test

# learner = makeLearner(cl = "regr.bcart",  # class of learner
#                       predict.type = "response", # prediction output type
#                       par.vals = list(), # set hyperparameters
#                       predict.threshold = NULL, # threshold for prediction
#                       fix.factors.prediction = TRUE,
#                       id = 'c rpart') # add a factor level for missing data in test
# learner
# bc_model <- mlr::train(learner = learner, 
#                   task = tsk)
# model <- train(learner = learner, 
#                task = tsk,
#                subset = which(mltrain$ids %in% idstrain))
# 
# pred <- predict(object = model,
#                 task = tsk, 
#                 subset = which(!(mltrain$ids %in% idstrain)))



res = resample("classif.cvglmnet", tsk, resampling = resDesc)
performance(res$pred, acc)
# performance(res$pred, rmse)
# 
# 
# model <- train(bestalg ~ num_stems + num_bifurcations + num_branches + num_of_tips + overall_x_span +
#                  overall_y_span + overall_z_span + average_diameter +
#                  total_length + total_surface + total_volume +
#                  max_euclidean_distance + max_path_distance + max_branch_order +
#                  average_contraction + average_fragmentation + parent_daughter_ratio +
#                  bifurcation_angle_local + bifurcation_angle_remote +
#                  ave_R + Correlation_swc +
#                  FocusScore_swc + LocalFocusScore_swc +
#                  MADIntensity_swc + MaxIntensity_swc +
#                  MeanIntensity_swc + MedianIntensity_swc +
#                  MinIntensity_swc + PercentMaximal_swc +
#                  PercentMinimal_swc + PowerLogLogSlope_swc +
#                  StdIntensity_swc + ThresholdOtsu_swc +
#                  TotalArea_swc + TotalIntensity_swc +
#                  xy_pixel_size + z_pixel_size, blocking=idsml, data=training, method = "treebag")
# 
# reference <- as.factor(testing$bestalg)
# test_pred <- predict(model, newdata = testing)
# 
# levels(test_pred)
# if(length(levels(test_pred))>length(levels(reference))){
#   levels(reference) <- levels(test_pred)
# }else{
#   levels(test_pred) <- levels(reference)
# }
# confusionMatrix(test_pred, reference) %>% plotCM() #check accuracy
# 
# # confusionMatrix(data = test_pred[,1], reference = reference)
# CM <- confusionMatrix(test_pred, reference)
# CM$overall[1]





# test_summ <- summary(test)
# # z <- summary(test)$coefficients/summary(test)$standard.errors
# z <- test_summ$coefficients/test_summ$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# 
# ## extract the coefficients from the model and exponentiate
# exp(coef(test))
# 
# head(pp <- fitted(test))
# 
# ## melt data set to long for ggplot2
# lpp <- melt(bestalg, id.vars = names(pp), value.name = "probability")
# head(lpp)  # view first few rows
# 
# ## plot predicted probabilities across write values for each level of ses
# ## facetted by program type
# ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~
#                                                                   ., scales = "free")
