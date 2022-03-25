library(ggplot2)
library(ggpubr)
library(mclust)
library(factoextra)
library(cluster)
library(caret)

load('shiny_app/clustdat_dend.Rdata')
cdat_dend <- cdat
load('shiny_app/clustdat_iq_3D.Rdata')
cdat_iq <- cdat
load('shiny_app/clustdat_both_3D.all.Rdata')
cdat_both <- cdat

vardf <- nearZeroVar(cdat_both, saveMetrics = T)
cdat_both <- cdat_both[vardf$percentUnique>90]

###############
BIC <- Mclust(scale(cdat),G=1:20)
BIC <- Mclust(scale(cdat), modelNames=c("EII", "VII", "EEE", "VVV"), G=1:15)
# save(cdat,file="clustdat.Rdata")
print(BIC$classification)
print(BIC$modelName)
memb <- BIC$classification
plot(mclustBIC(scale(cdat),modelNames=c("EII", "VII", "EEE", "VVV")))
##############

plot(mclustBIC(scale(cdat_dend),G=1:15))
plot(mclustBIC(scale(cdat_iq),G=1:15)) + abline(v = 7,col="lightblue", lwd=2, lty=2)
plot(mclustBIC(scale(cdat_both),G=1:20,modelNames=c("EII", "VII", "EEE", "VVV"),prior=priorControl())) + abline(v = 14,col="lightblue", lwd=2, lty=2)
plot(mclustBIC(cdat_both,G=1:20,modelNames=c("EII", "VII", "EEE", "VVV"))) + abline(v = 14,col="lightblue", lwd=2, lty=2)
plot(mclustBIC(scale(cdat_both),G=2:20)) + abline(v = 14,col="lightblue", lwd=2, lty=2)
plot(mclustBIC(scale(cdat_both),G=1:20,modelNames=c("EII", "VII", "EEE", "VVV"))) + abline(v = 14,col="lightblue", lwd=2, lty=2)
summary(mclustBIC(scale(cdat_both),G=2:20))
summary(mclustBIC(scale(cdat_both),G=2:20,modelNames=c("EII", "VII", "EEE", "VVV")))
# plot(mclustBIC(scale(cdat_dend)))
# plot(mclustBIC(scale(cdat_iq)))
# plot(mclustBIC(scale(cdat_both)))

hcTree <- hc(modelName = "VEV", data = cdat_iq)
plot(hcTree, what = "merge", labels = TRUE, maxG = 7)

hcTree <- hc(modelName = "EEE", data = cdat_both)
hcTree <- hc(data = cdat_both)
plot(hcTree, what = "merge", labels = TRUE, maxG = 14)

fviz_nbclust(scale(cdat_dend), kmeans, method = "gap_stat",k.max=15) +
  labs(subtitle = "Silhouette method")
fviz_nbclust(scale(cdat_iq), kmeans, method = "gap_stat",k.max=15) +
  labs(subtitle = "Silhouette method")
fviz_nbclust(scale(cdat_both), kmeans, method = "gap_stat",k.max=15) +
  labs(subtitle = "Silhouette method")
# fviz_nbclust(scale(cdat_dend), pam, method = "silhouette") +
#   labs(subtitle = "Silhouette method")
# fviz_nbclust(scale(cdat_iq), pam, method = "silhouette") +
#   labs(subtitle = "Silhouette method")
fviz_nbclust(scale(cdat_both), pam, method = "silhouette",k.max=20) +
  labs(subtitle = "Silhouette method")



# require(vegan)
# fit <- cascadeKM(scale(cdat_both, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
# plot(fit, sortg = TRUE, grpmts.plot = TRUE)
# calinski.best <- as.numeric(which.max(fit$results[2,]))
# cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 
# library("WeightedCluster")
# hc <- hclust(dist(scale(cdat_both)))
# hcRange <- as.clustrange(hc, diss=dist(scale(cdat_both)), ncluster=20) 
# summary(hcRange)
# plot(hcRange, stat = c("ASWw", "HG", "PBC"), lwd = 2)
# 
# library(pvclust)
# mydata <- t(scale(cdat_both))
# fit <- pvclust(mydata, method.hclust="ward",
#                method.dist="euclidean")
# plot(fit) # dendogram with p values
# # add rectangles around groups highly supported by the data
# pvrect(fit, alpha=.95)


hcTree <- hc(modelName = "EEE", data = cdat_both)
plot(hcTree, what = "loglik")
plot(hcTree, what = "loglik", labels = TRUE)
plot(hcTree, what = "loglik", maxG = 9, labels = TRUE)
plot(hcTree, what = "merge")
plot(hcTree, what = "merge", labels = TRUE)
plot(hcTree, what = "merge", labels = TRUE, hang = 0.1)
plot(hcTree, what = "merge", labels = TRUE, hang = -1)
plot(hcTree, what = "merge", labels = TRUE, maxG = 9)
plot(hcTree, what = "merge", labels = TRUE, maxG = 16)

# dend <- colour_clusters(hcTree, k=9, groupLabels=T)


library(factoextra)
library(MASS)
library(ggplot2)
# 