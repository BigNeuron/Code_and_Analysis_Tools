library(ggbiplot)
library(pcaMethods)

df <- read.csv('./IQ_3DImage.csv')
df$ids <- sapply(strsplit(as.character(df$FileName_DNA),"[.]"), "[", 1)
data <- df[,c(13:28,39)]
data <- df[,c(13:28)]
data$ImageQuality_Scaling_IQ <- NULL
# data$ImageQuality_Scaling_DNA <- NULL
# data$ImageQuality_Scaling_DNA <- NULL
data$ImageSet_ImageSet <- NULL
data <- remove_missing(data)

data3d <- data
names(data3d) <- paste0(names(data3d),"_3D")

save(df,file='df_iq3D.Rdata')
dfiq <- df
save(dfiq,file='../../../Both_GS_and_auto/df_iq3D.Rdata')



df2 <- read.csv('./ImageQ_projImage.csv')
df2$ids <- sapply(strsplit(as.character(df2$FileName_DNA),"[.]"), "[", 1)
data <- df2[,c(13:28)]
data$ImageQuality_Scaling_IQ <- NULL
data$ImageQuality_Scaling_DNA <- NULL
# data$ImageQuality_Scaling_DNA <- NULL
data$ImageQuality_PowerLogLogSlope_DNA <- NULL
data <- remove_missing(data)

data <- data[df2$ids %in% df$ids,]

data <- cbind(data,data3d)

pca <- prcomp(data,
              center = TRUE,
              scale. = TRUE)

print(pca)
plot(pca)
summary(pca)

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              labels.size = 15,
              varname.size = 4,
              # groups = groups, 
              ellipse = TRUE, 
              #var.axes = FALSE,
              circle = FALSE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'vertical', 
               legend.position = 'right') +
  theme_minimal() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=17)) +
  theme(axis.title.y = element_text(size=17)) +
  theme(title = element_text(size=20)) +
  xlim(-7,5) +
  labs(title = 'Image quality metrics Gold 166')
print(g)
