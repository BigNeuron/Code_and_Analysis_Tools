library(ggbiplot)
library(pcaMethods)

df <- read.csv('./ImageQ_projImage.csv')
data <- df[,c(13:28,39)]
data <- df[,c(13:28)]
data$ImageQuality_Scaling_IQ <- NULL
data$ImageQuality_Scaling_DNA <- NULL
data$ImageQuality_Scaling_DNA <- NULL
data <- remove_missing(data)

df$ids <- sapply(strsplit(as.character(df$FileName_DNA),"[.]"), "[", 1)
# save(df,file='df_iq.Rdata')
dfiq <- df
# save(dfiq,file='../../../Both_GS_and_auto/df_iq.Rdata')

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
