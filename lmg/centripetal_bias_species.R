library(ggplot2)
library(ggpubr)

# k <- read.csv('k_3D.csv',header=FALSE)
# L <- read.csv('L_3D.csv',header=FALSE)
# SIP <- read.csv('SIP_scale_3D.csv',header=FALSE)
# 
# k <- read.csv('k_2D.csv',header=FALSE)
# L <- read.csv('L_2D.csv',header=FALSE)
# SIP <- read.csv('SIP_scale_2D.csv',header=FALSE)

k <- read.csv('k_species.csv',header=FALSE)
L <- read.csv('L_species.csv',header=FALSE)
SIP <- read.csv('SIP_scale_species.csv',header=FALSE)


# load('shiny_app/groupsdf.Rdata')
# load('shiny_app/subsetdata.Rdata')
# total_length <- my_data$total_length[groupsdf$group=="Gold_Standard"]
# 
# groupsdf <- subset(groupsdf,groupsdf$group=="Gold_Standard")
# groupsdf$total_length <- total_length



qdeterm3d <- data.frame(k=seq(0,2,0.1),
                        qinv=c( 2,1.9983,1.9934,1.9852,1.9741,1.9602,1.9437,1.9251,1.9045,1.8824,1.8591,1.8349,1.8101,1.7849,
                1.7596,1.7344,1.7095,1.6850,1.6610,1.6376,1.6150))
qdeterm2d <- data.frame(k=seq(0,2,0.1),
                        qinv=c( 1.5708,1.5695,1.5656,1.5593,1.5507,1.5400,1.5276,1.5138,1.4987,1.4829,1.4664,1.4496,1.4327,1.4160,
                1.3994,1.3832,1.3675,1.3524,1.3378,1.3239,1.3106))

df2 <- data.frame(k = as.numeric(k$V1), q=as.numeric(L$V1)/as.numeric(SIP$V1),Species=c('Fruitfly',
                                                                                       'Human-cultured',
                                                                                       'Human',
                                                                                       'Zebrafish',
                                                                                       'Mouse',
                                                                                       'Frog',
                                                                                       'Silkmoth',
                                                                                       'Chicken')
)
# df <- data.frame(k = as.numeric(k$V1), q=1/as.numeric(SIP$V1))

# df2 <- cbind(dfids,df)#merge(dfids,df, by='ids',sort=F)


df2$Species= factor(df2$Species, levels =c('Fruitfly',
                                            'Chicken',
                                            'Zebrafish',
                                            'Silkmoth',
                                            'Frog',
                                            'Human-cultured',
                                            'Mouse',
                                            'Human'))

# df2$q <- df2$q*df2$total_length

library(ggforce)
ggplot(df2,aes(x=k,y=q)) + 
  geom_smooth(data=qdeterm3d, aes(x=qdeterm3d$k,y=qdeterm3d$qinv),span=3,se=FALSE,color='black') +
  geom_smooth(data=qdeterm2d, aes(x=qdeterm2d$k,y=qdeterm2d$qinv),span=3,se=FALSE,color='black',linetype = "dashed") +
  geom_point(aes(color=df2$Species),size=1.7) +
  stat_ellipse(aes(color=df2$Species),level=0.55) +
  xlim(0,2) +
  ylim(1.1,2.2) +
  scale_color_brewer(palette="Set2") +
  xlab('Centripetal bias k') +
  ylab('Length / SIP scale') +
  labs(color='Region') +
  theme_pubr()


# Orientation distributions


# orient <- read.csv('rootangle_dists.csv', header=FALSE)
# colnames(orient) <- 1:282
# 
# orient <- orient[,ids$id]
# colnames(orient) <- ids$region
# 
# region2 <- ids$region[which(ids$region!='')]
# 
# orient <- orient[, which(colnames(orient)!='')]
# 
# colnames(orient) <- region2
# 
# 
# df3 <- melt(t(orient))
# 
# df3 <- subset(df3,df3$Var1=='LGd' | df3$Var1=='CPU' | df3$Var1=='MG' | df3$Var1=='VPL' | df3$Var1=='VPM')
# 
# ggplot(df3, aes(x=value,fill=Var1,alpha=0.3)) + geom_density() +
#   xlab('Angle to root (soma) [rad]') + 
#   ylab('Density') +
#   labs(fill='Region')
