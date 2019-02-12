
library(ggplot2)
library(reshape2)
library(corrplot)
library(gridExtra)
library(NbClust)
library(factoextra)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(rgl)
library(e1071)

#---import the data----#
chiji.origin<-read.csv("/Users/yizhou/Desktop/PUBG_Player_Statistics.csv",header=TRUE)
#----Clean the data------#
chiji.clean<-na.omit(chiji.origin)
#str(chiji.clean)


solo<-chiji.clean[,2:52]
solo<-solo[,-c(4,6,7,8,10,12,18,22,23,25,26,27,30:35,38,39,41:43,47,48,50,51)]
solo$solo_Assists<-solo$solo_Assists/solo$solo_RoundsPlayed
solo$solo_VehicleDestroys<-solo$solo_VehicleDestroys/solo$solo_RoundsPlayed
solo$solo_Boosts<-solo$solo_Boosts/solo$solo_RoundsPlayed


duo<-chiji.clean[,c(2,53:102)]
duo<-duo[,-c(4,6,7,8,10,12,21,22,23,26,27,30:35,38,41:43,47,48)]
duo$duo_Assists<-duo$duo_Assists/duo$duo_RoundsPlayed
duo$duo_Suicides<-duo$duo_Suicides/duo$duo_RoundsPlayed
duo$duo_VehicleDestroys<-duo$duo_VehicleDestroys/duo$duo_RoundsPlayed
duo$duo_Boosts<-duo$duo_Boosts/duo$duo_RoundsPlayed
duo$duo_DBNOs<-duo$duo_DBNOs/duo$duo_RoundsPlayed


squad<-chiji.clean[,c(2,103:152)]
squad<-squad[,-c(4,6,7,8,10,12,22,23,26,27,30:35,38,41:43,47,48)]
squad$squad_Assists<-squad$squad_Assists/squad$squad_RoundsPlayed
squad$squad_Suicides<-squad$squad_Suicides/squad$squad_RoundsPlayed
squad$squad_VehicleDestroys<-squad$squad_VehicleDestroys/squad$squad_RoundsPlayed
squad$squad_Boosts<-squad$squad_Boosts/squad$squad_RoundsPlayed
squad$squad_DBNOs<-squad$squad_DBNOs/squad$squad_RoundsPlayed



#-------------Part1.Visualization-----------------#
#-------Bar Plot-------#
(AveWinRatio.solo<-sum(chiji.clean$solo_Wins)/sum(chiji.clean$solo_RoundsPlayed))
(AveWinRatio.squad<-sum(chiji.clean$squad_Wins)/sum(chiji.clean$squad_RoundsPlayed))
(AveWinRatio.squad<-sum(chiji.clean$squad_Wins)/sum(chiji.clean$squad_RoundsPlayed))
(AveWinRatio.all<-(sum(chiji.clean$squad_Wins)+sum(chiji.clean$squad_Wins)+sum(chiji.clean$squad_Wins))/
    (sum(chiji.clean$squad_RoundsPlayed)+sum(chiji.clean$squad_RoundsPlayed)+sum(chiji.clean$squad_RoundsPlayed)))
PartySize<-c('1','2','4')
AveWinRatio<-c(AveWinRatio.solo,AveWinRatio.squad,AveWinRatio.squad)
mydata <-data.frame(PartySize, AveWinRatio)
p <-ggplot(mydata, aes(PartySize, AveWinRatio))
p +geom_bar(stat = "identity")

#----------Box plot------#
melt_solo<-melt(solo,id.vars = "ID")
ggplot(data=melt_solo) +
  geom_boxplot(aes(x=variable, y=value)) +
  facet_wrap(~variable, scale="free", ncol=5) +
  labs(x="solo", y="Values")

#----------Correlegram-----#
corr_matrix.solo <- cor(solo[,-1])
corrplot(corr_matrix.solo)
corrplot(corr_matrix.solo,
         method = 'number',
         type = "lower")

solo<-solo[,-c(12,13,17)]
# Solo Dimension Reduction: Rating and 

## duo
corr_matrix.duo <- cor(duo[,-1])
corrplot(corr_matrix.duo)
corrplot(corr_matrix.duo,
         method = 'number',
         type = "lower")

duo<-duo[,-c(13,14,16)]

## squad
corr_matrix.squad <- cor(squad[,-1])
corrplot(corr_matrix.squad)
corrplot(corr_matrix.squad,
         method = 'number',
         type = "lower")
squad<-squad[,-c(13,14,17)]

#-----------Dimension Reducation:PCA------#
solo.scale<-solo
solo.scale[,-1]<-data.frame(scale(solo.scale[,-1]))
solo.standard<-solo.scale
solo.standard[,-1]<-f.data.std (solo.standard[,-1])
solo.pca <- prcomp(solo.standard[,-1], center = TRUE,scale. = TRUE)
summary(solo.pca)
solo.pca$x <- -solo.pca$x
solo.pca$rotation <- -solo.pca$rotation
solo.pve <- solo.pca$sdev^2 / length(solo.pca$sdev)

duo.scale<-duo
duo.scale[,-1]<-data.frame(scale(duo.scale[,-1]))
duo.standard<-duo.scale
duo.standard[,-1]<-f.data.std (duo.standard[,-1])
duo.pca <- prcomp(duo.standard[,-1], center = TRUE,scale. = TRUE)
summary(duo.pca)
duo.pca$x <- -duo.pca$x
duo.pca$rotation <- -duo.pca$rotation
duo.pve <- duo.pca$sdev^2 / length(duo.pca$sdev)

squad.scale<-squad
squad.scale[,-1]<-data.frame(scale(squad.scale[,-1]))
squad.standard<-squad.scale
squad.standard[,-1]<-f.data.std (squad.standard[,-1])
squad.pca <- prcomp(squad.standard[,-1], center = TRUE,scale. = TRUE)
summary(squad.pca)
squad.pca$x <- -squad.pca$x
squad.pca$rotation <- -squad.pca$rotation
squad.pve <- squad.pca$sdev^2 / length(squad.pca$sdev)

##----- Scree plot and cumulative proportion of variance explained------##
solo.p1 = ggplot() +
  geom_line(aes(x=c(1:length(solo.pca$sdev)), y=solo.pve)) +
  geom_point(aes(x=c(1:length(solo.pca$sdev)), y=solo.pve), size=3) +
  geom_hline(yintercept=solo.pve[5], color='red') +
  labs(x="Principal component", y="Proportion of variance explained")

solo.p2 = ggplot() +
  geom_line(aes(x=c(1:length(solo.pca$sdev)), y=cumsum(solo.pve))) +
  geom_point(aes(x=c(1:length(solo.pca$sdev)), y=cumsum(solo.pve)), size=3) +
  geom_hline(yintercept=cumsum(solo.pve)[5], color='red') +
  annotate("text", x=23, y=0.80, label=paste(round(cumsum(solo.pve)[5],3))) +
  labs(x="Principal component", y="Cumulative proportion of variance explained")

grid.arrange(solo.p1, solo.p2, ncol=2)

# duo

duo.p1 = ggplot() +
  geom_line(aes(x=c(1:length(duo.pca$sdev)), y=duo.pve)) +
  geom_point(aes(x=c(1:length(duo.pca$sdev)), y=duo.pve), size=3) +
  geom_hline(yintercept=duo.pve[6], color='red') +
  labs(x="Principal component", y="Proportion of variance explained")

duo.p2 = ggplot() +
  geom_line(aes(x=c(1:length(duo.pca$sdev)), y=cumsum(duo.pve))) +
  geom_point(aes(x=c(1:length(duo.pca$sdev)), y=cumsum(duo.pve)), size=3) +
  geom_hline(yintercept=cumsum(duo.pve)[6], color='red') +
  annotate("text", x=23, y=0.80, label=paste(round(cumsum(duo.pve)[6],3))) +
  labs(x="Principal component", y="Cumulative proportion of variance explained")

grid.arrange(duo.p1, duo.p2, ncol=2)

# squad

squad.p1 = ggplot() +
  geom_line(aes(x=c(1:length(squad.pca$sdev)), y=squad.pve)) +
  geom_point(aes(x=c(1:length(squad.pca$sdev)), y=squad.pve), size=3) +
  geom_hline(yintercept=squad.pve[6],color='red') +
  labs(x="Principal component", y="Proportion of variance explained")

squad.p2 = ggplot() +
  geom_line(aes(x=c(1:length(squad.pca$sdev)), y=cumsum(squad.pve))) +
  geom_point(aes(x=c(1:length(squad.pca$sdev)), y=cumsum(squad.pve)), size=3) +
  geom_hline(yintercept=cumsum(squad.pve)[6], color='red') +
  annotate("text", x=23, y=0.80, label=paste(round(cumsum(squad.pve)[6],3))) +
  labs(x="Principal component", y="Cumulative proportion of variance explained")

grid.arrange(squad.p1, squad.p2, ncol=2)

##------Heatmap of nutrients and first 14 principal components------------##
melt_solo.pca <- melt(solo.pca$rotation[,c(1:11)])
colnames(melt_solo.pca) <- c("Solo", "PC", "Value")
ggplot(data=melt_solo.pca) +
  geom_tile(aes(x=PC, y=Solo, fill=Value)) +
  scale_fill_gradient2(low='blue', mid='white', high='red', midpoint=0) +
  labs(x="Principal component")

melt_duo.pca <- melt(duo.pca$rotation[,c(1:12)])
colnames(melt_duo.pca) <- c("Duo", "PC", "Value")
ggplot(data=melt_duo.pca) +
  geom_tile(aes(x=PC, y=Duo, fill=Value)) +
  scale_fill_gradient2(low='blue', mid='white', high='red', midpoint=0) +
  labs(x="Principal component")

melt_squad.pca <- melt(squad.pca$rotation[,c(1:12)])
colnames(melt_squad.pca) <- c("Squad", "PC", "Value")
ggplot(data=melt_squad.pca) +
  geom_tile(aes(x=PC, y=Squad, fill=Value)) +
  scale_fill_gradient2(low='blue', mid='white', high='red', midpoint=0) +
  labs(x="Principal component")

# ------- Full spectrum of the nutrient loadings from PC1 to PC8------
melt_solo.pca1<- melt(solo.pca$rotation[,c(1:5)])
colnames(melt_solo.pca1) = c("Solo", "PC", "Value")
ggplot(data=melt_solo.pca1) +
  geom_col(aes(x=Solo, y=Value)) +
  facet_wrap(~PC, ncol=1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Solo", y="Loading")

melt_duo.pca1<- melt(duo.pca$rotation[,c(1:6)])
colnames(melt_duo.pca1) = c("Duo", "PC", "Value")
ggplot(data=melt_duo.pca1) +
  geom_col(aes(x=Duo, y=Value)) +
  facet_wrap(~PC, ncol=1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="duo", y="Loading")

melt_squad.pca1<- melt(squad.pca$rotation[,c(1:6)])
colnames(melt_squad.pca1) = c("Squad", "PC", "Value")
ggplot(data=melt_squad.pca1) +
  geom_col(aes(x=Squad, y=Value)) +
  facet_wrap(~PC, ncol=1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="squad", y="Loading")

##----------------Selected Variables----------##
### Selection of variables
pve.mat <- matrix(rep(solo.pve, each = 5), nrow=length(solo.pve))
solo.impact <- apply(solo.pca$rotation[,c(1:5)]^2 * pve.mat, 1, sum)
melt_NI <- melt(solo.impact)
ggplot(data=melt_NI) +
  geom_col(aes(x=reorder(rownames(melt_NI), -value), y=value)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Solo", y="Variable importance")
(top10<-rownames(melt_NI)[order(-melt_NI$value)[1:10]])


pve.mat <- matrix(rep(duo.pve, each = 6), nrow=length(duo.pve))
duo.impact <- apply(duo.pca$rotation[,c(1:6)]^2 * pve.mat, 1, sum)
melt_NI <- melt(duo.impact)
ggplot(data=melt_NI) +
  geom_col(aes(x=reorder(rownames(melt_NI), -value), y=value)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="duo", y="Variable importance")
(top10<-rownames(melt_NI)[order(-melt_NI$value)[1:10]])

pve.mat <- matrix(rep(squad.pve, each = 6), nrow=length(squad.pve))
squad.impact <- apply(squad.pca$rotation[,c(1:6)]^2 * pve.mat, 1, sum)
melt_NI <- melt(squad.impact)
ggplot(data=melt_NI) +
  geom_col(aes(x=reorder(rownames(melt_NI), -value), y=value)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="squad", y="Variable importance")
(top10<-rownames(melt_NI)[order(-melt_NI$value)[1:10]])

#---------------------------#
#---------Cluster-----------#

#------------KMeans Cluster:solo-----------------#
solo.comp<-data.frame(solo.pca$x[,1:5])
k.solo<- kmeans(solo.comp, 5)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(solo.comp, col=k.solo$clust, pch=16)
k.solo$cluster
k.solo$size

# 3D plot
plot3d(solo.comp$PC1,solo.comp$PC2,solo.comp$PC3,col=k.solo$clust)
plot3d(solo.comp$PC3,solo.comp$PC4,solo.comp$PC5,col=k.solo$clust)

#--------------Disribution of Variable:solo--------------#
k.solo$cluster <- as.factor(k.solo$cluster)
ggplot(solo, aes(solo_Top10Ratio,solo_KillDeathRatio, color = k.solo$cluster)) + geom_point()
ggplot(solo, aes(solo_DamagePg,solo_KillDeathRatio, color = k.solo$cluster)) + geom_point()



#------------KMeans Cluster: Duo-----------------#
duo.comp<-data.frame(duo.pca$x[,1:6])
k.duo<- kmeans(duo.comp, 6)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(duo.comp, col=k.duo$clust, pch=16)
k.duo$cluster
k.duo$size

# 3D plot
plot3d(duo.comp$PC1,duo.comp$PC2,duo.comp$PC3,col=k.duo$clust)
plot3d(duo.comp$PC4,duo.comp$PC5,duo.comp$PC6,col=k.duo$clust)

#--------------Disribution of Variable:Duo--------------#
k.duo$cluster <- as.factor(k.duo$cluster)
ggplot(duo, aes(duo_Top10Ratio,duo_DamagePg, color = k.duo$cluster)) + geom_point()
ggplot(duo, aes(duo_KillDeathRatio,duo_DamagePg, color = k.duo$cluster)) + geom_point()


#------------KMeans Cluster: Squad-----------------#
squad.comp<-data.frame(squad.pca$x[,1:6])
k.squad<- kmeans(squad.comp, 6)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(squad.comp, col=k.squad$clust, pch=16)
k.squad$cluster
k.squad$size

# 3D plot
plot3d(squad.comp$PC1,squad.comp$PC2,squad.comp$PC3,col=k.squad$clust)
plot3d(squad.comp$PC4,squad.comp$PC5,squad.comp$PC6,col=k.squad$clust)

#--------------Disribution of Variable:Squad--------------#
k.squad$cluster <- as.factor(k.squad$cluster)
ggplot(squad, aes(squad_Top10Ratio,squad_DamagePg, color = k.squad$cluster)) + geom_point()
ggplot(squad, aes(squad_KillDeathRatio,squad_DamagePg, color = k.squad$cluster)) + geom_point()


#---------------------------------#
#----------Classification---------#
# Subset data according to Price
KDClass.solo<-solo$solo_KillDeathRatio
solo<-cbind(solo,KDClass.solo)
solo[[ "KDClass.solo"]]<-ordered(cut(as.numeric(solo[[ "KDClass.solo"]]), c(-1,1,1.5,2,3,101)),
                                 labels = c(as.numeric(1),as.numeric(2), as.numeric(3), as.numeric(4),as.numeric(5)))

KDClass.duo<-duo$duo_KillDeathRatio
duo<-cbind(duo,KDClass.duo)
duo[[ "KDClass.duo"]]<-ordered(cut(as.numeric(duo[[ "KDClass.duo"]]), c(-1,1,1.5,2,3,101)),
                               labels = c(as.numeric(1),as.numeric(2), as.numeric(3), as.numeric(4),as.numeric(5)))

KDClass.squad<-squad$squad_KillDeathRatio
squad<-cbind(squad,KDClass.squad)
squad[[ "KDClass.squad"]]<-ordered(cut(as.numeric(squad[[ "KDClass.squad"]]), c(-1,1,1.5,2,3,101)),
                                   labels = c(as.numeric(1),as.numeric(2), as.numeric(3), as.numeric(4),as.numeric(5)))

# Delete: KDRatio,Rating, LongestSurviveTime, Winpoints
solo.svm<-solo[,-c(2,6,16,17)]
duo.svm<-duo[,-c(2,6,17,19)]
squad.svm<-squad[,-c(2,6,18,20)]

# Data Slicing-solo
(n<-nrow(chiji.clean)*0.8)
set.seed(1)
train.solo.ind<-sample(nrow(solo),n, replace = FALSE, prob = NULL)
training.solo<-solo.svm[train.solo.ind,]
test.solo<-solo.svm[-train.solo.ind,]
dim(training.solo)
summary(training.solo$KDClass.solo)
dim(test.solo)
summary(test.solo$KDClass.solo)

train.duo.ind<-sample(nrow(duo),n, replace = FALSE, prob = NULL)
training.duo<-duo.svm[train.duo.ind,]
test.duo<-duo.svm[-train.duo.ind,]
dim(training.duo)
summary(training.duo$KDClass.duo)
dim(test.duo)
summary(test.duo$KDClass.duo)

train.squad.ind<-sample(nrow(squad),n, replace = FALSE, prob = NULL)
training.squad<-squad.svm[train.squad.ind,]
test.squad<-squad.svm[-train.squad.ind,]
dim(training.squad)
summary(training.squad$KDClass.squad)
dim(test.squad)
summary(test.squad$KDClass.squad)


######### Intergral Model ########
## Solo ##
set.seed(1)
train0.ind.solo<-sample(nrow(training.solo),2000, replace = FALSE, prob = NULL)
training0.solo<-training.solo[train0.ind.solo,]
training0.solo<-training0.solo[,-1]

x0.solo <- subset(training0.solo, select=-KDClass.solo)
y0.solo <- training0.solo$KDClass.solo
dat0.solo <- data.frame(x=x0.solo,y=as.factor(y0.solo))
svmfit0.solo <- svm(y~., data = dat0.solo)
tune.out0.solo <- tune(svm, y~., data = dat0.solo, kernel = "radial",
                       ranges = list(cost = c(0.1,1,10,100,1000),
                                     gamma = c(0.5,1,2,3,4)))
tune.out0.solo$best.model

training.solo<-training.solo[,-1]
x.solo <- subset(training.solo, select=-KDClass.solo)
y.solo <- training.solo$KDClass.solo
dat.solo <- data.frame(x=x.solo,y=as.factor(y.solo))
svmfit.solo <- svm(y~., data = dat.solo)
svmfit.solo <- svm(y~., data = dat.solo, kernel = "radial", cost=10,gamma=0.5)
system.time(ypred.solo<-predict(svmfit.solo,dat.solo))
(misclass.solo <- table(predict.solo = ypred.solo, truth = dat.solo$y))
(acc.solo<-(misclass.solo[1,1]+misclass.solo[2,2]+misclass.solo[3,3]+misclass.solo[4,4]+misclass.solo[5,5])/70318)


## Duo ##
set.seed(1)
train0.ind.duo<-sample(nrow(training.duo),2000, replace = FALSE, prob = NULL)
training0.duo<-training.duo[train0.ind.duo,]
training0.duo<-training0.duo[,-1]

x0.duo <- subset(training0.duo, select=-KDClass.duo)
y0.duo <- training0.duo$KDClass.duo
dat0.duo <- data.frame(x=x0.duo,y=as.factor(y0.duo))
svmfit0.duo <- svm(y~., data = dat0.duo)
tune.out0.duo <- tune(svm, y~., data = dat0.duo, kernel = "radial",
                      ranges = list(cost = c(0.1,1,10,100,1000),
                                    gamma = c(0.5,1,2,3,4)))
tune.out0.duo$best.model

training.duo<-training.duo[,-1]
x.duo <- subset(training.duo, select=-KDClass.duo)
y.duo <- training.duo$KDClass.duo
dat.duo <- data.frame(x=x.duo,y=as.factor(y.duo))
svmfit.duo <- svm(y~., data = dat.duo)
svmfit.duo <- svm(y~., data = dat.duo, kernel = "radial", cost=10,gamma=0.5)
system.time(ypred.duo<-predict(svmfit.duo,dat.duo))
(misclass.duo <- table(predict.duo = ypred.duo, truth = dat.duo$y))
(acc.duo<-(misclass.duo[1,1]+misclass.duo[2,2]+misclass.duo[3,3]+misclass.duo[4,4]+misclass.duo[5,5])/70318)

## Squad ##
set.seed(1)
train0.ind.squad<-sample(nrow(training.squad),2000, replace = FALSE, prob = NULL)
training0.squad<-training.squad[train0.ind.squad,]
training0.squad<-training0.squad[,-1]

x0.squad <- subset(training0.squad, select=-KDClass.squad)
y0.squad <- training0.squad$KDClass.squad
dat0.squad <- data.frame(x=x0.squad,y=as.factor(y0.squad))
svmfit0.squad <- svm(y~., data = dat0.squad)
tune.out0.squad <- tune(svm, y~., data = dat0.squad, kernel = "radial",
                        ranges = list(cost = c(0.1,1,10,100,1000),
                                      gamma = c(0.5,1,2,3,4)))
tune.out0.squad$best.model

training.squad<-training.squad[,-1]
x.squad <- subset(training.squad, select=-KDClass.squad)
y.squad <- training.squad$KDClass.squad
dat.squad <- data.frame(x=x.squad,y=as.factor(y.squad))
svmfit.squad <- svm(y~., data = dat.squad)
svmfit.squad <- svm(y~., data = dat.squad, kernel = "radial", cost=10,gamma=0.5)
system.time(ypred.squad<-predict(svmfit.squad,dat.squad))
(misclass.squad <- table(predict.squad = ypred.squad, truth = dat.squad$y))
(acc.squad<-(misclass.squad[1,1]+misclass.squad[2,2]+misclass.squad[3,3]+misclass.squad[4,4]+misclass.squad[5,5])/70318)

########## Test Set Prediction ##########
x.test.solo <- subset(test.solo, select=-KDClass.solo)
y.test.solo <- test.solo$KDClass.solo
dat.test.solo <- data.frame(x=x.test.solo,y=as.factor(y.test.solo))
ypred.test.solo <- predict(svmfit.solo, dat.test.solo)
(misclass.test.solo <- table(predict = ypred.test.solo, truth = dat.test.solo$y))
(t.acc.solo<-(misclass.test.solo[1,1]+misclass.test.solo[2,2]+misclass.test.solo[3,3]+misclass.test.solo[4,4]+misclass.test.solo[5,5])/17580)

x.test.duo <- subset(test.duo, select=-KDClass.duo)
y.test.duo <- test.duo$KDClass.duo
dat.test.duo <- data.frame(x=x.test.duo,y=as.factor(y.test.duo))
ypred.test.duo <- predict(svmfit.duo, dat.test.duo)
(misclass.test.duo <- table(predict = ypred.test.duo, truth = dat.test.duo$y))
(t.acc.duo<-(misclass.test.duo[1,1]+misclass.test.duo[2,2]+misclass.test.duo[3,3]+misclass.test.duo[4,4]+misclass.test.duo[5,5])/17580)

x.test.squad <- subset(test.squad, select=-KDClass.squad)
y.test.squad <- test.squad$KDClass.squad
dat.test.squad <- data.frame(x=x.test.squad,y=as.factor(y.test.squad))
ypred.test.squad <- predict(svmfit.squad, dat.test.squad)
(misclass.test.squad <- table(predict = ypred.test.squad, truth = dat.test.squad$y))
(t.acc.squad<-(misclass.test.squad[1,1]+misclass.test.squad[2,2]+misclass.test.squad[3,3]+misclass.test.squad[4,4]+misclass.test.squad[5,5])/17580)