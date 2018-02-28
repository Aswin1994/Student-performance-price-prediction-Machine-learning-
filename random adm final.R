
library(randomForest)
library(nlme)
setwd("G:/")
Adm<-read.csv("G:/data/student/student-mat.csv",header=T,sep = ";")
names(Adm)
ttr<-Adm[,]

#table(Adm$G1)
#Adm$housing_median_age
set.seed(123)
smp_size <- floor(0.75 * nrow(ttr))
train_ind <- sample(seq_len(nrow(ttr)), size = smp_size)

trainAdm <- ttr[train_ind, ]
testAdm <- ttr[-train_ind, ]
dim(testAdm)

modelRandom<-randomForest(G3~.,data = trainAdm,mtry=6,ntree=100,na.action =na.exclude)
modelRandom

#checking important variables
importance(modelRandom)
varImpPlot(modelRandom)

rest<-predict(modelRandom,testAdm)
summary(modelRandom)
sqrt( sum( (rest - testAdm$G3)^2 , na.rm = TRUE ) / nrow(testAdm) )


###############2222
ttr2<-Adm[,-c(32,31)]
set.seed(124)
smp_size2 <- floor(0.75 * nrow(ttr2))
train_ind2 <- sample(seq_len(nrow(ttr2)), size = smp_size2)

trainAdm2 <- ttr2[train_ind2, ]
testAdm2 <- ttr2[-train_ind2, ]
dim(testAdm2)

modelRandom2<-randomForest(G3~.,data = trainAdm,mtry=6,ntree=100,na.action =na.exclude)
plot(modelRandom2)
rest2<-predict(modelRandom,testAdm)
summary(modelRandom2)
sqrt( sum( (rest2 - testAdm2$G3)^2 , na.rm = TRUE ) / nrow(testAdm2) )
###################linear 
model<-lm(G3~.,data = ttr)
model
rest2<-predict(model,testAdm)
rest2
sqrt( sum( (rest2 - testAdm$G3)^2 , na.rm = TRUE ) / nrow(testAdm) )
###################
ttr3<-Adm[,-c(33)]
set.seed(125)
smp_size3 <- floor(0.75 * nrow(ttr3))
train_ind3 <- sample(seq_len(nrow(ttr3)), size = smp_size3)
trainAdm3 <- ttr3[train_ind3, ]
testAdm3 <- ttr3[-train_ind3, ]
dim(testAdm3)
model3<-lm(G2~ G1,data = trainAdm3)
model3
rest2<-predict(model3,testAdm3)
rest2
summary(model3)
sqrt( sum( (rest2 - testAdm3$G2)^2 , na.rm = TRUE ) / nrow(testAdm3) )
#####################################
#RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

