getwd()
setwd('/Users/guiran/Google Drive/Case_Competition/Dataset_csv')

df <- read.csv('healcare_cleaned v4.csv')

library(randomForest)
library(AUC)

make.data = function(N=1000) {
  X = data.frame(replicate(6,rnorm(N))) #six features
  y = X[,1]^2+sin(X[,2]) + rnorm(N)*1 #some hidden data structure to learn
  rare.class.prevalence = 0.1
  y.class = factor(y<quantile(y,c(rare.class.prevalence))) #10% TRUE, 90% FALSE
  return(data.frame(X,y=y.class))
}

#make some data structure
train.data = make.data()
########################################3
#Split Data
set.seed(123)
inTrain <- sample(1:nrow(df),0.7*nrow(df))
dfTrain <- data.frame(df[inTrain,])
dfTest <- data.frame(df[-inTrain,])
#1 - Balancing by voting rule, AUC of ROC will be unchanged...
rare.class.prevalence = 0.1
rf.cutoff = randomForest(df$Heart_Disease~.,data=df,mtry = 5, importance = TRUE,maxnodes=12, ntree=100,cutoff=c(1-rare.class.prevalence,rare.class.prevalence))
print(rf.cutoff)

#2 - Balancing by sampling stratification
nRareSamples = 139092 * rare.class.prevalence
rf.strata = randomForest(df$Heart_Disease~.,data=df,strata=df$Heart_Disease,
                         sampsize=c(nRareSamples,nRareSamples))
print(rf.strata)

#3 - Balancing by class-weight during training.
rf.classwt = randomForest(y~.,data=train.data,classwt=c(0.0005,1000))
print(rf.classwt)

#view OOB-CV specificity and sensitiviy
plot(roc(rf.cutoff$votes[,2],train.data$y),main="black default, red stata, green classwt")
plot(roc(rf.strata$votes[,2],train.data$y),col=2,add=T)
plot(roc(rf.classwt$votes[,2],train.data$y),col=3,add=T)


#make test.data and remove random sample until both classes are equally prevalent
test.data = make.data(N=50000)
test.data.balanced = test.data[-sample(which(test.data$y=="FALSE"))[1:40000],]

#print prediction performance %predicted correct:
sapply(c("rf.cutoff","rf.strata","rf.classwt"),function(a.model) {
  mean(test.data.balanced$y == predict(get(a.model), newdata=test.data.balanced))
})
