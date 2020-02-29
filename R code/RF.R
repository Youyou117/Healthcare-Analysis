getwd()
setwd('/Users/guiran/Google Drive/Case_Competition/Dataset_csv')

df <- read.csv('healcare_cleaned v5.csv')
#library(dplyr)

#df <- filter(df, !Arthritis_or_Rheumatism %in% c("DON'T KNOW" , "REFUSAL"))
#df$Year <- as.factor(df$Year)
#Write CSV in R
#write.csv(df, file = "healcare_cleaned v5.csv",row.names=FALSE)

#Split Data
set.seed(123)
inTrain <- sample(1:nrow(df),0.7*nrow(df))
dfTrain <- data.frame(df[inTrain,])
dfTest <- data.frame(df[-inTrain,])

########## RF
library(randomForest)

set.seed(123)
dfTrain$Heart_Disease <- as.factor(dfTrain$Heart_Disease)
dfTest$Heart_Disease <- as.factor(dfTest$Heart_Disease)
rf_hc <- randomForest(Heart_Disease~.,data = dfTrain,mtry = 5, importance = TRUE)
#rf_hc
y_hc<- predict(rf_hc,newdata = dfTest)
hc_test.bag <- dfTest[,'Heart_Disease']
(c= table(hc_test.bag,y_hc))
(acc = (c[1,1]+c[2,2])/sum(c))

#Boosting
library(gbm)

df <- read.csv("healcare_cleaned v5.csv")
df$X <- NULL
df$Heart_Disease <- ifelse(df$Heart_Disease=='YES',1,0)
table(df$Heart_Disease)
set.seed(123)

inTrain <- sample(1:nrow(df),0.7*nrow(df))
dfTrain <- data.frame(df[inTrain,])
dfTest <- data.frame(df[-inTrain,])

boost.hc <- gbm(dfTrain$Heart_Disease~., data = dfTrain,distribution="bernoulli",n.trees=500,interaction.depth=1)
summary(boost.hc)
yhat.boost <- predict(boost.hc,newdata = dfTest, n.trees = 500,type = "response") 
predicted <- ifelse(yhat.boost>=0.5,1,0)
hc.test.boost <- dfTest[,'Heart_Disease']
(c = table(hc.test.boost,predicted))
(acc = (c[1,1]+c[2,2])/sum(c))

table(df$Province)
#test.boost <- dfTest
#test.boost$Heart_Disease <- predicted
#(profitability.boost <- sum(test.boost$Heart_Disease[test.boost$Heart_Disease == '1']))
