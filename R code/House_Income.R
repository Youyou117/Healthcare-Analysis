df2 <- read.csv('healcare_cleaned v5.csv')
#logistic regression
str(df2)
#split the data
set.seed(71923)
splitrule <- sample(nrow(df2), 0.6*nrow(df2))
df <- data.frame(df2[splitrule,])
dft <- data.frame(df2[-splitrule,])
#logistic regression
fit <- glm(formula = Total_Household_Income ~ ., family = "binomial", data = df)
summary(fit)
library(caret)
#predict & confusion Matrix
predicted0 <- predict(fit, newdata = dft,type = "response")
pred0 <- ifelse(predicted0>0.5,1,0)
hi_t <- dft[,'Total_Household_Income']
(c = table(hi_t,pred0))
(acc = (c[1,1]+c[2,2])/sum(c))

