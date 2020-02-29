df2 <- read.csv('healcare_cleaned v3.csv')
#logistic regression
str(df2)
#split the data
set.seed(71923)
splitrule <- sample(nrow(df2), 0.6*nrow(df2))
df <- data.frame(df2[splitrule,])
dft <- data.frame(df2[-splitrule,])
#logistic regression
fit <- glm(formula = Heart_Disease ~ ., family = "binomial", data = df)
summary(fit)
library(caret)
#predict & confusion Matrix
predicted0 <- predict(fit, type = "response")
pred0 <- ifelse(predicted0>0.5,'YES','NO')
confusionMatrix(reference=factor(df$Heart_Disease), data=factor(pred0), positive='YES')

#coefficient
length(df$Heart_Disease)
length(pred0)
coef(fit)

df2$Year <- factor(df2$Year)
str(df2)



