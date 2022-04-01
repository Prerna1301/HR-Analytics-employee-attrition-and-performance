rm(list=ls())
library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(readxl)
library(randomForest)
library(ROCR)
library(pROC)
library(ggplot2)

employee.df <- read_excel("BAwithR - Project Proposal - Part3 Description.xlsx")
employee.df <- employee.df[,-c(9,10,22,27)]
employee.df[sapply(employee.df, is.character)] <- lapply(employee.df[sapply(employee.df, is.character)], 
                                                         as.factor)
employee.df$Education <- as.factor(employee.df$Education)
employee.df$EnvironmentSatisfaction <- as.factor(employee.df$EnvironmentSatisfaction)
employee.df$JobInvolvement <- as.factor(employee.df$JobInvolvement)
employee.df$JobLevel <- as.factor(employee.df$JobLevel)
employee.df$JobSatisfaction <- as.factor(employee.df$JobSatisfaction)
employee.df$PerformanceRating <- as.factor(employee.df$PerformanceRating)
employee.df$RelationshipSatisfaction <- as.factor(employee.df$RelationshipSatisfaction)
employee.df$StockOptionLevel <- as.factor(employee.df$StockOptionLevel)
employee.df$WorkLifeBalance <- as.factor(employee.df$WorkLifeBalance)

summary(employee.df)
employee.df <- subset(employee.df, PerformanceRating == 3)

table(employee.df$Attrition)
str(employee.df)
summary(employee.df)

numberOfRows <- nrow(employee.df)
set.seed(1)
train.index <- sample(numberOfRows, numberOfRows*0.6)

print(train.index)
train.df <- employee.df[train.index, -c(22) ]
valid.df <- employee.df[-train.index, -c(22) ]
View(train.df)
View(valid.df)

# create a classification tree
.ct <- rpart(Attrition ~ ., data = train.df, method = "class", cp = 0, maxdepth = 4, minsplit = 1)

# print tree summary and plot tree. try different values for extra
printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)

t(t(.ct$variable.importance))
#plot(.ct, pch="")

rpart.plot(.ct, extra=1, cex=0.8)
# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.
ct.pred <- predict(.ct, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred, valid.df$Attrition)


rf <- randomForest(Attrition ~ ., data = train.df, 
                   ntree = 500, mtry = 4, nodesize = 1, importance = TRUE, sampsize = 500) 

#plot the variables by order of importance
varImpPlot(rf, type = 1)

#create a confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$Attrition)

#logistic Regression
logit.reg <- glm(Attrition ~., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df, type = "response"); logit.reg.pred
confusionMatrix(factor(ifelse(logit.reg.pred>=0.5, "Yes", "No")), valid.df$Attrition)

#blind guess accuracy is 83.92% as YES = 200 and NO = 1044; Total = 1244

t.df.dec <- data.frame("Predicted" = ct.pred, "Label" = as.factor(valid.df$Attrition))
t.df.dec

pred.decision <- prediction(as.numeric(t.df.dec$Predicted), as.numeric(t.df.dec$Label))
perf.dec <- performance( pred.decision, "tpr", "fpr" )

#####Random Forest

t.df.rand <- data.frame("Predicted" = rf.pred, "Label" = as.factor(valid.df$Attrition))
t.df.rand

pred.random <- prediction(as.numeric(t.df.rand$Predicted), as.numeric(t.df.rand$Label))
perf.rand <- performance( pred.random, "tpr", "fpr" )


#######Logistic Regression

t.df.log <- data.frame("Predicted" = logit.reg.pred, "Label" = as.factor(valid.df$Attrition))
t.df.log

pred.log <- prediction(t.df.log$Predicted, t.df.log$Label)
perf.log <- performance( pred.log, "tpr", "fpr" )

plot( perf.dec, lty = 3, lwd = 3, col = "orangered4")
plot( perf.rand, add = T, lty = 4, lwd = 3, col = "darkslategray4")
plot( perf.log, add = T, lty = 1, lwd = 3, col = "blue4")



legend("bottomright", 
       legend = c("Decision Tree", "Random Forest", "Logistic Regression"), 
       col = c( "orangered4", "darkslategray4", "blue4"),
       lty = c(3, 4, 1, 4, 1),
       lwd = c(3, 3, 3, 8, 1))

