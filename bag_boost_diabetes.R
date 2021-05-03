
###############Start of code##########
diabetes <- read.csv("C:\\Users\\Bibhuti\\OneDrive\\Desktop\\360digiTMG assignment\\Ensemble technique\\Datasets_ET\\Diabeted_Ensemble.csv")
diabetes$Class.variable <-as.factor(diabetes$Class.variable)
View(diabetes)
str(diabetes)
#Accuracy with single model
library(caret)
inTraininglocal <- createDataPartition(diabetes$Class.variable, p = 0.75, list = F)
training <- diabetes[inTraininglocal, ]
testing <- diabetes[-inTraininglocal, ]

install.packages("C50")
library(C50)
model <- C5.0(training$ Class.variable ~ ., data = training[, -9])
plot(model)
pred <- predict.C5.0(model, testing[, -9])
a <- table(testing$ Class.variable, pred)

sum(diag(a))/sum(a)

#*****************************************************************
########Bagging
acc <- c()
for(i in 1:11)
{
  inTraininglocal <- createDataPartition(diabetes$ Class.variable, p = 0.75, list = F)
  training1 <- diabetes[inTraininglocal, ]
  testing <-diabetes[-inTraininglocal, ]
  fittree <- C5.0(training1$ Class.variable ~ ., data = training1[, -9])
  pred <- predict.C5.0(fittree,testing[ , -9])
  a <- table(testing$ Class.variable, pred)
  acc <- c(acc,sum(diag(a))/sum(a))
}
acc
mean(acc)

#**************************************************************
############## Boosting

# Accuracy with single model with Boosting

inTraininglocal <- createDataPartition(diabetes$ Class.variable, p = 0.75, list = F)
training <- diabetes[inTraininglocal, ]
testing <- diabetes[-inTraininglocal, ]

model <- C5.0(training$Class.variable ~ ., data = training[, -9], trials = 10)
pred <- predict.C5.0(model, testing[, -9])
a <- table(testing$Class.variable, pred)

sum(diag(a))/sum(a)

#***************************************************************
######### Bagging and Boosting
acc <- c()
for(i in 1:11)
{
  
  inTraininglocal <- createDataPartition(diabetes$ Class.variable, p = 0.75, list = F)
  training1 <- diabetes[inTraininglocal, ]
  testing <- diabetes[-inTraininglocal, ]
  
  fittree <- C5.0(training1$ Class.variable ~ ., data = training1, trials = 10)
  pred <- predict.C5.0(fittree, testing[, -9])
  a <- table(testing$ Class.variable, pred)
  
  acc <- c(acc, sum(diag(a))/sum(a))
  
}

acc
mean(acc)
