# Read Diabetes Dataset
data <- read.csv("/home/kilio/R/Skyserver_SQL2_27_2018 6_51_39 PM.csv")[,-c(9:13,15:18)]
## from "https://www.kaggle.com/lucidlenn/sloan-digital-sky-survey"

set.seed(999)
select <- sample(1:nrow(data),nrow(data)*0.8)

### 1. Build Bagging Model
library(adabag)

set.seed(999)
Bagging_Model <- bagging(class ~ ., data=data[select,],  mfinal = 100)

# Make Predictions for Training Data
Bagging.Prediction <- predict(Bagging_Model, newdata=data[select,])

results1 <- data.frame(Bagging.Prediction$class, Bagging.Prediction$prob)
names(results1) <- c("Sky Survey", "GALAXY", "QSO", "STAR")
results1

barplot(Bagging_Model$importance) 

Bagging.Prediction$confusion

results1 <- table(Prediction=Bagging.Prediction$class, Actual=data[select,]$class)
results1

Correct_Rate1 <- sum(diag(results1)) / sum(results1) 
Correct_Rate1

# Make Predictions for Test Data
Bagging.Prediction <- predict(Bagging_Model, newdata=data[-select,])

Bagging.Prediction$confusion

results1 <- table(Prediction=Bagging.Prediction$class, Actual=data[-select,]$class)
results1

Correct_Rate1 <- sum(diag(results1)) / sum(results1) 
result_Bagging_Model <- Correct_Rate1
result_Bagging_Model

### 2. Build Boosting Model

set.seed(999)
Boosting_Model <- boosting(class ~ ., data=data[select,])

# Make Predictions for Training Data
Boosting.Prediction <- predict(Boosting_Model, newdata=data[select,])

results2 <- data.frame(Boosting.Prediction$class, Boosting.Prediction$prob)
names(results2) <- c("Sky Survey", "GALAXY", "QSO", "STAR")
results2

barplot(Boosting_Model$importance) 

Boosting_Model.Error <- errorevol(Boosting_Model, data[select,])
plot(Boosting_Model.Error$error,type="l",
     main="AdaBoost Error vs Number of Trees")

Boosting.Prediction$confusion

results2 <- table(Prediction=Boosting.Prediction$class, Actual=data[select,]$class)
results2

Correct_Rate2 <- sum(diag(results2)) / sum(results2) 
Correct_Rate2

# Make Predictions for Test Data
Boosting.Prediction <- predict(Boosting_Model, newdata=data[-select,])

Boosting.Prediction$confusion

results2 <- table(Prediction=Boosting.Prediction$class, Actual=data[-select,]$class)
results2

Correct_Rate2 <- sum(diag(results2)) / sum(results2) 
result_Boosting_Model <- Correct_Rate2
result_Boosting_Model

### 3. Build Random Forest Model
library(randomForest)

set.seed(999)
RF_Model <- randomForest(class ~ ., data=data[select,], ntree=300)

# Make Predictions for Training Data
RF.Prediction <- predict(RF_Model, data[select,])

accuracy.rf <- sum(RF.Prediction==data[select,]$class)/length(RF.Prediction)
accuracy.rf

table(RF.Prediction, data[select,]$class)

# Make Predictions for Test Data
RF.Prediction <- predict(RF_Model, data[-select,])

accuracy.rf <- sum(RF.Prediction==data[-select,]$class)/length(RF.Prediction)
result_Random_Forest_Model <- accuracy.rf
result_Random_Forest_Model

table(RF.Prediction, data[-select,]$class)

