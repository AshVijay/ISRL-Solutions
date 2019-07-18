# Code for the project presentation

# Importing the libraries that are required
library(caret)
library(ggplot2)
library(grid)
library(gridExtra)
library(corrplot)
library(psych)
library(plyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(survival)
library(pROC)
library(DMwR)
library(scales)

# Getting the dataframe from the csv file
HRdata <- read.csv("D:/Downloads/SUNY Buffalo Semester 1/Statistical Data Mining 1/Final Project/HRData.csv")
names(HRdata)

# Split data to train and test
set.seed(12345)
inTrain <- createDataPartition(HRdata$Attrition,p=0.75,list = FALSE)
Training <- HRdata[inTrain,]
Testing <- HRdata[-inTrain,]

# Calculating the percentage of attrition
ggplot(Training,aes(Attrition,fill=Attrition))+geom_bar()
prop.table(table(Training$Attrition)) #Percentage of Attrition

# Trying out a plot
jobLevel <- ggplot(Training,aes(JobLevel,fill=Attrition))+geom_bar()
newManager <- ggplot(Training,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()
jobSatisfaction <- ggplot(Training,aes(JobSatisfaction,fill=Attrition))+geom_bar()
distFromWork <- ggplot(Training,aes(DistanceFromHome,fill=Attrition))+geom_bar()
# grid.arrange(jobLevel,newManager,jobSatisfaction,distFromWork,ncol=2,top = "Some of the factors that influence attrition")
grid.arrange(jobLevel,newManager,jobSatisfaction,distFromWork,top = "Some of the factors that influence attrition")

# Feature engineering
# Training_os <- Training
# Training_os$TenurePerJob <- ifelse(Training_os$NumCompaniesWorked!=0, Training_os$TotalWorkingYears/Training_os$NumCompaniesWorked,0)
# Training_os$YearsWithoutChange <- Training_os$YearsInCurrentRole - Training_os$YearsSinceLastPromotion
Training$TenurePerJob <- ifelse(Training$NumCompaniesWorked!=0, Training$TotalWorkingYears/Training$NumCompaniesWorked,0)
Training$YearsWithoutChange <- Training$YearsInCurrentRole - Training$YearsSinceLastPromotion

# Adding these features in test data
Testing$TenurePerJob <- ifelse(Testing$NumCompaniesWorked!=0, Testing$TotalWorkingYears/Testing$NumCompaniesWorked,0)
Testing$YearsWithoutChange <- Testing$YearsInCurrentRole - Testing$YearsSinceLastPromotion

# Converting non-numeric variables to numberic
Train <- Training
Train$BusinessTravel <- as.integer(Train$BusinessTravel)
Train$Department <- as.integer(Train$Department)
Train$Gender <- as.integer(Train$Gender)
Train$MaritalStatus <- as.integer(Train$MaritalStatus)
Train$OverTime <- as.integer(Train$OverTime)
Train$JobRole <- as.integer(Train$JobRole)
Train$EducationField <- as.integer(Train$EducationField)
Train$Over18 <- as.integer(Train$Over18)
# Train$Attrition <- as.integer(Train$Attrition)

Test <- Testing
Test$BusinessTravel <- as.integer(Test$BusinessTravel)
Test$Department <- as.integer(Test$Department)
Test$Gender <- as.integer(Test$Gender)
Test$MaritalStatus <- as.integer(Test$MaritalStatus)
Test$OverTime <- as.integer(Test$OverTime)
Test$JobRole <- as.integer(Test$JobRole)
Test$EducationField <- as.integer(Test$EducationField)
Test$Over18 <- as.integer(Test$Over18)
# Test$Attrition <- as.integer(Test$Attrition)

Train$EmployeeCount <- NULL
Train$EmployeeNumber <- NULL
Train$StandardHours <- NULL
Train$Over18 <- NULL

Test$EmployeeCount <- NULL
Test$EmployeeNumber <- NULL
Test$StandardHours <- NULL
Test$Over18 <- NULL

# Fitting different models
set.seed(12345)
fit_rpart <- train(Attrition ~.,Train,method = 'rpart', trControl = trainControl(method = 'cv',number = 3))

set.seed(12345)
fit_rf <- train(Attrition ~.,Train,method = 'rf', trControl = trainControl(method = 'repeatedcv',number = 3))

set.seed(12345)
xgbGrid <- expand.grid(nrounds = 300, max_depth = 1, eta = 0.3, gamma = 0.01, colsample_bytree = .7, min_child_weight = 1, subsample = 0.9)

set.seed(12345)
fit_xgb <- train(Attrition ~.,Train,method = 'xgbTree',tuneGrid = xgbGrid,trControl = trainControl(method = 'repeatedcv',number = 3,classProbs = TRUE)) 

set.seed(12345)
fit_tbag <- train(Attrition ~.,Train,method = 'treebag', trControl = trainControl(method = 'repeatedcv',number = 3))

# set.seed(12345)
# fit_nn <- train(Attrition ~.,Train,method = 'pcaNNet',trControl = trainControl(method = 'repeatedcv',number = 3),tuneGrid = expand.grid(size = 25,decay = 0.01))

set.seed(12345)
fit_glm <- train(Attrition~.,Train,method = 'glm',trControl = trainControl(method = 'repeatedcv',number = 3))

set.seed(12345)
fit_svm <- train(Attrition~.,Train,method = 'svmRadial',trControl = trainControl(method = 'repeatedcv',number = 3))

set.seed(12345)
fit_knn <- train(Attrition~.,Train,method = 'knn',trControl = trainControl(method = 'repeatedcv',number = 3))

# set.seed(12345)
# fit_glmBoost <- train(Attrition~.,Train,method = 'glmboost',trControl = trainControl(method = 'repeatedcv',number = 3))

set.seed(12345)
Predictions_rpart <- predict(fit_rpart,Test)
Predictions_rf <- predict(fit_rf, Test)
Predictions_xgb <- predict(fit_xgb, Test)
# Predictions_nn <- predict(fit_nn, Test)
Predictions_glm <- predict(fit_glm, Test)
Predictions_svm <- predict(fit_svm,Test)
Predictions_tbag <- predict(fit_tbag,Test)
Predictions_knn <- predict(fit_knn,Test)
# Predictions_glmboost <- predict(fit_glmBoost,Test)

roc_rpart <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_rpart))
roc_rpart$auc

roc_rf <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_rf))
roc_rf$auc

roc_xgb <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_xgb))
roc_xgb$auc

# roc_nn <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_nn))
# roc_nn$auc

roc_glm <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_glm))
roc_glm$auc

roc_svm <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_svm))
roc_svm$auc

roc_knn <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_knn))
roc_knn$auc

# roc_glmboost <- roc(as.numeric(Test$Attrition), as.numeric(Predictions_glmboost))
# roc_glmboost$auc

plot(roc_rpart, ylim = c(0,1), print.thres = T, main = "ROC curves", col = "salmon")
plot(roc_rf, ylim = c(0,1), print.thres = T, col = "darkolivegreen", add = T)
plot(roc_xgb, ylim = c(0,1), print.thres = T, col = "steelblue", add = T)
plot(roc_glm, ylim = c(0,1), print.thres = T, col = "burlywood", add = T)
plot(roc_svm, ylim = c(0,1), print.thres = T, col = "burlywood", add = T)
plot(roc_knn, ylim = c(0,1), print.thres = T, col = "burlywood", add = T)

confusionMatrix(Predictions_glm,Testing$Attrition)
confusionMatrix(Predictions_knn,Testing$Attrition)
confusionMatrix(Predictions_rpart,Testing$Attrition)
confusionMatrix(Predictions_rf,Testing$Attrition)
confusionMatrix(Predictions_svm,Testing$Attrition)
confusionMatrix(Predictions_xgb,Testing$Attrition)
confusionMatrix(Predictions_tbag,Testing$Attrition)


df <- data.frame(method=c("GLM", "kNN", "RPart", "RF", "SVM", "xGB"),
                 accuracy=c(85.56, 82.83, 83.11, 82.02, 83.92, 86.38))
head(df)

ggplot(df, aes(method, accuracy)) + geom_bar(stat = "identity") + scale_y_continuous(limits=c(80,90),oob = rescale_none)


varImp(fit_xgb)

probs <- predict(fit_xgb, Test, type = "prob")

Test$Prediction <- probs$Yes

Test2 <- Test

levels(Test2$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")

Test2$JobRole <- factor(Test2$JobRole, labels = c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep"))
#ggplot(Test2, aes(x = Test2$JobRole, y = Test2$Prediction)) + geom_boxplot()
ggplot(Test2, aes(x = Test2$JobRole, y = Test2$Prediction, fill=Test2$JobRole)) + geom_boxplot(alpha=0.1) + theme(legend.position = "none") + xlab("Job Role") + ylab("Probability of Attrition")


Predictions_xgb <- predict(fit_xgb, Test)

set.seed(1234)
in2 <- sample(1:1450, 1)
set.seed(123)
in3 <- sample(1:1450, 1)

set.seed(12345)
in1 <- sample(1:1450, 1)
Test_ran <- Testing[in1,]
Predictions_xgb_in1 <- predict(fit_xgb, Test_ran)