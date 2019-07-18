#Question 2
# To predict the class of a new data point by fitting LDA and QDA models

##################################################
#Note: The modified csv file DiabetesAndrews36_1.csv must be set as the input input file
#The file is attached with the other files within the compressed file.
#################################################
install.packages('klaR')
install.packages('ISLR')
install.packages('MASS')
setwd("C:/Users/Ashwin/Desktop/Data_Mining/Project3")
library(klaR)
library(ISLR)
library(MASS)
library(corrplot)
data_input = read.csv("DiabetesAndrews36_1.csv", header=FALSE)
mydata <- data_input
pairs(mydata[1:5], main = "Diabetes", pch = 21, bg = c("red", "green3", "blue")[unclass(mydata$V6)], lower.panel=NULL, labels=c("Gl","Ins","SS","Wgt","Pl"), font.labels=2, cex.labels=4.5) 

##Correlation
cor(mydata[,1:5])
##Train and test split
train = sample(1:nrow(mydata),nrow(mydata)*.80)
mydata_train = mydata[train,]
mydata_test = mydata[-train,]

##LDA
lda.fit <- lda( V6 ~ . , data=mydata_train )
lda.pred.train <- predict(lda.fit , newdata=mydata_train)
lda.pred.test <- predict(lda.fit , newdata=mydata_test)
y_true_train <- as.numeric(mydata_train$V6)
y_true_test <- as.numeric(mydata_test$V6)
y_hat_train <- as.numeric(lda.pred.train$class)
y_hat_test <- as.numeric(lda.pred.test$class)
LDA_train_error = sum(abs(y_true_train -y_hat_train))/length(y_true_train) 
LDA_test_error = sum(abs(y_true_test -y_hat_test))/length(y_true_test)

##QDA
qda.fit <- qda(V6 ~ . , data=mydata_train )
qda.pred.train <- predict(qda.fit , newdata=mydata_train)
qda.pred.test <- predict(qda.fit , newdata=mydata_test)
y_true_train <- as.numeric(mydata_train$V6)
y_true_test <- as.numeric(mydata_test$V6)
y_hat_train <- as.numeric(qda.pred.train$class)
y_hat_test <- as.numeric(qda.pred.test$class)
QDA_train_error = sum(abs(y_true_train -y_hat_train))/length(y_true_train) 
QDA_test_error = sum(abs(y_true_test -y_hat_test))/length(y_true_test)  

##Prediction using LDA
V1 <- c(0.98)
V2 <- c(122)
V3 <- c(544)
V4 <- c(186)
V5 <- c(184)
V6 <- c(0)
df <- data.frame(V1,V2,V3,V4,V5,V6)
lda.fit <- lda(V6 ~ . , data=mydata_train )
lda.pred.train <- predict(lda.fit , newdata=df)
lda.pred.train$class

##Prediction using QDA 
qda.fit <- qda(V6 ~ . , data=mydata_train )
qda.pred.train <- predict(qda.fit , newdata=df)
qda.pred.train$class
