###########################################################################
## Statistical Data Mining 1 - Homework 1 - Question 3
## Name: Arun Krishnamurthy
## Class Number: 34
## Created: September 22, 2017
## Edited:
###########################################################################

rm(list = ls())

setwd("~/Dropbox/STA_545_Fall2017/Comp_Labs")

dim(train)

head(train)

# compare
train <- as.data.frame(zip.train)
test <- as.data.frame(zip.test)


#knn
digits.knn = knn(zip.train[,-1],zip.test[,-1],cl=zip.train[,1],k=3)

digits.knn = knn(zip.train[,-1],zip.test[,-1],cl=zip.train[,1],k=5)