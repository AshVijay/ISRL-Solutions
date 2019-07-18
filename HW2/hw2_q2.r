###########################################################################
## Statistical Data Mining 1 - Homework 2 - Question 2
## Name: Arun Krishnamurthy
## Class Number: 34
## Created: October 15, 2017
## Edited:
###########################################################################

###################################################################################################################
# 2) The insurance company benchmark data set gives information on customers.
# Specifically,it contains 86 variables on product-usage data and 
# socio-demographic data derived from zip area codes.  There are 5,822 customers
# in the training  set  and  another  4,000 in  the  test  set.
# The data were collected to answer the  following  questions: 
# Can you predict who will be interested in buying a caravan insurance policy and give an explanation why?
# Compute the OLS estimates and compare them with those obtained from the following variable-selection algorithms:
# Forwards Selection, Backwards Selection, Lasso regression, and Ridge regression.
# Support your answer.
# (The data can be downloaded from: https://kdd.ics.uci.edu/databases/tic/tic.html)
###################################################################################################################

# Import the necessary libraries
library(ISLR)
library(glmnet)
library(pls)
library(leaps)

# Create data frames
read_train_data <- read.delim("ticdata2000.txt" , sep = "\t" , header=FALSE)
read_test_data <- read.delim("ticeval2000.txt" , sep = "\t" , header=FALSE )
read_target_data <- read.delim("tictgts2000.txt" , sep = "\t" , header=FALSE)

# From the data frames, form the train and test datasets
train <- data.frame(X=read_train_data[,1:85] , Y=read_train_data[,86])
test <- data.frame(X=read_test_data[,1:85] , Y=read_target_data[,1])

# Set the seed
set.seed(12345)

# Compute the OLS estimates
fit.OLS <- lm(train$Y ~ . , data = train)
OLS <- mean((test$Y - predict.lm(fit.OLS, test))^2)
OLS

# Putting the train and test data set into a matrix
train.mat <- model.matrix(train$Y ~ ., data = train, nvmax = 85)
test.mat <- model.matrix(test$Y ~ ., data = test, nvmax = 85)


############################
# Forward stepwise selection
############################

regfit.forward <- regsubsets(train$Y~., data = train, nvmax = 85, method = "forward")
summary(regfit.forward)

# Plot the training set MSE associated with the best model of each size.
val.errors <- c()
for (i in 1:85) {
    coefi <- coef(regfit.forward, id = i)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors = c(val.errors, mean((pred - test$Y)^2))
}
par(mfrow = c(1,1))
plot(val.errors, xlab = "Number of predictors", ylab = "Test MSE", pch = 19, type = "b")

# Model size for which the train set MSE takes on the minimum value and the error associated with it.
which.min(val.errors)
min(val.errors)


##############################
# Backward stepwise selection
##############################

regfit.backward <- regsubsets(train$Y~., data = train, nvmax = 85, method = "backward")
summary(regfit.backward)

# Plot the training set MSE associated with the best model of each size.
val.errors <- c()
for (i in 1:85) {
    coefi <- coef(regfit.backward, id = i)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors = c(val.errors, mean((pred - test$Y)^2))
}
par(mfrow = c(1,1))
plot(val.errors, xlab = "Number of predictors", ylab = "Test MSE", pch = 19, type = "b")

# Model size for which the train set MSE takes on the minimum value and the error associated with it.
which.min(val.errors)
min(val.errors)


##################
# Ridge Regression
##################

# Set a seed value to get reproducible results
set.seed(12345)

# Creating the x matrix and y vector to pass to the glmnet function
x = model.matrix(train$Y ~ ., data = train)
y = train$Y

# For lambda, use a grid of values ranging from 10^10 to 10^-2
grid <- 10 ^ seq(10, -2, length = 100)

# Perform ridge regression for the given range of lambda values
fit.ridge <- glmnet(x, y, alpha = 0, lambda = grid)

# To find best lambda value, perform ten-fold cross-validation
cv.ridge <- cv.glmnet(x, y, alpha = 0)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

# Find mean-squared error associated with the bestlam
x_test <- model.matrix(test$Y ~ ., data = test)
prediction.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = x_test)
mean((prediction.ridge - test$Y)^2)


##############
# Lasso Method
##############

# Perform lasso regression for the given range of lambda value
fit.lasso <- glmnet(x, y, alpha = 1, lambda = grid)

# To find best lambda value, perform ten-fold cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

# Find mean-squared error associated with the bestlam
prediction.lasso = predict(fit.lasso, s = bestlam.lasso , newx=x_test)
mean((prediction.lasso - test$Y)^2)