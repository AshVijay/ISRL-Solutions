###########################################################################
## Statistical Data Mining 1 - Homework 2 - Question 1
## Name: Arun Krishnamurthy
## Class Number: 34
## Created: October 15, 2017
## Edited:
###########################################################################

####################################################
# 1. In this exercise, we will predict the number of  
# applications received using the other variables
# in the College data set in the ISLR package.
####################################################

library(ISLR)
data(College)
set.seed(12345)

#############################################################
# (a) Split the data set into a training set and a test set.
# Fit a linear model using least squares on the training set,
# and report the test error obtained.
#############################################################

# Splitting the data into train and test sets
indices = sample(1:dim(College)[1], dim(College)[1] / 2)
train <- College[indices, ]
test <- College[-indices, ]

# Fitting a linear model and caculating mean squared error
fit.lm <- lm(Apps ~ ., data = train)
prediction.lm <- predict(fit.lm, test)
mean((prediction.lm - test$Apps)^2)


#######################################################
# (b) Fit a ridge regression model on the training set,
# with lamda chosen by cross-validation.
# Report the test error obtained.
#######################################################

# Creating the x matrix and y vector to pass to the glmnet function
x = model.matrix(train$Apps ~ ., data = train)
y = train$Apps

# For lambda, use a grid of values ranging from 10^10 to 10^-2
grid <- 10 ^ seq(10, -2, length = 100)

# Perform ridge regression for the given range of lambda values
fit.ridge <- glmnet(x, y, alpha = 0, lambda = grid)

# To find best lambda value, perform ten-fold cross-validation
cv.ridge <- cv.glmnet(x, y, alpha = 0)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

# Find mean-squared error associated with the bestlam
x_test <- model.matrix(test$Apps ~ ., data = test)
prediction.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = x_test)
mean((prediction.ridge - test$Apps)^2)


#######################################################################
# (d) Fit a lasso model on the training set,
# with lambda chosen by cross-validation.Report the test error obtained,
# along with the number of non-zero coefficient estimates.
#######################################################################

# Perform lasso regression for the given range of lambda value
fit.lasso <- glmnet(x, y, alpha = 1, lambda = grid)

# To find best lambda value, perform ten-fold cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

# Find mean-squared error associated with the bestlam
prediction.lasso = predict(fit.lasso, s = bestlam.lasso , newx=x_test)
mean((prediction.lasso - test$Apps)^2)

# Find the number of non-zero co-efficients
coefficients.lasso = predict(fit.lasso, s = bestlam.lasso, type = "coefficients")
coefficients.lasso


#########################################################################################
# (e) Fit a PCR model on the training set, with k chosen by cross-validation.  
# Report the test error obtained, along with the value of k selected by cross-validation.
#########################################################################################

# Install the pls package and import the library to the workspace
#install.packages("pls")
library(pls)

# Perform PCR on training data
fit.pcr <- pcr(train$Apps ~ ., data = train, scale = TRUE, validation = "CV")

# Plot the cross-validation scores
validationplot(fit.pcr, val.type = "MSEP")

# Evaluate the test set performance
# Lowest cross-validation error occurs when M = 17
pcrerror.list = c()
for(i in 1:17) {
  prediction.pcr <- predict(fit.pcr, test, ncomp = i)
  current_error = mean((prediction.pcr - test$Apps)^2)
  pcrerror.list = c(pcrerror.list, current_error)
}
pcrerror.list


#########################################################################################
# (f) Fit a PLS model on the training set, with k chosen by crossvalidation.
# Report the test error obtained, along with the value of k selected by cross-validation.
#########################################################################################

# Perform PLS on training data
fit.pls <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")

# Plot the cross-validation scores
validationplot(fit.pls, val.type = "MSEP")

# Evaluate the test set performance
# Lowest cross-validation error occurs when M = 17
plserror.list = c()
for(i in 1:17) {
  prediction.pls <- predict(fit.pls, test, ncomp = i)
  current_error = mean((prediction.pls - test$Apps)^2)
  plserror.list = c(plserror.list, current_error)
}
plserror.list


##################################################################################
# (g) Comment on the results obtained. How accurately can we predict the number of 
# college applications received? Is there much difference amongthe test errors
# resulting from these five approaches?
##################################################################################

# Calculating the R^2 values for the test data for all the above models
test.average <- mean(test$Apps)
lm.r2 <- 1 - mean((prediction.lm - test$Apps)^2) / mean((test.average - test$Apps)^2)
lm.r2
ridge.r2 <- 1 - mean((prediction.ridge - test$Apps)^2) / mean((test.average - test$Apps)^2)
ridge.r2
lasso.r2 <- 1 - mean((prediction.lasso - test$Apps)^2) / mean((test.average - test$Apps)^2)
lasso.r2
pcr.r2 <- 1 - mean((prediction.pcr - test$Apps)^2) / mean((test.average - test$Apps)^2)
pcr.r2
pls.r2 <- 1 - mean((prediction.pls - test$Apps)^2) / mean((test.average - test$Apps)^2)
pls.r2