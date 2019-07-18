###########################################################################
## Statistical Data Mining 1 - Homework 2 - Question 3
## Name: Arun Krishnamurthy
## Class Number: 34
## Created: October 15, 2017
## Edited:
###########################################################################

##################################################################################################################################################
# 3) (10 points) (Exercise 9 modified, ISL)
# We have seen that as the number of features used in a model increases, the training error will necessarily decrease, but the test error may not.
# We will now explore this in a simulated data set. Generate a data set with p  = 20 features, n  = 1, 000 observations,
# and an associated quantitative response vector generated according to the model y = x.beta + e
# has some elements that are exactly equal to zero.
# Split your data set into a training set containing 100 observations and a test set containing 900 observations.
# 
# Perform best subset selection on the training set, and plot the training set MSE associated with the best model of each size.
# Plot the test set MSE associated with the best model of each size.
# 
# For which model size does the test set MSE take on its minimum value? 
# Comment on your results. How does the model at which the test set MSE is minimized compare to the true model used to generate the data? 
# Commenton the coefficient values.
##################################################################################################################################################

########################################################################################################################
# Generate a data set with p  = 20 features, n  = 1, 000 observations,
# and an associated quantitative response vector generated according to the model y = x.beta + e
# beta has some elements that are exactly equal to zero.
########################################################################################################################

# Setting a seed value for random numbers
set.seed(12345)

# Need a matrix of 20 features and 1000 observations. In other words, 20 columns and 1000 rows i.e. 20x1000 = 20000 table cells
x <- matrix(rnorm(20000), ncol = 20)
dim(x)

# Generate 20 different random values for b
b <- rnorm(20)
b

# Set a seed value again to make this result reproducible
set.seed(12345)

# b should have some elements that are exactly equal to zero.
# Generate 5 random indices
zero_indices <- sample(1:20, 5)
zero_indices

# Set zero to these indices so we can have some elements that are exactly equal to zero
for(i in 1:20) {
    if(i %in% zero_indices) {
        b[i] = 0
    }
}
b

# Generate 1000 error values
e <- rnorm(1000)

# Putting all the generated values together to get the response y
y <- x %*% b + e


#################################################################################################################
# Split your data set into a training set containing 100 observations and a test set containing 900 observations.
#################################################################################################################

# Generate 100 random indices in order to parition x and y into training and test
indices = sample(1:dim(x)[1], dim(x)[1] / 10)

# Partition the x and y to train and test 
x.train <- x[indices, ]
x.test <- x[-indices, ]
y.train <- y[indices, ]
y.test <- y[-indices, ]

# Now create the final train and test datasets with both x and y
train = data.frame(y = y.train, x = x.train)
test = data.frame(y = y.test, x = x.test)

# install.packages("leaps")
library("leaps")

# Perform exhaustive subset selection on the training data
regfit.full <- regsubsets(y~., data = train, nvmax = 20, method = "exhaustive")
my_sum <- summary(regfit.full)

# This matrix displays best subset from 1 to 20 parameters
my_sum$outmat

# Plot the cp bic, rss, and adjr2 statistic for regfit.full
par(mfrow = c(2,2))
plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

# Find the optimum model for each of the four statistics
which(my_sum$cp == min(my_sum$cp))
which(my_sum$bic == min(my_sum$bic))
which(my_sum$rss == min(my_sum$rss))
which(my_sum$adjr2 == max(my_sum$adjr2))

# Putting the train and test data set into a matrix
train.mat <- model.matrix(y ~ ., data = train, nvmax = 20)
test.mat <- model.matrix(y ~ ., data = test, nvmax = 20)


###############################################################################################################################
# Perform best subset selection on the training set, and plot the training set MSE associated with the best model of each size.
###############################################################################################################################

val.errors <- c()
for (i in 1:20) {
    coefi <- coef(regfit.full, id = i)
    pred <- train.mat[, names(coefi)] %*% coefi
    val.errors = c(val.errors, mean((pred - y.train)^2))
}
par(mfrow = c(1,1))
plot(val.errors, xlab = "Number of predictors", ylab = "Training MSE", pch = 19, type = "b")

# Model size for which the train set MSE takes on the minimum value
which.min(val.errors)


####################################################################
# Plot the test set MSE associated with the best model of each size.
####################################################################

val.errors <- c()
for (i in 1:20) {
    coefi <- coef(regfit.full, id = i)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors = c(val.errors, mean((pred - y.test)^2))
}
par(mfrow = c(1,1))
plot(val.errors, xlab = "Number of predictors", ylab = "Test MSE", pch = 19, type = "b")


#######################################################################
# For which model size does the test set MSE take on its minimum value?
####################################################################### 

which.min(val.errors)


#########################################################################################
# Compare to the true model used to generate the data? Comment on the coefficient values.
#########################################################################################

coef(regfit.full, which.min(val.errors))