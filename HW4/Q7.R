# Import the requisite libraries
library(ISLR)
library(e1071)


# Set seed value to get reproducible results
set.seed(12345)

# Sample the data and seperate them into train and test sets
train = sample(dim(OJ)[1], 750) ## 70% of 1070

OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]


#Tuning cost parameter between 0.01 and 10
set.seed(12345)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

# Compute training and test errors for this fit
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)

#plot(svm.linear, OJ.test, svSymbol = 1, dataSymbol = 2, symbolPalette = rainbow(2), color.palette = terrain.colors)


################################################################################
# Radial and Polynomial kernel (k=2) 
################################################################################

set.seed(12345)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
set.seed(12345)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 
    1, by = 0.25)))
summary(tune.out)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)

##Polynomial
set.seed(12345)
svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)


set.seed(12345)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, 
    ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
