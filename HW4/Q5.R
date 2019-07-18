## Load R package for the book
install.packages("ElemStatLearn")
library("ElemStatLearn")

## load the training data
data(spam)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(spam))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(spam)), size = smp_size)

train <- spam[train_ind, ]
test <- spam[-train_ind, ]
# 'data.frame':	528 obs. of  11 variables:
# $ y   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ x.1 : num  -3.64 -3.33 -2.12 -2.29 -2.6 ...
# $ x.2 : num  0.418 0.496 0.894 1.809 1.938 ...

## use neural network method
## need package "nnet"
library("nnet")
#y <- class.ind(spam$spam) # Generates class indicator from the given factor "xclass"
y <- as.integer(train$spam)
X <- train[,1:57]

##Converting spam colum to numeric(0-> not spam and 1-> spam)
y <- rep(0, length(train$spam))
y[train$spam == "spam"] <- 1

## fit neural network model based on Cross-Validation
size.nn <- 10          # number of units in the hidden layer
decay.nn <- 0.001      # decay.nn.opt[18] = 0.0009765625 
rang.nn <- 0.3         # Initial random weights on [-rang, rang]
maxit.nn <- 1000        # maximum number of iterations
set.seed(123)
fit.nn <- nnet(X, y, size=size.nn, rang=rang.nn, decay=decay.nn, maxit=maxit.nn)
## check training error rate
train.label.nn <- apply(predict(fit.nn, X), 1, which.max)
sum(train.label.nn!=y) # 39
sum(train.label.nn!=y)/length(y) # 39


## check testing error rate

Xt=test[,1:57]
tclass <- rep(0, length(test$spam))
tclass[test$spam == "spam"] <- 1
test.label.nn <- apply(predict(fit.nn, Xt), 1, which.max)
sum(test.label.nn!=tclass) # 275
sum(test.label.nn!=tclass)/length(tclass) # 0.5952381
