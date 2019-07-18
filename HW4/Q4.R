library(ElemStatLearn)
library(randomForest)

spam$spam <- as.integer(spam$spam)
set.seed(12345)
smp_size <- floor(0.75 * nrow(spam))
set.seed(12345)
train_ind <- sample(seq_len(nrow(spam)), size = smp_size)
train <- spam[train_ind, ]
test <- spam[-train_ind, ]

spam.test= spam[-train_ind,"spam"]

bag.spam = randomForest(spam~., data = train, mtry = 57, importance = TRUE)
bag.spam

yhat.bag = predict(bag.spam, newdata = test)
plot(yhat.bag, test$spam)
abline(0, 1)
mean((yhat.bag - spam.test)^2)

bag.spam = randomForest(spam~., data = train, mtry = 57, ntree = 25)
yhat.bag = predict(bag.spam, newdata = test)
t1=mean((yhat.bag - spam.test)^2)

set.seed(12345)
rf.spam = randomForest(spam~., data = train, mtry = 40, importance = TRUE)
yhat.rf = predict(rf.spam, newdata = test)
t2=mean((yhat.rf - spam.test)^2)
set.seed(12345)

rf.spam = randomForest(spam~., data = train, mtry = 20, importance = TRUE)
yhat.rf = predict(rf.spam, newdata = test)
t3=mean((yhat.rf - spam.test)^2)

set.seed(12345)
rf.spam = randomForest(spam~., data = train, mtry = 10, importance = TRUE)
yhat.rf = predict(rf.spam, newdata = test)
t5=mean((yhat.rf - spam.test)^2)

set.seed(12345)
rf.spam = randomForest(spam~., data = train, mtry = 5 importance = TRUE)
yhat.rf = predict(rf.spam, newdata = test)
t6=mean((yhat.rf - spam.test)^2)
set.seed(12345)

x = c(57,40,20,10,5)
y = c(t1,t2,t3,t5,t6)
plot(x,y)
