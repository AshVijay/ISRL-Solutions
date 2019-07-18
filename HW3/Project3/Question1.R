#Question 1 
#To predict  whether  a  given  suburb 
# has a  crime  rate  above  or  below  the median.  
# Explore logistic regression, LDA and kNN models using 
# various subsets of the predictors.  
##############################################################

library(MASS)
data(Boston)
set.seed(123)


summary(Boston)

# To numerize the class to 0 and 1
crim_med <- rep(0, length(Boston$crim))
crim_med[Boston$crim > median(Boston$crim)] <- 1
Boston <- data.frame(Boston, crim_med)
Boston$crim_med

# Create a set of box plots to check how each of the variable relates to crim_med
# ignore the chas variable and the crim variables
par(mfrow=c(2,6))
for(i in names(Boston)){
  # excluding the own crime variables and the chas variable
  if( grepl(i, pattern="^crim|^chas")){ next}
  boxplot(eval(parse(text=i)) ~ crim_med, ylab=i, col=c("red", "blue"), varwidth=T)
}

#Train and Test sets
set.seed(12345)
variables = c("zn", "indus", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv", "crim_med")
rows = sample(x=nrow(Boston), size=.75*nrow(Boston))
train = Boston[rows, variables]
test = Boston[-rows, variables]

# Logistic Regression
lr.fit <- glm(as.factor(crim_med) ~ ., data=train, family="binomial")
lr.probs <- predict(lr.fit, test, type="response")
lr.pred <- ifelse(lr.probs>.5, "1","0")
lr.test.error <- mean(lr.pred!=test$crim_med)
lr.test.error


# LDA
lda.fit <- lda(train$crim_med ~ ., data = train)
lda.pred <- predict(lda.fit, test)
lda.test.error <- mean(lda.pred$class != test$crim_med)
lda.test.error

# KNN
set.seed(12345)
train.knn = train[, c("zn", "indus", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv")]
test.knn = test[, c("zn", "indus", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv")]
# kNN takes the training response variable seperately
train.knn.response = train$crim_med
test.knn.response = test$crim_med

library(class)
set.seed(123)
knn.pred = knn(train.knn, test.knn, train.knn.response, k = 1)
mean(knn.pred != test$crim_med)

knn.pred = NULL
knn.error = NULL
for(i in 1:dim(test.knn)[1]) {
    set.seed(12345)
    knn.pred = knn(train.knn, test.knn, train.knn.response, k = i)
    knn.error[i] = mean(test$crim_med != knn.pred)
}

### find the minimum error rate
min.knn.error = min(knn.error)
print(min.knn.error)

### get the index of that error rate, which is the k
k = which(knn.error == min.knn.error)
print(k)
par(mfrow=c(1,1))
plot(knn.error, xlab = "k value", ylab = "error rate", pch = 11, type = "b")