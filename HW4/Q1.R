library(ElemStatLearn)
library('leaps')
prost <- prostate

# Scale data and prepare train/test split
#prost.std <- data.frame(cbind(scale(prost[,1:8]),prost$lpsa))
#names(prost.std)[9] <- 'lpsa'
smp_size <- floor(0.75 * nrow(prost))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(prost)), size = smp_size)

data.train <- prost[train_ind, ]
data.test <- prost[-train_ind, ]
y.test <- data.test$lpsa
n.train <- nrow(data.train)


library(leaps)
l <- leaps(data.train[,1:8],data.train[,9],method='r2')
plot(l$size,l$r2)
l <- leaps(data.train[,1:8],data.train[,9],method='Cp')
plot(l$size,l$Cp)

# Select best model according to Cp
bestfeat <- l$which[which.min(l$Cp),]

# Train and test the model on the best subset
m.bestsubset <- lm(lpsa ~ .,data=data.train[,bestfeat])
summary(m.bestsubset)
y.pred.bestsubset <- predict(m.bestsubset,data.test[,bestfeat])
summary((y.pred.bestsubset - y.test)^2)

##Fitting a linear model
data.train <- prost[train_ind, ]
data.test <- prost[-train_ind, ]
newdata <- prostate[,c("lcavol","age","svi","lpsa")]
linear_model <- lm(lpsa ~ . , data = data.train)
prost <- newdata
##AIC value
AIC(linear_model)
##BIC value
BIC(linear_model)

##Mean Square error on Test Data
mean((data.test$lpsa - predict.lm(linear_model,data.test )) ^ 2)

##5 fold cross validation
library(DAAG)
cvResults <- suppressWarnings(CVlm(data=prost, form.lm=lpsa ~ ., m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error

##10 fold cross validation

cvResults <- suppressWarnings(CVlm(data=prost, form.lm=lpsa ~ ., m=10, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error
