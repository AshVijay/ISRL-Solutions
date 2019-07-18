install.packages("neuralnet")
install.packages("tree")
library(neuralnet)
library("tree")
library("caret")
library(rpart)
library(MASS)

wine_data_set <- read.delim("C:\Users\Ashwin\Desktop\Data Mining HW 4\WineData.txt", sep = ",", header = TRUE)

set.seed(12345)
test_indis <- sample(1:nrow(wine_data_set), 3/4*nrow(wine_data_set))
test <- wine_data_set[-test_indis,]
training <- wine_data_set[test_indis,]




model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.wine.data <- rpart(training$Class~., data = training, method = "class", control = model.control)

plot(fit.wine.data$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(fit.wine.data$cptable[,4])
pruned_fit_wine <- prune(fit.wine.data, cp = fit.wine.data$cptable[min_cp,1])


## plot the full tree and the pruned tree
x11()
plot(pruned_fit_wine, branch = .3, uniform = T, compress=T, main = "Pruned Tree")
text(pruned_fit_wine, use.n = T , cex = .5)

x11()
plot(fit.wine.data, branch = .3, compress=T, main = "Full Tree")
text(fit.wine.data, use.n = T , cex = .5)

