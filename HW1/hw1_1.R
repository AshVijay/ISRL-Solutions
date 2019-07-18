###########################################################################
## Statistical Data Mining 1 - Homework 1 - Question 1
## Name: Arun Krishnamurthy
## Class Number: 34
## Created: September 22, 2017
## Edited:
###########################################################################


# set the current working directory
setwd("C:/Users/Arun/Desktop/STA_545")

# load libarary 
library(DAAG)
library(lattice)
library(MASS)
library(ISLR)

# get information about the Auto data set
?Auto 
dim(Auto)
summary(Auto)

attach(Auto)

#####################################################
#########   Histogram and Density Plots
#####################################################

#plot histograms
x11()
par(mfrow = c(3,3))
hist(mpg, xlab = "miles per gallon", main = "histogram of mpg")
hist(cylinders, xlab = "cylinders", main = "histogram of cylinders")
hist(displacement, xlab = "displacement", main = "histogram of displacement")
hist(horsepower, xlab = "horsepower", main = "histogram of horsepower")
hist(weight, xlab = "weight", main = "histogram of weight")
hist(acceleration, xlab = "acceleration", main = "histogram of acceleration")
hist(year, xlab = "year", main = "histogram of year")
hist(origin, xlab = "origin", main = "histogram of origin")

#Checking head of Auto
head(Auto,20)


#####################################################
#########   Scatterplot
#####################################################
pairs(Auto)

xyplot(mpg ~ acceleration, data = Auto)
xyplot(mpg ~ displacement, data = Auto)
xyplot(mpg ~ horsepower, data = Auto)
xyplot(mpg ~ weight, data = Auto)
xyplot(mpg ~ year, data = Auto)


#####################################################
#########   Save the clean dataset
#####################################################
cleaned_Auto <- subset(Auto, select = c(1,3,4,5,6,7))
save(cleaned_Auto, file = "cleaned_Auto.RData")
summary(cleaned_Auto)