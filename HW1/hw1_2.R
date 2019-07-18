###########################################################################
## Statistical Data Mining 1 - Homework 1 - Question 2
## Name: Arun Krishnamurthy
## Class Number: 34
## Created: September 22, 2017
## Edited:
###########################################################################


#####################################################
#########   Linear Model
#####################################################

fit <- lm(mpg ~ displacement + horsepower + weight + acceleration + year, data = Auto)
summary(fit)

fit <- lm(mpg ~ displacement:weight + displacement:year + displacement:cylinders, data = Auto)
summary(fit)