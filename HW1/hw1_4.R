###########################################################################
## Statistical Data Mining 1 - Homework 1 - Question 4
## Name: Arun Krishnamurthy
## Class Number: 34
## Created: September 22, 2017
## Edited:
###########################################################################


#####################################################
#########   Crime rate
#####################################################

# scatterplot matrix
pairs(Boston)

# correlation with crime rate
cor(Boston$crim, Boston)

#linear model with crime rate
fit <- lm(crim ~ ., data = Boston)
summary(fit)


#####################################################
#########   Suburbs crime rate, tax, ptratio
#####################################################

# sumamry of crime rates
summary(Boston$crim)

# histogram
hist(Boston$crim)

# crime rate as a percentage of neighborhoods
crime_selection <- subset(Boston, Boston$crim > 5)
nrow(crime_selection) / nrow(Boston)

# summary of tax rates
summary(Boston$tax)

# histogram for tax
hist(Boston$tax)

# tax as a percent of neighborhood
tax_selection <- subset(Boston, Boston$tax > 330)
nrow(tax_selection) / nrow(Boston)

# summary of ptratio
summary(Boston$ptratio)

prratio_selection <- subset(Boston, Boston$ptratio > 21)
nrow(prratio_selection) / nrow(Boston)

