#Question 4
#LOOCV 
######################################################################


set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

plot(x,y)

library(boot)
set.seed(12345)
Data <- data.frame(x, y)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]

fit.glm.2 <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.2)$delta[1]

fit.glm.3 <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.3)$delta[1]

fit.glm.4 <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.4)$delta[1]

summary(fit.glm.1)
summary(fit.glm.2)
summary(fit.glm.3)
summary(fit.glm.4)