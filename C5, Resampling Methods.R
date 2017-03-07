### Mar 7, 2017
library(ISLR)
library(boot) # for cross-validataion and boostrap

# loading data
data("Auto")
attach(Auto)

# sampling
set.seed(1)
train <- sample(392, 196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)

# prediction error
mean((mpg - predict(lm.fit, Auto))[-train]^2)

# introducing polinomail terms
lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# Leave one out
glm.fit <- glm(mpg~horsepower, data=Auto)
coef(glm.fit)

cv.err = cv.glm(Auto, glm.fit)
cv.err$delta # error, similar as mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# bootstrap

# loading data
data("Portfolio")

alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return ((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y)) )
}

# 1,
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

# 2, using boot()
boot(Portfolio, alpha.fn, R = 1000)

# use case, estimating the accuracy of a linear regression model
boot.fn <- function(data, index)
  return (coef(lm(mpg~horsepower), data=data, subset=index))

boot.fn(Auto, 1:392)

boot(Auto, boot.fn, 1000)

# compare with:
summary(lm(mpg~horsepower, data=Auto))$coef









