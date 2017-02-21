# Feb 20, 2017

# clear environment
rm(list = ls())
# load library
library(e1071)

# prepare data
set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1, ] <- x[y==1, ] + 1
plot(x, col=3-y)

# fit into SVM
dat <- data.frame(x=x, y=as.factor(y))
svm.fit <- svm(y~., data = dat, kernel = 'linear', cost = 10, scale = FALSE)
# the cost argument allows people to specify the cost of violation to the margin,
# when the cost is small, the margins will be wide
# to prevent overfitting, cost has to be contained within a reasonable range

# results
plot(svm.fit, dat)

summary(svm.fit)

# parameter tuning
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = 'linear',
                 ranges=list(cost=c(.001, .01, .1, 1, 5, 10, 100)))
# result
summary(tune.out)

# choose best model
bestmod <- tune.out$best.model
summary(bestmod)

# prepare testing data
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

# test model on testing data
ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

# non-linear kernel
# 1, polynomial kernel
# 2, radial kernel

# prepare data
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100, ] <- x[1:100] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x=x, y=as.factor(y))

plot(x, col=y)

# radial kernel
train <- sample(200, 100)
svmfit <- svm(y~., data = dat[train, ], kernel='radial', gamma=1, cost=1)
plot(svmfit, dat[train, ])
summary(svmfit)

# parameter tuning
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train, ], kernel='radial',
                 ranges = list(cost=c(.1, 1, 10, 100, 1000),
                               gamma=c(.5, 1, 2, 3, 4)))

# best model
bestmod <- tune.out$best.model

# performance
table(true = dat[-train, 'y'], pred=predict(bestmod, dat[-train, ]))












