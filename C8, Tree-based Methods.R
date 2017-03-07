# Feb 26, 2017

# clear environment
rm(list = ls())

# load library
library(tree)

# ------ classification tree ------
# prepare data
library(ISLR)
data(Carseats)
attach(Carseats)
High <- ifelse(Sales<=8, 'NO', 'YES')

Carseats <- data.frame(Carseats, High)

# modeling
tree.carseats <- tree(High~. -Sales, Carseats)
# results
summary(tree.carseats)
tree.carseats
# visualize
plot(tree.carseats)
text(tree.carseats)

# split the whole dataset into training and testing, training and evaluating
set.seed(2)
train <- sample(1:nrow(Carseats), 200) # 400 in total
Carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats <- tree(High~.-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)

# tree pruning
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats # the tree with 9 termonal nodes yield the lowest cross-validation errors(50)
# plot error rate
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b') # k is the cost-complexity parameter
par(mfrow=c(1,1))
# apply pruning parameter learnt from the above
prune.carseats <- prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)
# prediction
tree.pred <- predict(prune.carseats, Carseats.test, type='class')
table(tree.pred, High.test)

# ------ regression tree ------
library(MASS)
set.seed(1)
# load and prepare data
data(Boston)
train <- sample(1:nrow(Boston), nrow(Boston)/2)

# modeling
tree.boston <- tree(medv~., Boston, subset = train)
summary(tree.boston)

# visualize tree
plot(tree.boston)
text(tree.boston, pretty = 0)

# tree pruning
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')
prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# evaluation performance
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, 'medv']
plot(yhat, boston.test)
abline(0,1)
# MSE
mean((yhat - boston.test)^2)

# ------ Bagging and Random Forests

# load library
library(randomForest)

# modeling -- bagging
set.seed(1)
bag.boston <- randomForest(medv~., data=Boston, subset = train, 
                           mtry=13, important=TRUE) 
# mtry indicates the num of features taken into consideration, if it is equal to the num of features
# that the training dataest has, then it's a bagging model

bag.boston

# evaluation performance
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
mean((yhat.bag - boston.test)^2)

# modeling -- random forests
set.seed(1)
rf.boston <- randomForest(medv~., data = Boston, subset = train,
                          mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)

# importance function, used for listing importance of features
importance(rf.boston)
varImpPlot(rf.boston)
# two measurements are used to evaluate the importance of features
# 1, IncMSE, based upon the mean decrease of accuracy in predictions on the out-of-bag samples
#    when a given variable is excluded from the model
# 2, the total decrease in node impurity that results from splits over that variable, averaged over
#    all trees


# ------ Boosting ------
# load library
library(gbm)

# modeling
set.seed(1)
boost.boston <- gbm(medv~., data=Boston[train, ], distribution='gaussian', 
                    n.trees=5000, interaction.depth=4)
# interaction.depth parameter limits the depth of each tree
# shrinkage parameter, adjusts learning speed

# 
summary(boost.boston)

# partial dependence plot, which illustrates the marginal effect of the selected variables on the
# response after integrating out the other variables
par(mfrow=c(1, 2))
plot(boost.boston, i='rm')
plot(boost.boston, i='lstat')
par(mfrow=c(1, 1))

# evaluation of performance
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)


# modeling
data(Carseats)
attach(Carseats)
High <- ifelse(Sales<=8, 0, 1)

Carseats <- data.frame(Carseats, as.factor(High))

gbm.carseats <- gbm(High~., data = Carseats, distribution='huberized',
                     n.trees=5000, interaction.depth=4, cv.folds = 10)
summary(gbm.carseats)
gbm.carseats.iter <- gbm.perf(gbm.carseats, method='cv')
print(gbm.carseats.iter)
pred <- round(predict(gbm.carseats, Carseats, gbm.carseats.iter, type='response'))
table(tru=High, pred=ifelse(pred<0, 0, 1))

predict(gbm.carseats, Carseats, gbm.carseats.iter)





































