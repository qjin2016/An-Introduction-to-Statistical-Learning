### Mar 11, 2017

# clear environment
rm(list=ls())

### PCA

# loading data
data("USArrests")

pr.out <- prcomp(USArrests, scale=TRUE)

# list of outputs:
names(pr.out) # rotation -- loadings; # center, scale correspond to means and sdv prior to PCA

# biplot
biplot(pr.out, scale=0)
# get a mirror of the above biplot
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x 
biplot(pr.out, scale = 0)

# standard deviation of each PC
pr.out$sdev
# variance of each PC
pr.var <- pr.out$sdev ^ 2

# proportion of variance explained by each PC
pve <- pr.var/(sum(pr.var))

plot(pve, xlab='PC', ylab='% of variance explained', ylim=c(0,1), type='b')
# cumulative plot
plot(cumsum(pve), xlab='PC', ylab='% of variance explained', ylim=c(0,1), type='b')



### Clustering
### 1, K-Means
set.seed(2)
# prepare data
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

km.out <- kmeans(x, 2, nstart=20)
km.out$tot.withinss # total within-cluster sum of squares
# nstart specifies the num of random assignments, only the best result will be reported 
#   (lowest total within-cluster variance)

plot(x, col=(km.out$cluster + 1), main='KM Clustering with K=2', xlab='', ylab='', pch=20, cex=2)

### 2, Hierarchical Clustering
# method can be:
# 1, complete: the maximum intercluster dissimilarity. Compute all the pairwise dissimilarities,
#               only use the largest dissimilarity to represent dissimilarity
# 2, single: minimum
# 3, average
# 4, centroid: assign a centroid to each cluster
hc.complete <- hclust(dist(x), method = 'complete')
hc.average <- hclust(dist(x), method = 'average')
hc.single <- hclust(dist(x), method = 'single')

# visualize
par(mfrow = c(1, 3))
plot(hc.complete, main = '', cex=.9)
plot(hc.average, main = '', cex=.9)
plot(hc.single, main = '', cex=.9)

# to determine the cluster labels for each observation associated with a given cut of the dendrogram
cutree(hc.complete, 2)

# correlation-based dist measurement
par(mfrow = c(1, 1))
x <- matrix(rnorm(30*3), ncol=3)
dd <- as.dist(1-cor(t(x))) # looks at correlation between each observation
plot(hclust(dd, method = 'complete'), main='Complete Linkage with Correlation-Based Distance',
                    xlab='', ylab='', sub='')




















