library(ggplot2)
library(data.table)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pca_output <- prcomp(USArrests,scale = TRUE)

pca_output$center
pca_output$scale

pca_output$rotation

head(pca_output$x)

biplot(pca_output, scale = 0)

# K-means
set.seed(2)
x <- data.table(a = rnorm(50), b = rnorm(50))
x[1:25, `:=` (a = a + 3, b = b - 4)]
# x[1:25, a := a + 3]
# x[1:25, b := b - 4]


km_output <- kmeans(x, centers = 2, nstart = 20)
km_output
km_output$cluster

ggplot(x, aes(x = a, y = b)) + geom_point(colour = (km_output$cluster + 1), size = 4)

set.seed(4)
km_output <- kmeans(x, centers = 3, nstart = 20)
ggplot(x, aes(x = a, y = b)) + geom_point(colour = (km_output$cluster + 1), size = 4)

set.seed(3)
km_output <- kmeans(x, centers = 3, nstart = 1)
km_output$tot.withinss # lucky hit, should be bigger than the others
km_output <- kmeans(x, centers = 3, nstart = 20)
km_output$tot.withinss  
km_output <- kmeans(x, centers = 3, nstart = 50)
km_output$tot.withinss

ks <- 1:5
tot_within_ss <- sapply(ks, function(k){
  km_output <- kmeans(x, centers = k, nstart = 20)
  km_output$tot.withinss
})
tot_within_ss

plot(
  x = ks,
  y = tot_within_ss,
  type = "b",
  xlab = "VAlues of K",
  ylab = "Total within cluster sum of squares"
)


hc_complete <- hclust(dist(x), method = "complete")
hc_average <- hclust(dist(x), method = "average")
hc_single <- hclust(dist(x), method = "single")

par(mfrow = c(1, 3))
plot(hc_complete, main = "Complete linkage", xlab = "", 
     ylab = "", sub = "")
plot(hc_average, main = "Average linkage", xlab = "", 
     ylab = "", sub = "")
plot(hc_single, main = "Single linkage", xlab = "", 
     ylab = "", sub = "")

# Az alapján, hogy hány csoportot akarsz
cutree(hc_complete, k = 2)
cutree(hc_average, k = 2)
cutree(hc_single, k = 2)

# Az alapján, hogy mien magasan vágod el
cutree(hc_complete, h = 5)
cutree(hc_average, h = 4)
cutree(hc_single, h = 1.4)


par(mfrow = c(1,1))
x_scaled <- scale(x)
plot(hclust(dist(x_scaled), method = "complete"), 
     main = "Hierarchical clustering with scaled features")


set.seed(5)
x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))

plot(hclust(dd, method = "complete"), 
     main = "Complete linkage with correlation-based distance",
     xlab = "", sub = "")