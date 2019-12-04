library(data.table)
library(ggplot2)

data <- fread("data/sales_data_for_clustering.csv")

set.seed(20191204)

cluster <- data[, c("product_id", "price", "user_id", "quantity")]
pca_out <- prcomp(cluster, scale = TRUE)
biplot(pca_out, scale = 0)

cluster2 <- data[, c("product_id", "quantity", "user_id")]

ks <- 1:5
tot_within_ss <- sapply(ks, function(k){
  km_output <- kmeans(cluster2, centers = k, nstart = 20)
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

km_output <- kmeans(cluster, centers = 2, nstart = 100)
km_output

ggplot(data, aes(x = data$product_id, y = data$quantity)) + geom_point(colour = (km_output$cluster + 1), size = 1)

# hc_complete <- hclust(dist(data), method = "complete")
# plot(hc_complete, main = "Complete linkage", xlab = "", 
#      ylab = "", sub = "")

#########################