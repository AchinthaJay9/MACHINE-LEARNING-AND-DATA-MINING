boxplot(Whitewine_v2$`fixed acidity`)
boxplot(Whitewine_v2$`volatile acidity`)
boxplot(Whitewine_v2$`citric acid`)
boxplot(Whitewine_v2$`residual sugar`)
boxplot(Whitewine_v2$chlorides)
boxplot(Whitewine_v2$`free sulfur dioxide`)
boxplot(Whitewine_v2$`total sulfur dioxide`)
boxplot(Whitewine_v2$density)
boxplot(Whitewine_v2$pH)
boxplot(Whitewine_v2$sulphates)
boxplot(Whitewine_v2$alcohol)


detect_outlier <- function(x) {

  # calculate first quantile
  Quantile1 <- quantile(x, probs=.25)

  # calculate third quantile
  Quantile3 <- quantile(x, probs=.75)

  # calculate inter quartile range
  IQR = Quantile3-Quantile1

  # return true or false
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

# create remove outlier function
remove_outlier <- function(dataframe,
                            columns=names(dataframe)) {

  # for loop to traverse in columns vector
  for (col in columns) {

    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }

  # return dataframe
  print("Remove outliers")
  print(dataframe)
}

outlier_remove_data <- remove_outlier(Whitewine_v2, c('fixed acidity','volatile acidity','citric acid','residual sugar','chlorides','free sulfur dioxide','total sulfur dioxide','density', 'pH','sulphates','alcohol'))
print(outlier_remove_data)

boxplot(outlier_remove_data$`fixed acidity`)
boxplot(outlier_remove_data$`volatile acidity`)
boxplot(outlier_remove_data$`citric acid`)
boxplot(outlier_remove_data$`residual sugar`)
boxplot(outlier_remove_data$chlorides)
boxplot(outlier_remove_data$`free sulfur dioxide`)
boxplot(outlier_remove_data$`total sulfur dioxide`)
boxplot(outlier_remove_data$density)
boxplot(outlier_remove_data$pH)
boxplot(outlier_remove_data$sulphates)
boxplot(outlier_remove_data$alcohol)

scale_data <- as.data.frame(scale(outlier_remove_data)) #z score scale
scale_data$quality <- outlier_remove_data$quality
print(scale_data)

# Installing Packages
install.packages("ClusterR")
install.packages("cluster")

# Loading package
library(ClusterR)
library(cluster)


# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(scale_data, centers = 2, nstart = 100) #scale_data
kmeans.re

# Cluster identification for 
# each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(scale_data$quality, kmeans.re$cluster)
cm

kmeans.re <- kmeans(scale_data, centers = 3, nstart = 100)
kmeans.re

# Cluster identification for 
# each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(scale_data$quality, kmeans.re$cluster)
cm

kmeans.re <- kmeans(scale_data, centers = 4, nstart = 100)
kmeans.re

# Cluster identification for 
# each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(scale_data$quality, kmeans.re$cluster)
cm

# Installing Packages
install.packages("NbClust")
library("NbClust")

library(cluster) 
library(factoextra)

result <- NbClust(data = scale_data, distance = "euclidean", min.nc = 2, max.nc = 15, method = 'complete', index = "ch") #result
print(result$Best.nc)

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scale_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 100 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#silhouette 2
kmm <- kmeans(scale_data, centers = 2, nstart = 100)

D <- daisy(scale_data)

plot(silhouette(kmm$cluster, D), col=1:8, border=NA)

#silhouette 3
kmm <- kmeans(scale_data, centers = 3, nstart = 100)

D <- daisy(scale_data)

plot(silhouette(kmm$cluster, D), col=1:8, border=NA)

#silhouette 4
kmm <- kmeans(scale_data, centers = 4, nstart = 100)

D <- daisy(scale_data)

plot(silhouette(kmm$cluster, D), col=1:8, border=NA)




res.pca <- prcomp(outlier_remove_data[,c(1:11)], center = TRUE,scale. = TRUE)

summary(res.pca)

install.packages("factoextra")
library("factoextra")
fviz_eig(res.pca)




cumpro <- cumsum(res.pca$sdev^2 / sum(res.pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 9, col="blue", lty=5)
abline(h = 0.97395, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)



result <- NbClust(data = res.pca$x[,c(1:9)], distance = "euclidean", min.nc = 2, max.nc = 9, method = 'complete', index = "ch") #result
print(result$Best.nc)

wss <- sapply(1:4, 
              function(k){kmeans(res.pca$x[,c(1:9)], k, nstart=50,iter.max = 100 )$tot.withinss})
wss
plot(1:4, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kmm <- kmeans(res.pca$x[,c(1:9)], centers = 2, nstart = 100)

D <- daisy(res.pca$x[,c(1:9)])
