library(rgl)
# Get file
myData <- read.csv("C:/Users/reei/Documents/CAS_BigData/tag6/bierkunden.csv")
str(myData)
summary(myData)

# Clean data, reduce to numeric values used for cluster analysis
dat <- myData
dat$plz <- dat$kanton <- dat$gender <- dat$id <- dat$education <- dat$craftbeer <- NULL
head(dat)

# how many clusters
# Determine number of clusters
wss <- (nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dat,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# Clustering mit k-means
fit <- kmeans(dat, 4, nstart = 10)
summary(fit)

# add cluster to dat
dat$Cluster <- fit$cluster
head(dat)

# have a look at clusters
windows()
pairs(dat[1:5],
      main = "Clusters",
      pch = 21,
      bg = c("red", "green2", "steelblue4", "black")[unclass(dat$Cluster)])

# Can I predict the consumption of a cluster?
fit2 <- lm(consumption ~ Cluster, data = dat)
summary(fit2)  # nope.

# have a 3d look at clusters
plot3d(dat$consumption, dat$age, dat$m2bar, col = as.integer(dat$Cluster), size = 5)

#add cluster to myData
myData$cluster <- fit$cluster
pairs(myData)


# vary parameters for most readable graph
library(cluster)
clusplot(dat, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(dat, fit$cluster) 

# predict species with Iris dataset

dat3 <- iris
dat3$Species <- NULL
kmIris <- kmeans(dat3, 3, nstart = 1)
kmIris
dat3$cluster <- kmIris$cluster
table(iris$Species, dat3$cluster)
