library(rgl)
library(ggplot2)
library(scatterplot3d)
# Mit Korrelationsmatrix probieren
M <- matrix(c(1,0.2,0.9,
              0.2,1,0.7,
              0.9,0.7,1), 
            nrow = 3, ncol = 3)

# add column and row names
colnames(M) <- c("x", "y", "z")
rownames(M) <- c("x", "y", "z")

# seed fuer stabile Zahlen
set.seed(42)
# Cholesky Decomposition
n <- 1000
L <- chol(M)
nvars = dim(L)[1]
r <- t(L) %*% matrix(rnorm(nvars*n), nrow=nvars, ncol=n)
r <- t(r)
rdata = as.data.frame(r)
C <- round(cor(rdata), 2)

# Definition der Max- und Min Werte der Variablen
Max <- c(100,100,100)
Min <- c(0,0,0)

max <- apply(rdata, 2, max)
min <- apply(rdata, 2, min)
dat <- rdata
#Normierung zwischen 0 und 1: Verhaeltnis zwischen aktueller und max Spanne ausrechnen
for (i in 1:3) {
  dat[,i] <- (dat[,i] - min[i])/(max[i]-min[i])
}
# Dehnung zw. Min und Max
for (i in 1:3) {
  dat[,i] <- Min[i] + dat[,i] * (Max[i] - Min[i])
}
plot3d(dat, col = "steelblue", size = 5)
pairs(dat)
p1 <- scatterplot3d(dat$x, dat$y, dat$z, color = "steelblue", pch = 16)
p2 <- scatterplot3d(dat$y, dat$z, dat$x, color = "steelblue", pch = 16)
p3 <- scatterplot3d(dat$z, dat$x, dat$y,  color = "steelblue", pch = 16)
cor(dat)

hk <- princomp(~ x + y + z,
               cor=TRUE, data=dat) # berechnet die Hauptkomponenten
#https://statquest.org/2015/08/13/pca-clearly-explained/
hk <- prcomp(dat)
loadings(hk)
summary(hk)
summary(hk$scores)
str(hk)
windows()
plot(hk2$x[,1], hk2$x[,2], col = "red",pch = 19,
     main = "Reduced to two axes",
     xlab = "Principal component 1",
     ylab = "Principal component 2")
class(hk)
