########################################
# Korrelationsmatrix erstellen         #
########################################

M <- matrix(c(1,0.2,0.3,0.7,
            0.2,1,0.72,0.68,
            0.3,0.72,1,0.64,
            0.7,0.68,0.64,1), 
            nrow = 4, ncol = 4)

# add column and row names
colnames(M) <- c("distanz", "age", "income", "consumption")
rownames(M) <- c("distanz", "age", "income", "consumption")

# edit
#M <- edit(M)

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
Max <- c(1000, 90, 210000, 80)
Min <- c(30, 16, 40000, 23)

max <- apply(rdata, 2, max)
min <- apply(rdata, 2, min)
dat <- rdata
#Normierung zwischen 0 und 1: Verhaeltnis zwischen aktueller und max Spanne ausrechnen
for (i in 1:4) {
  dat[,i] <- (dat[,i] - min[i])/(max[i]-min[i])
}
# Dehnung zw. Min und Max
for (i in 1:4) {
  dat[,i] <- Min[i] + dat[,i] * (Max[i] - Min[i])
}

head(dat)
cor(dat)
pairs(dat, col = "steelblue")
summary(dat)

# Meter umkehren
m2bar <- 1000 - dat$distanz + 30
head(m2bar)
summary(m2bar)
plot(m2bar, dat$distanz)
dat$m2bar <- m2bar
head(dat)
cor(dat)
pairs(dat, col = "steelblue", main = "Korrelogramm des Datensatzes")
summary(dat)

# Geschlecht zum DF hinzunehmen. Maenner trinken mehr als Frauen
library(ggplot2)
#generate n random numbers
rand <- runif(n)
dat$gender <- ifelse(rand < (dat$consumption - 23)/(80-23), "m", "f")
qplot(gender, consumption, fill = gender, data = dat, geom = "boxplot")
head(dat)

# read PLZ
raw.plz <- read.csv2("../tag3/Postleitzahlen-Schweiz.csv")
plz <- raw.plz$Postleitzahl...Code.Postal...Codice.Postale
ort <- raw.plz$Ort...Ville...CittÃ 
kanton <- raw.plz$Kanton
lookup.raw <- data.frame(plz, kanton)
lookup <- unique(lookup.raw)

# Buesingen entfernen
lookup <- subset(lookup, kanton != "Büsingen" & kanton!= "IT" & kanton != "Fürstentum Lichtenstein")

# id of people
id <- 1:n

# education
education <- sample(c("obligatorisch", "sekundaer", "tertiaer"), n, prob = c(0.2, 0.46, 0.34), replace = TRUE)

# interested in craftbeer
#craftbeer <- sample(c("y", "n"), n, replace = TRUE)
craftbeer <- ifelse(education == "tertiaer", ifelse(rand < 0.7, "y", "n"), ifelse(education == "sekundaer", ifelse(rand < 0.5, "y", "n"), ifelse(rand < 0.3, "y", "n")))

# create df
plz <- sample(plz, n, replace = TRUE)

# combine to dataframe
dat$id <- id
dat$plz <- plz
dat$education <- education
dat$craftbeer <- craftbeer
dat$distanz <- NULL

d <- merge(lookup, dat, by = "plz")
str(d)
# turn gender, education and craftbeer into factors
d$gender <- as.factor(d$gender)
d$education <- as.factor(d$education)
d$craftbeer <- as.factor(d$craftbeer)
str(d)
# beautifying: round age and income
d$age <- round(d$age, 0)
d$income <- round(d$income, 0)
str(d)
head(d)

############################################
# Daten Ueberblick gewinnen                #
############################################

# Histogramme
library(Hmisc)
windows()
hist(d)

# Correlogramme
# reduzierten df mit nur numeric Werte erstellen
str(d)
d.numeric <- data.frame(d$age, d$income, d$consumption, d$m2bar)
library(PerformanceAnalytics)
chart.Correlation(d.numeric,histogram=TRUE,pch=19, main = "Korrelogramm")

