library(PropCIs) # fuer Konfidenzintervalle
library(Hmisc) # Fuer die Darstellung der Konfidenzintervalle auf den Barcharts
library(Rmisc) # Fuer die Funktion CI

Tab <- aggregate(d$consumption ~ d$gender, FUN = CI)

f.mean <- Tab[1,2][2]
f.lower <- Tab[1,2][3]
f.upper <- Tab[1,2][1]

m.mean <- Tab[2,2][2]
m.lower <- Tab[2,2][3]
m.upper <- Tab[2,2][1]

heights <- c(f.mean, m.mean)
lower <- c(f.lower, m.lower)
upper <- c(f.upper, m.upper)

labels <- round(heights, 2)
# barplot erstellen

bp <- barplot(heights, xlab = "Geschlecht", ylab = "Bierkonsum in Litern",
              main = "Durschnittlicher Bierkonsum pro Jahr in Litern nach Geschlecht",
              ylim = c(0, 65),
              border = NA,
              col = "steelblue4",
              names.arg = c("Frauen","Männer"))
text(bp, heights, labels = labels, pos = 1, offset = 4, col = "white")

# add confidence intervals to plot
errbar(bp[,1], heights, upper, lower, add=T, lwd = 2)

fit <- aov(consumption ~ gender, data=d) # Varianzanalyse
summary(fit)

t_test <- t.test(consumption ~ gender, data = d)
