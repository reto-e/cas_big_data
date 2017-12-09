library(sem, pos=15)

library(rio)
dat <- import("C:/Users/reei/Documents/CAS_BigData/tag6/Ausland2.txt")
d <- dat

# Lösche alle Merkmale, bis auf a1, ..., a15
d$nr <- d$ewv <- d$sozeng <- d$stellung <- d$gebjg <- d$geschl <- NULL

fit <- c('Feind: a4, a12, a13, a14, a15', 
         'Freund: a1, a2, a5, a8, a9, a11',
         'Bedenken: a3, a6, a7, a10')
fit <- cfa(file=textConnection(fit), reference.indicators=FALSE)
dd <- d[, c('a4', 'a12', 'a13', 'a14', 'a15', 'a1', 'a2', 'a5', 'a8', 
            'a9', 'a11', 'a3', 'a6', 'a7', 'a10')]
summary(sem(fit, data=dd), robust=FALSE, fit.indices=c("AIC","BIC"))

#------------------------------------------------------------
dd <- import("C:/Users/reei/Documents/CAS_BigData/tag6/beer_answers.txt")

fit <- c('Einstellung: e1, e2, e3', 
         'Typ: t1, t2, t3',
         'Lokal: l1, l2, l3')
fit <- cfa(file=textConnection(fit), reference.indicators=FALSE)

summary(sem(fit, data=dd), robust=FALSE, fit.indices=c("AIC","BIC"))

#---------------------------------------------------------------

hk <- princomp(~ e1 + e2 + e3 + t1 + t2 + t3 + l1 + l2 + l3,
               cor=TRUE, data=dd) # berechnet die Hauptkomponenten
loadings(hk)
hk$sd^2
summary(hk)

r <- cor(dd)
library(psych)
pc <- principal(r, nfactors=3)
pc

