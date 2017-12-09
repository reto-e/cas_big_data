############################################
# Trainingsdatenset erstellen              #
############################################
set.seed(42)
d$rand_id <- round(runif(1006) * 100, 0)
# nach rand_id sortieren
library(plyr)
d <- arrange(d, rand_id)
# teilen in training und test und predict
training <- d[1:500, ]
training$rand_id <- NULL #rand_id wegschmeissen

test <- d[501:956, ]
test$rand_id <- NULL #rand_id wegschmeissen

neu <- d[957:1006, ]
neu$rand_id <- NULL

#############################################
# Modell erstellen                          #
#############################################

# Zuerst ein Modell fuer die consumption, also ein lineares Model.

fit0 <- lm(consumption ~ ., data = training)
summary(fit0)

# stepwise bestes Modell finden
library(MASS)
fit1 <- stepAIC(fit0, direction = "both", k = 2)
summary(fit1)

fit2 <- stepAIC(fit0, direction = "both", k = 3)
summary(fit2)

fit3 <- stepAIC(fit0, direction = "both", k = 4)
summary(fit3)

fit4 <- stepAIC(fit0, direction = "both", k = 5)
summary(fit4)

fit5 <- stepAIC(fit0, direction = "both", k = 6)
summary(fit5)

fit6 <- stepAIC(fit0, direction = "both", k = 7)
summary(fit6)

fit7 <- stepAIC(fit0, direction = "both", k = 8)
summary(fit7)

fit8 <- step(fit0, direction = "both", k = 9)
summary(fit8)

fit9 <- stepAIC(fit0, direction = "both", k = 20)
summary(fit9)
# step forward
fit10 <- stepAIC(fit0, direction = "forward", k = 2)
summary(fit10)

# step backward
fit11 <- step(fit0, direction = "backward", k = 2)
summary(fit11)

# my own guess
fit12 <- lm(consumption ~ m2bar + age + gender, data = training)
summary(fit12)

fit13 <- lm(consumption ~ m2bar + age, data = training)
summary(fit13)

# finaler fit
fit.final <- fit1
summary(fit.final)

# Einfluss der einzelnen Faktoren berechnen
library(relaimpo)
metrics <- calc.relimp(fit.final, type = "lmg")
plot(metrics)

# finalen fit rechnen: consumption ~ age + income + m2bar + gender + craftbeer
check <- lm(consumption ~ age + income + m2bar + gender + craftbeer, data = training)
summary(check)

# testset kopieren, dann consumption löschen
test1 <- test
test1$consumption <- NULL

# consumption mit fit.final vorraussagen
actual <- test$consumption
predict1 <- predict(fit.final, newdata = test1)

# validierung
rsq <- 1-sum((actual-predict1)^2)/sum((actual-mean(actual))^2)
print(rsq)
# Mit dieser Formel misst man den Abstand des predict Wertes zum actual Wert.
# Dieser wird ins Verhaeltnis gesetzt zum Abstand der actual Werte zu den Durchschnittswerten der Actual Werte
# Wenn alle Actual Werte sehr nah am Mittelwert sind (d. h. kaum Streuung), wiegt ein Fehler viel mehr als wenn 
# die Actual Werte sehr stark gestreut sind (also weit weg vom Mittelwert)

qplot(actual, predict1, geom="point",
      main = "Vergleich Vorhersagen mit echten Daten")



