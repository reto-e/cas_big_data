################################################
# Model für Craftbeer trainineren              #
################################################

# zuerst craftbeer vom trainigsset von y/n auf 1/0 aendern
training$craftbeer <- ifelse(training$craftbeer == "y", 1, 0)

# urmodel rechnen
glm0 <- glm(craftbeer ~ ., family = binomial(logit), data = training)
summary(glm0)

# bestes model mit stepAIC erstellen
library(MASS)
glm1 <- stepAIC(glm0, direction = "both", k = 10)
summary(glm1)

glm2 <- stepAIC(glm0, direction = "both", k = 2)
summary(glm2)


# test glm
glm.final <- glm(craftbeer ~ consumption + gender + education, family = binomial(logit), data = training)
summary(glm.final)

# eigene Modelle erstellen
glm.own <- glm(craftbeer ~ gender + education, family = binomial(logit), data = training)
summary(glm.own)

# testdaten bereinigen für Validierung
test.clean <- test
test.clean$craftbeer <- ifelse(test.clean$craftbeer == "y", 1, 0)
test2 <- test.clean
test2$craftbeer <- NULL

# neue funktion predit.glm macht umweg ueber z-wert ueberfluessig
prob_craftbeer <- predict.glm(glm.final, newdata = test2, type = "response")

# validierung
qplot(test$craftbeer, prob_craftbeer, geom = "boxplot", 
      main = "Uebersicht Modell Craftbeer Wahrscheinlichkeit",
      fill = test$craftbeer,
      xlab = "echte Werte",
      ylab = "vorausgesagte Wahrscheinlichkeit")




