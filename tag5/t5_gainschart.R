#####################################
# Voraussagen machen                #
#####################################

head(neu)
summary(neu)
str(neu)

# Craftbeer entfernen
neu1 <- neu
head(neu1)
neu1$craftbeer <- NULL

# Voraussagen machen mit meinem glm model
neu1$prob_craftbeer <- predict.glm(glm.final, newdata = neu1, type = "response")

# sortieren fuer gainchart
gainchart <- neu1[order(- neu1$prob_craftbeer), ]

write.csv(gainchart, "gainchart.txt")
