n<-1000
rand <- runif(n)
# read PLZ
raw.plz <- read.csv2("../tag3/Postleitzahlen-Schweiz.csv")
plz <- raw.plz$Postleitzahl...Code.Postal...Codice.Postale
ort <- raw.plz$Ort...Ville...Città
kanton <- raw.plz$Kanton
lookup.raw <- data.frame(plz, kanton)
lookup <- unique(lookup.raw)

# Büsingen entfernen
lookup <- subset(lookup, kanton != "Büsingen" & kanton!= "IT" & kanton != "Fürstentum Lichtenstein")

# id of people
id <- 1:n

# age
age <- as.integer(16 + 80 * rbeta(n, 5,9))

# income, aghängig vom Alter
income <- as.integer(age * 1000 + 60000 * rbeta(n, 9,5))

# gender
gender <- sample(c("m", "f"), n, prob = c(0.7, 0.3), replace = TRUE)

# education
education <- sample(c("obligatorisch", "sekundaer", "tertiaer"), n, prob = c(0.2, 0.46, 0.34), replace = TRUE)

# interested in craftbeer
#craftbeer <- sample(c("y", "n"), n, replace = TRUE)
craftbeer <- ifelse(education == "tertiaer", ifelse(rand < 0.7, "y", "n"), ifelse(education == "sekundaer", ifelse(rand < 0.5, "y", "n"), ifelse(rand < 0.3, "y", "n")))

# beer consumption per year
#consumption <- ifelse(gender == "m", 30 + 50 * rbeta(n, 1,1), 28 + 50 * rbeta(n, 1,1))
consumption <- ifelse(gender == "m", 110 * rbeta(n, 2.3,2), 110 * rbeta(n, 2,2.3))

# create df
plz <- sample(plz, n, replace = TRUE)


# combine to dataframe
data <- data.frame(id, plz, income, gender, education, age, craftbeer, consumption)

d <- merge(lookup, data, by = "plz")
