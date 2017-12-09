library(simputation)

# vector mit zufallszahlen
x <- sample(1:n, 20)
# add NA for income
income.sample <- d[x, ]

# add NA
d[x,4] <- NA

# replace NA for income using correlation from age
d <- impute_lm(d, income ~ age)

# neuer vector mit Zufallszahlen
y <- sample(1:n, 20)

# add NA for education
education.sample <- d[y, ]
d[y,6] <- NA

# d2 <- impute_cart(d, education ~ craftbeer) # Decision tree abhängig von allen Faktoren war besser
d <- impute_cart(d, education ~ .)
#replace NA for education using 