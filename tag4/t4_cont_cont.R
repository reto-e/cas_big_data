library(ggplot2)
source("t3_generate_data.R")
source("t3_manipulate_data.R")

# plot regression
dot <- qplot(d$income, d$consumption, col = I("steelblue"),
             geom = c("jitter", "line"),
             main = "Beziehung des Jahreskonsums abhängig vom Einkommen",
             xlab = "Einkommen in CHF",
             ylab = "Jahreskonsum in Litern")
ggsave("dot_consumption_income.png", dot)

# lineares MOdell berechnen
fit <- lm(d$consumption ~ d$income)
summary(fit)



