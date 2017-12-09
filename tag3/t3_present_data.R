library(ggplot2)

# show info of data set
str(d)
print(head(d))

### Comparison ###
# barplot
# first compute means
frauen <- d[d$gender == "f", ]
maenner <- d[d$gender == "m", ]
f_mean <- mean(frauen$income)
m_mean <- mean(maenner$income)
Geschlecht <- c("Männer", "Frauen")
Einkommen <-c(m_mean, f_mean)
colDat <- data.frame(Geschlecht, Einkommen)

# nach http://ggplot2.tidyverse.org/reference/geom_bar.html
barplot_av_income <- ggplot(colDat, aes(Geschlecht, Einkommen, fill = Geschlecht)) +
  geom_col() + ggtitle("Durchschnittliches Jahreseinkommen nach Geschlecht")

ggsave("barplot_av_income.png", plot = barplot_av_income)


# boxplot
boxplot_gender_consumption <- qplot(gender, consumption, data = d, fill = gender, geom = "boxplot", xlab="Geschlecht", 
      ylab="Konsumation in Litern pro Jahr", main="Konsumation in Litern pro Jahr")

ggsave("boxplot_gender_consumption.png", plot = boxplot_gender_consumption)

### composition ###
# Pie chart
pie_kanton_1 <- ggplot(d, aes(x=factor(1), fill=kanton))+
  geom_bar(width = 1)+
  coord_polar("y")

ggsave("pie_kanton_1.png", plot = pie_kanton_1)

# pie chart from r core
kant.raw <- as.data.frame(table(d$kanton))
# 0-Werte entfernen
kant <- kant.raw[kant.raw$Freq > 0, ]
png("pie_kanton_2.png", width=600, height=600, res=80)
pie(kant$Freq, labels = kant$Var1, main="Anzahl Teilnehmer pro Kanton")
dev.off()



### relation ###
# income depending on age

dotplot_age_income <- qplot(age, income, data = d, color = gender, 
                            xlab = "Alter", ylab = "Lohn", main = "Lohn in Beziehung zum Alter")
ggsave("dotplot_age_income.png", plot = dotplot_age_income)



### distribution ###

#consumption
histogram_consumption <- qplot(consumption, data = d, geom = "histogram", binwidth = 2, fill = I("orange"))
ggsave("histogram_consumption.png", plot = histogram_consumption)


# Allenfalls zum Spass noch eine mehrdimensionale Graphik einbauen
# ggplot2 examples


# Kernel density plots for mpg
# grouped by number of gears (indicated by color)
kernel_density <- qplot(consumption, data=d, geom="density", fill=education, alpha=I(.5),
      main="Verteilung der Jahreskonsumation in Litern", xlab="Konsumation",
      ylab="Dichte")
ggsave("kernel_density.png", kernel_density)

# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
facets_dotplot <- qplot(age, income, data=d, shape=craftbeer, color=craftbeer,
      facets=education~gender, size=I(3),
      xlab="age", ylab="Income")
ggsave("facets_dotplot.png", facets_dotplot)

# Separate regressions of mpg on weight for each number of cylinders
smoth_dotplot <- qplot(consumption, age, data=d, geom=c("point", "smooth"),
      method="lm", formula=y~x, color=gender,
      main="Regression of consumption on age",
      xlab="age", ylab="consumption")
ggsave("smoth_dotplot.png", smoth_dotplot)

# Boxplots of mpg by number of gears
# observations (points) are overlayed and jittered
jitter_boxplot_consumption_gender <- qplot(gender, consumption, data=d, geom=c("boxplot", "jitter"),
      fill=gender, main="Consumption depending on gender",
      xlab="", ylab="consumption") 
ggsave("jitter_boxplot_consumption_gender.png", jitter_boxplot_consumption_gender)


