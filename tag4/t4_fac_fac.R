### Kat - Kat Test: je höher die Bildung, desto höher der Craft beer Konsum #####
library(ggplot2)
library(DiagrammeR)
library(PropCIs)
library(Hmisc)

##################
# Prozente "ja" berechnen, getrennt nach "obl", "sek" und "ter"
##################
Tab <- table(d$education, d$craftbeer) 

Proz <- prop.table(Tab, 1) # Prozentwerte, der Margin von 1 bewirkt, dass pro Zeile der Anteil von 100% berechnet wird.

oblyP <- Proz[1,2]
sekyP <- Proz[2,2]
teryP <- Proz[3,2]


oblyPP <- 100 * round(oblyP, 2)
sekyPP <- 100 * round(sekyP, 2)
teryPP <- 100 * round(teryP, 2)

bars <- c(oblyP, sekyP, teryP)
labels <- c(paste0(oblyPP,"%"), paste0(sekyPP, "%"), paste0(teryPP, "%"))

# Plot Barplot
par(bg = "white")
bp <- barplot(bars, xlab = "Bildung", ylab = "Anteil Craftbeer Konsumenten in %",
        main = "Anteil Craftbeer Konsumenten abhängig vom Bildungsstand",
        ylim = c(0, 1),
        border = NA,
        col = "steelblue4",
        names.arg = c("obligatorisch","sekundär","tertiär"))
text(bp, bars, labels = labels, pos = 1, offset = 4, col = "white")
# add 95 confidenz Intervals
obly <- Tab[1,2]
obl <- Tab[1,1] + Tab[1,2]
res.obl <- add4ci(obly, obl, 0.95) # calculates 95% confidence interval

seky <- Tab[2,2]
sek <- Tab[2,1] + Tab[2,2]
res.sek <- add4ci(seky, sek, 0.95)

tery <- Tab[3,2]
ter <- Tab[3,1] + Tab[3,2]
res.ter <- add4ci(tery, ter, 0.95)


p.obl <- res.obl$estimate
l.obl <- res.obl$conf.int[1]
u.obl <- res.obl$conf.int[2]

p.sek <- res.sek$estimate
l.sek <- res.sek$conf.int[1]
u.sek <- res.sek$conf.int[2]

p.ter <- res.ter$estimate
l.ter <- res.ter$conf.int[1]
u.ter <- res.ter$conf.int[2]


heights <- c(p.obl, p.sek, p.ter)
upper <- c(u.obl, u.sek, u.ter)
lower <- c(l.obl, l.sek, l.ter)

# add confidence intervals to plot
errbar(bp[,1], heights, upper, lower, add=T, lwd = 2)

### Signifikanz mit dem Chi Quadrat Test ####
my_chi <- chisq.test(Tab, correct = FALSE)

my_fisher <- fisher.test(Tab, alternative = "greater")



#####################
# Graph für Hypothese #
#######################
mermaid("
        graph LR
        A(Bildung<br>obl, sek, ter) --> B(Craftbeer Konsum<br>y, n)
        ")
