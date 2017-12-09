library(qgraph)
windows()
qgraph(dat, 
       layout = "spring")


Var1 <- c("Reto", "Alex")
Var2 <- c("Peter", "Adrian", "Christian", "Claude", "Marco", "Claudia")
Freq <- c(5,2,3,2,1,4)
example <- data.frame(Var1, Var2, Freq)
example

example <- cbind(data.frame("Claude"))

#########################################
##     NetworkD3                       ##
#########################################
library(networkD3)
library(magrittr)

windows()
simpleNetwork(dat) %>%
  saveNetwork(file = "net.html")

##########################################
##         igraph example               ##
##########################################
