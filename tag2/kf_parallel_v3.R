# Pakete installieren
#install.packages("readtext")
#install.packages("tm")
#install.package("parallel")
#install.packages("snowfall")

# Pakete laden
library(wordcloud)
library(readtext)
library(tm)
library(parallel)
library(snowfall)

# Cluster aufsetzen
no_cores <- 3
sfInit(parallel = TRUE, cpus = no_cores)

### Funktion, nimmt als Argument den Filename entgegen, gibt eine Map zurÃ¼ck
map <- function(index) {
  ### Text laden ###
  # filename erstellen
  
  library(readtext)
  library(tm)
  
  fileName <- paste0("dummy_text/chapter_", index, ".txt")
  
  textRaw <- readtext(fileName, encoding="utf8")
  
  
  ### Text bereinigen ###
  # Satzzeichen rausnehmen
  text_no_punct <- removePunctuation(textRaw$text)
  text_no_linebreaks <- gsub("[\r\n]", " ", text_no_punct)
  text_no_special_chars <- gsub("[^[:alnum:]///' ]", " ", text_no_linebreaks) #https://stackoverflow.com/questions/11970891/r-remove-special-characters-from-data-frame
  
  text_clean <- text_no_special_chars
  
  ### Map bilden ###
  # Text in Vector mit einzelnen WÃ¶rtern aufsplitten
  wordList <- strsplit(text_clean, " ")
  wordVector <- unlist(wordList)
  freqVector <- rep.int(1, length(wordVector))
  wordDf <- data.frame(wordVector, freqVector)
  
  print("unaggregierten DF erstellt")
  print(proc.time() - startzeit)
  
  
  # stopwords rausnehmen
  #wordDf <- wordDf[!wordDf$wordList %in% stopwords(kind = "de"),]
  return(wordDf)
}

### Startzeit ausgeben ###
startzeit <- proc.time() 

# empty df for collecting maps
allMaps <- data.frame(wordVector = factor(), freqVector = numeric()) #empty data.frame, will collect all results

sfExport("allMaps")
sfExport("startzeit")

myList <- (1:3)
pr <- sfLapply(myList, map)
allMaps <- rbind(pr[[1]], pr[[2]], pr[[3]])

sfStop() # stop cluster

### Reduce part 2 ###
# Zwischenzeit
print("Zeit nach erstellen der einzelnen Maps")
print(proc.time() - startzeit)
allMaps.sorted <- allMaps[order(allMaps$wordVector),]
data <- aggregate(freqVector ~ wordVector, data=allMaps.sorted, FUN=sum) # dieser Schritt dauerte sehr lange mit unsortiertem data.frame
print(object.size(allMaps))
wordsVec <- data$wordVector
FreqsVec <- data$freqVector

# Test Anzahl jmggg, sollte 4 sein
print(data[data$wordVector == "jmggg",])


### Zeit ausgeben ###
print("Zeit vor Wolke darstellen")
elapsed2 <- proc.time() - startzeit
print(elapsed2)
### Wolke ausgeben ###
my_colors <- c("black", "red", "blue")
windows()
wordcloud(wordsVec, FreqsVec, min.freq = 1, colors = my_colors, max.words = 200, random.order = FALSE)



