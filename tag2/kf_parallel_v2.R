# Pakete installieren
#install.packages("readtext")
#install.packages("tm")
#install.package("parallel")
#install.packages("snowfall")

# Pakete laden
library(wordcloud)
library(readtext)
library(tm)

### Funktion, nimmt als Argument den Filename entgegen, gibt eine Map zurück
map <- function(key, fileName) {
  ### Text laden ###
  textRaw <- readtext(fileName, encoding="utf8")
  
  
  ### Text bereinigen ###
  # Satzzeichen rausnehmen
  text_no_punct <- removePunctuation(textRaw$text)
  text_no_linebreaks <- gsub("[\r\n]", " ", text_no_punct)
  text_no_special_chars <- gsub("[^[:alnum:]///' ]", " ", text_no_linebreaks) #https://stackoverflow.com/questions/11970891/r-remove-special-characters-from-data-frame
  
  text_clean <- text_no_special_chars
  
  ### Map bilden ###
  # Text in Vector mit einzelnen Wörtern aufsplitten
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

for (chapter in 1:3) {
  fileName = paste0("dummy_text/chapter_", chapter, ".txt")
  key <- paste0("kapitel_", chapter)
  #fileName = paste0("krieg_und_frieden/test", chapter, ".txt")
  chapter_df <- map(key, fileName)
  
  ### Reduce part 1###
  #rbind all dataframes into one
  #print(chapter_df[chapter_df$wordList == "und",]) #double check der Anzahl "und"
  allMaps <-rbind(allMaps, chapter_df)
  
}

### Reduce part 2 ###
# Zwischenzeit
print("Zeit nach erstellen der einzelnen Maps")
print(proc.time() - startzeit)
allMaps.sorted <- allMaps[order(allMaps$wordVector),]
data <- aggregate(freqVector ~ wordVector, data=allMaps.sorted, FUN=sum) # dieser Schritt dauerte sehr lange mit unsortiertem data.frame
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



