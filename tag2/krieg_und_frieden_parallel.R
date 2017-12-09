# Pakete installieren
#install.packages("readtext")
#install.packages("tm")
#install.package("parallel")

# Pakete laden
library(wordcloud)
library(readtext)
library(tm)
library(parallel)

### Funktion, nimmt als Argument den Filename entgegen, gibt einen Wordcount DF zurück
wrapper <- function(fileName) {
  ### Text laden ###
  textRaw <- readtext(fileName, encoding="utf8")
  # wordList leeren
  
  
  
  ### Text bereinigen ###
  # Satzzeichen rausnehmen
  text_no_punct <- removePunctuation(textRaw$text)
  text_no_linebreaks <- gsub("[\r\n]", " ", text_no_punct)
  text_no_special_chars <- gsub("[^[:alnum:]///' ]", " ", text_no_linebreaks) #https://stackoverflow.com/questions/11970891/r-remove-special-characters-from-data-frame
  
  text_clean <- text_no_special_chars
  
  ### Map bilden ###
  # Text in Vector mit einzelnen Wörtern aufsplitten
  wordList <- strsplit(text_clean, " ")
  wordTable <- table(wordList)
  wordDf <- as.data.frame(wordTable)
  print("Teil-Aggregation")
  print(proc.time() - startzeit)
  # stopwords rausnehmen
  #wordDf <- wordDf[!wordDf$wordList %in% stopwords(kind = "de"),]
  return(wordDf)
  
  #freq <- rep.int(1, length(words)) damit fügt man jedem Wort die Frequenz 1 an, macht die Table Funktion obsolet
}

### Startzeit ausgeben ###
startzeit <- proc.time() 

wordList <- c("")
Freq <- c(0)
big_df <- data.frame(wordList, Freq) #empty data.frame, will collect all results

for (chapter in 1:3) {
  fileName = paste0("dummy_text/chapter_", chapter, ".txt")
  #fileName = paste0("krieg_und_frieden/test", chapter, ".txt")
  chapter_df <- wrapper(fileName)
  
  ### Reduce part 1###
  #rbind all dataframes into one
  #print(chapter_df[chapter_df$wordList == "und",]) #double check der Anzahl "und"
  big_df <-rbind(big_df, chapter_df)
  
}
print("Zeit nach erstellen der einzelnen reduzierten Maps")
elapsed1 <- proc.time() - startzeit
print(elapsed1)
### Reduce part 2 ###
data <- aggregate(Freq ~ wordList, data=big_df, FUN=sum)
print(object.size(big_df))
wordsVec <- data$wordList
FreqsVec <- data$Freq

# Test Anzahl jmggg, sollte 4 sein
print(data[data$wordList == "jmggg",])

### Zeit ausgeben ###
print ("Zeit vor Darstellen der Wolke")
elapsed <- proc.time() - startzeit
print(elapsed)
### Wolke ausgeben ###
my_colors <- c("black", "red", "blue")
wordcloud(wordsVec, FreqsVec, min.freq = 1, colors = my_colors, max.words = 100)



