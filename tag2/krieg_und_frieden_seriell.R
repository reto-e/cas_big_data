# Pakete installieren
#install.packages("readtext")
#install.packages("tm")

# Pakete laden
library(wordcloud)
library(readtext)
library(tm)

### Startzeit ausgeben ###
startzeit <- proc.time() 
### Text laden ###
textRaw <- readtext("dummy_text/all.txt", encoding="utf8")


### Text bereinigen ###
# Satzzeichen rausnehmen
text_no_punct <- removePunctuation(textRaw$text)
text_no_linebreaks <- gsub("[\r\n]", " ", text_no_punct)
text_no_special_chars <- gsub("[^[:alnum:]///' ]", " ", text_no_linebreaks) #https://stackoverflow.com/questions/11970891/r-remove-special-characters-from-data-frame

text_clean <- text_no_special_chars

### Map bilden ###
# Text in Vector mit einzelnen Wörtern aufsplitten
wordList <- strsplit(text_clean, " ")
print("wordlist errstellt")
print(proc.time() - startzeit)

wordTable <- table(wordList)
print("wordlist in table aggregiert")
print(proc.time() - startzeit)

wordDf <- as.data.frame(wordTable)

wordsVec <- unlist(wordDf$wordList)
FreqsVec <- unlist(wordDf$Freq)

# Test Anzahl jmggg, sollte 4 sein
print(wordDf[wordDf$wordList == "jmggg",])


### Zeit ausgeben ###
elapsed <- proc.time() - startzeit
print(elapsed)
### Wolke ausgeben ###
my_colors <- c("black", "red", "blue")
windows()
wordcloud(wordsVec, FreqsVec, min.freq = 1, colors = my_colors, max.words = 200, random.order = FALSE)
