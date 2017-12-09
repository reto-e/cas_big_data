
generateWord <- function(word_length) {
  myLetters <- sample(letters, word_length, replace = TRUE)
  word <- paste(myLetters, collapse = "")
  word
}

generateText <- function(text_length) {
  text <- list()
  wordList <- list()
  for (i in 1:text_length) {
    word <- generateWord(5)
    wordList[i] <- word
    text <- (paste(wordList, collapse = " "))
  }
  text
}

# startzeit <- proc.time()
# for (i in 1:20) {
#   text <- generateText(10000)
#   write(text, file = "dummy_text/chapter_3.txt", append = TRUE)
# }
# print(proc.time() - startzeit)