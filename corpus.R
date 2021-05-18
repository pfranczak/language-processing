#włączenie bibliotek
library(tm)
library(hunspell)

setwd("./")

#definicja lokalizacji katalogów funkcjonalnych
inputDir <- "./data"
outputDir <- "./results"

#utworzenie wybranych katalogów funkcjonalnych
dir.create(outputDir, showWarnings = F)

#utworzenie korpusu dokumentów
corpusDir <- paste(inputDir)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    encoding = "UTF-8",
    pattern = "*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#wstępne przetwarzanie
pasteParagraphs <- function(text){
  paste(text, collapse = " ")
}
corpus <- tm_map(corpus, content_transformer(pasteParagraphs))

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))

removeChar <- content_transformer(
  function(text,char){
    gsub(char, " ", text)
  }
)
corpus <- tm_map(corpus, removeChar, intToUtf8(8722))
corpus <- tm_map(corpus, removeChar, intToUtf8(190))

#lematyzacja
polish <- dictionary(lang = "pl_PL")
lemmatize <- function(text){
  tokenizedText <- unlist(hunspell_parse(text, dict = polish))
  lemmatizedVec <- hunspell_stem(tokenizedText, dict = polish)
  for (t in 1:length(lemmatizedVec)){
    if (length(lemmatizedVec[[t]]) == 0) lemmatizedVec[t] <- tokenizedText[t]
    if (length(lemmatizedVec[[t]])  > 1) lemmatizedVec[t] <- lemmatizedVec[[t]][1]
  }
  lemmatizedText <- paste(lemmatizedVec, collapse = " ")
  return(lemmatizedText)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

stoplistFile <- paste(
  "./",
  "stopwords_pl.txt",
  sep = "/"
)
stoplist <- readLines(stoplistFile)
corpus <- tm_map(corpus, removeWords, stoplist)

corpus <- tm_map(corpus, stripWhitespace)

cutExtension <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$",
    "",
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cutExtension)

#zapis korpusu do plików tekstowych
preprocessedDir <- paste(
  "./processed"
)
dir.create(preprocessedDir, showWarnings = F)
writeCorpus(corpus, path = preprocessedDir)

