#Kod odpowiedzialny za stworzenie korpusu dokumentów
library(tm)
library(hunspell)

setwd("./")

inputDir <- "./data"
outputDir <- "./results"

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

#Kod odpowiedzialny za przetwarzanie wczytanego korpusu dokumentów

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


#Kod odpowiedzialny za stworzenie odpowiednich macierzy rzadkich
dtmBinAll4_8 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightBin,
    bounds = list(
      global = c(4,8)
    )
  )
)

dtmTfAll <- DocumentTermMatrix(corpus)

dtmTfIdf19_20 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(19,20)
    )
  )
)


# Funkcja służąca do wykonywania analizy pca oraz rysowania jej wyników
# Wykres pojawia się w prawej dolne części interfejsu R Studio
# W zakładce Plots
generate_pca_chart_for_matrix <- function(matrix) {
  usedMatrix <- matrix
  pca <- prcomp(usedMatrix)
  legend <- paste(paste("d", 1:usedMatrix$nrow,sep = ""), rownames(usedMatrix),sep = " => ")
  x <- pca$x[,1]
  y <- pca$x[,2]
  options(scipen = 5)
  plot(
    x,
    y, 
    col = "purple",
    main = "Analiza głównych składowych",
    xlab = "PC1",
    ylab = "PC2",
    pch = 18
  )
  text(
    x,
    y,
    paste("d", 1:usedMatrix$nrow,sep = ""),
    col = "purple",
    pos = 4,
    cex = 0.8
  )
  legend(
    "bottom",
    legend,
    cex = 0.4,
    text.col = "purple"
  )
}

# Analiza PCA poszczególnych macierzy
# Wykresy generują się jeden po drugim
generate_pca_chart_for_matrix(matrix = dtmBinAll4_8)
generate_pca_chart_for_matrix(matrix = dtmTfAll)
generate_pca_chart_for_matrix(matrix = dtmTfIdf19_20)


