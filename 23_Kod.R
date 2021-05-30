#Kod odpowiedzialny za stworzenie korpusu dokumentów
library(tm)
library(hunspell)
library(topicmodels)
library(wordcloud)
library(randomcoloR)

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

#analiza ukrytej alokacji Dirichlet'a
generate_topics_charts_for_matrix <- function(matrix, topicsNumber) {
  usedMatrix <- matrix
  lda <- LDA(
    usedMatrix,
    k = topicsNumber,
    method = "Gibbs",
    control = list(
      burnin = 2000,
      thin = 100,
      iter = 3000
    )
  )
  perplexity <- perplexity(lda, usedMatrix)
  results <- posterior(lda)
  par(mai = c(1,2,1,1))

  for (i in 1:topicsNumber) {
    topic <- tail(sort(results$terms[1,]),length(corpus))
    barplot(
      topic,
      horiz = T,
      las = 1,
      main = paste("Temat ",i),
      xlab = "Prawdopodobieństwo",
      col = randomColor()
    )
  }
  
  return(results)
}

generate_prop_charts_for_document <- function(results, documentsLength) {
  for (i in 1:documentsLength) {
    document <- results$topics[i,]
    barplot(
      rev(document),
      horiz = T,
      las = 1,
      main = rownames(results$topics)[i],
      xlab = "Prawdopodobieństwo",
      col = randomColor()
    )
  }
}

results <- generate_topics_charts_for_matrix(matrix = dtmTfAll, topicsNumber = 5)

generate_prop_charts_for_document(results = results, documentsLength = length(corpus))

# Identyfikacja słów i fraz kluczowych
generate_keywords_chart_for_matrix <- function(matrix, document) {
  usedMatrix <- matrix
  keywordsTf1 <- tail(sort(usedMatrix[1,]))
  rev(keywordsTf1)
  
  #waga Tf jako miara istotności słów
  keywordsTfIdf1 <- tail(sort(usedMatrix[1,]))
  rev(keywordsTfIdf1)
  
  #prawdopodobieństwo w LDA jako miara istorności słów
  termsImportance1 <- c(results$topics[1,]%*%results$terms)
  names(termsImportance1) <- colnames(results$terms)
  keywordsLda1 <- tail(sort(termsImportance1))
  rev(keywordsLda1)
  
  #chmura tagów
  par(mai = c(0,0,0,0))
  wordcloud(document, max.words = 150, color = brewer.pal(8,"PuOr"))
}

generate_keywords_charts <- function() {
  dtmTfIdfAll <- DocumentTermMatrix(
    corpus,
    control = list(
      weighting = weightTfIdf
    )
  )
  dtmTfIdfAllMatrix <- as.matrix(dtmTfIdfAll)
  
  dtmTfAll <- DocumentTermMatrix(corpus)
  dtmTfAllMatrix <- as.matrix(dtmTfAll)
  
  for (i in 1:20) {
    generate_keywords_chart_for_matrix(matrix = dtmTfIdfAllMatrix, document = corpus[i])
    generate_keywords_chart_for_matrix(matrix = dtmTfAllMatrix, document = corpus[i])
  }
}

generate_keywords_charts()


