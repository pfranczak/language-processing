#Kod odpowiedzialny za stworzenie korpusu dokumentów
library(tm)
library(hunspell)
library(topicmodels)
library(wordcloud)
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)

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

dtmTfIdf2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

dtmTfIdf2_16Matrix <- as.matrix(dtmTfIdf2_16)

dtmBinAll4_8Matrix <- as.matrix(dtmBinAll4_8)

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
nTerms <- dtmTfAll$ncol
nTopics <- 5
lda <- LDA(
  dtmTfAll,
  k = nTopics,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100,
    iter = 3000
  )
)
perplexity <- perplexity(lda, dtmTfAll)
results <- posterior(lda)

par(mai = c(1,2,1,1))
#prezentacja tematów
topic1 <- tail(sort(results$terms[1,]),20)
barplot(
  topic1,
  horiz = T,
  las = 1,
  main = "Temat 1",
  xlab = "Prawdopodobieństwo",
  col = "orange"
)
topic2 <- tail(sort(results$terms[2,]),20)
barplot(
  topic2,
  horiz = T,
  las = 1,
  main = "Temat 2",
  xlab = "Prawdopodobieństwo",
  col = "violet"
)
topic3 <- tail(sort(results$terms[3,]),20)
barplot(
  topic3,
  horiz = T,
  las = 1,
  main = "Temat 3",
  xlab = "Prawdopodobieństwo",
  col = "turquoise"
)
topic4 <- tail(sort(results$terms[4,]),20)
barplot(
  topic4,
  horiz = T,
  las = 1,
  main = "Temat 4",
  xlab = "Prawdopodobieństwo",
  col = "darkseagreen"
)
topic5 <- tail(sort(results$terms[5,]),20)
barplot(
  topic5,
  horiz = T,
  las = 1,
  main = "Temat 5",
  xlab = "Prawdopodobieństwo",
  col = "lightskyblue"
)

#prezentacja dokumentów
document1 <- results$topics[1,]
barplot(
  rev(document1),
  horiz = T,
  las = 1,
  main = rownames(results$topics)[1],
  xlab = "Prawdopodobieństwo",
  col = "darkseagreen"
)
document4 <- results$topics[4,]
barplot(
  rev(document4),
  horiz = T,
  las = 1,
  main = rownames(results$topics)[4],
  xlab = "Prawdopodobieństwo",
  col = "orange"
)
document11 <- results$topics[11,]
barplot(
  rev(document11),
  horiz = T,
  las = 1,
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobieństwo",
  col = "turquoise"
)
document17 <- results$topics[17,]
barplot(
  rev(document17),
  horiz = T,
  las = 1,
  main = rownames(results$topics)[17],
  xlab = "Prawdopodobieństwo",
  col = "violet"
)

# Identyfikacja słów i fraz kluczowych

for (j in 1:20) {
  keywordsTf1 <- tail(sort(dtmTfIdfAllMatrix[1,]))
  rev(keywordsTf1)
  
  #waga TfIdf jako miara istotności słów
  keywordsTfIdf1 <- tail(sort(dtmTfIdfAllMatrix[1,]))
  rev(keywordsTfIdf1)
  
  #prawdopodobieństwo w LDA jako miara istorności słów
  termsImportance1 <- c(results$topics[1,]%*%results$terms)
  names(termsImportance1) <- colnames(results$terms)
  keywordsLda1 <- tail(sort(termsImportance1))
  rev(keywordsLda1)
  
  #chmura tagów
  par(mai = c(0,0,0,0))
  wordcloud(corpus[j], max.words = 150, color = brewer.pal(8,"PuOr"))
}

for (j in 1:20) {
  keywordsTf1 <- tail(sort(dtmTfAllMatrix[1,]))
  rev(keywordsTf1)
  
  #waga Tf jako miara istotności słów
  keywordsTfIdf1 <- tail(sort(dtmTfAllMatrix[1,]))
  rev(keywordsTfIdf1)
  
  #prawdopodobieństwo w LDA jako miara istorności słów
  termsImportance1 <- c(results$topics[1,]%*%results$terms)
  names(termsImportance1) <- colnames(results$terms)
  keywordsLda1 <- tail(sort(termsImportance1))
  rev(keywordsLda1)
  
  #chmura tagów
  par(mai = c(0,0,0,0))
  wordcloud(corpus[j], max.words = 150, color = brewer.pal(8,"PuOr"))
}















# ANALIZA SKUPIEŃ

# ilość dokumentów


clusterAnalysis <- function(nClusters) {
  nDocs <- 20
  distMatrix <- dist(dtmBinAll4_8Matrix, method = "cosine")
  hclust <- hclust(distMatrix, method = "ward.D2")
  par(mai = c(1,2,1,1))
  plot(hclust)
  barplot(hclust$height, names.arg = 19:1)
  dendrogram <- as.dendrogram(hclust)
  coloredDendrogram <- color_branches(dendrogram, k=nClusters)
  par(mai = c(1,1,1,4))
  plot(coloredDendrogram, horiz = T)
  clusters <- cutree(hclust, k <- nClusters)
  clustersMatrix <- matrix(0,nDocs,nClusters)
  rownames(clustersMatrix) <- names(nClusters)
  for (i in 1:nDocs) {
    clustersMatrix[i, clusters[i]] <- 1
  }
  par(mai = c(1,1,1,1))
  corrplot(clustersMatrix, tl.col = "black", cl.pos = "n")
}

clusterAnalysis(nClusters = 5)
clusterAnalysis(nClusters = 4)
clusterAnalysis(nClusters = 7)


