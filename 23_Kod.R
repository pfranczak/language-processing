#Kod odpowiedzialny za stworzenie korpusu dokumentów
library(tm)
library(hunspell)
library(topicmodels)
library(wordcloud)
library(randomcoloR)
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

tdmTfIdfBounds2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

tdmTfIdfBounds1_2 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(1,2)
    )
  )
)

tdmTfIdfBounds19_20 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(1,2)
    )
  )
)


dtmTfIdf2_16Matrix <- as.matrix(dtmTfIdf2_16)

dtmBinAll4_8Matrix <- as.matrix(dtmBinAll4_8)

tdmTfIdfBounds2_16Matrix <- as.matrix(tdmTfIdfBounds2_16)

tdmTfIdfBounds1_2Matrix <- as.matrix(tdmTfIdfBounds1_2)

tdmTfIdfBounds19_20Matrix <- as.matrix(tdmTfIdfBounds19_20)



# Analiza PCA poszczególnych macierzy

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

# Wykresy generują się jeden po drugim
generate_pca_chart_for_matrix(matrix = dtmBinAll4_8)
generate_pca_chart_for_matrix(matrix = dtmTfAll)
generate_pca_chart_for_matrix(matrix = dtmTfIdf19_20)



#Dekompozycja według wartości osobliwych

generate_decomposition_plots_for_matrix <- function(matrix) {
  lsa <- lsa(matrix)
  
  #przygotowanie danych do wykresu
  coordDocs <- lsa$dk%*%diag(lsa$sk)
  coordTerms <- lsa$tk%*%diag(lsa$sk)
  termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
  importantTerms <- names(tail(sort(termsImportance),30))
  coordImportantTerms <- coordTerms[importantTerms,]
  legend <- paste(paste("d", 1:length(rownames(coordDocs)),sep = ""), rownames(coordDocs),sep = " => ")
  x1 <- coordDocs[,1]
  y1 <- coordDocs[,2]
  x2 <- coordImportantTerms[,1]
  y2 <- coordImportantTerms[,2]
  options(scipen = 5)
  
  #eksport wykresu do pliku .png
  plotFile <- paste(
    outputDir,
    "lsa.png",
    sep = "/"
  )
  
  png(filename = plotFile)
  dev.new(width =1000, height = 1000, unit = "px")
  
  #wykres dokumentów i słów w przestrzeni dwuwymiarowej
  plot(
    x1,
    y1, 
    col = "purple",
    main = "Analiza ukrytych wymiarów semantycznych",
    xlab = "SD1",
    ylab = "SD2",
    xlim = c(-0.1,0),
    ylim = c(-0.1,0.1),
    pch = 18
  )
  text(
    x1,
    y1,
    paste("d", 1:length(rownames(coordDocs)),sep = ""),
    col = "purple",
    pos = 4,
    cex = 0.8
  )
  points(
    x2,
    y2, 
    col = "purple4",
    pch = 16
  )
  text(
    x2,
    y2,
    rownames(coordImportantTerms),
    col = "purple4",
    pos = 2,
    cex = 0.8
  )
  legend(
    "bottomleft",
    #"right",
    legend,
    cex = 0.6,
    text.col = "purple"
  )
}

generate_decomposition_plots_for_matrix(matrix = tdmTfIdfBounds2_16Matrix)
generate_decomposition_plots_for_matrix(matrix = tdmTfIdfBounds1_2Matrix)
generate_decomposition_plots_for_matrix(matrix = tdmTfIdfBounds19_20Matrix)



# Analiza skupień

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



#Analiza ukrytej alokacji Dirichlet'a

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




