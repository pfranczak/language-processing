library(tm)

setwd("./")

#definicja lokalizacji katalogów funkcjonalnych
inputDir <- "./processed"
outputDir <- "./results"

#utworzenie wybranych katalogów funkcjonalnych
dir.create(outputDir, showWarnings = F)
dir.create(workspacesDir, showWarnings = F)

#utworzenie korpusu dokumentów
corpusDir <- paste(
  inputDir
)

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

cutExtension <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$",
    "",
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cutExtension)

#tworzenie macierzy częstości
tdmTfAll <- TermDocumentMatrix(corpus)
tdmBinAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdmTfIdfAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdmTfIdfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfAll <- DocumentTermMatrix(corpus)
dtmBinAll <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
dtmTfIdfAll <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
dtmTfBounds <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTf,
    bounds = list(
      global = c(0,20)
    )
  )
)
dtmTfIdfBounds <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(1,16)
    )
  )
)

#konwersja macierzy rzadkich do macierzy klasycznych
tdmTfAllMatrix <- as.matrix(tdmTfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfIdfAllMatrix <- as.matrix(tdmTfIdfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfIdfBoundsMatrix <- as.matrix(tdmTfIdfBounds)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
dtmBinAllMatrix <- as.matrix(dtmBinAll)
dtmTfIdfAllMatrix <- as.matrix(dtmTfIdfAll)
dtmTfBoundsMatrix <- as.matrix(dtmTfBounds)
dtmTfIdfBoundsMatrix <- as.matrix(dtmTfIdfBounds)

#eksport macierzy częstości do pliku
matrixFile <- paste(
  outputDir,
  "tdmTfAll.csv",
  sep = "/"
)
write.table(dtmTfIdfBoundsMatrix, matrixFile, sep = ";", dec = ",", col.names = F)

