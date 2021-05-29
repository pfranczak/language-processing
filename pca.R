setwd("./")


#wczytanie i wynikanie pliku frequency_matrix.R
sourceFile <- paste(
  ".",
  "matrixes.R",
  sep = "/"
)
source(sourceFile)
#eval(parse(sourceFile, encoding = "UTF-8"))

#analiza głównych składowych
pca <- prcomp(dtmTfBounds)

#przygotowanie danych do wykresu
legend <- paste(paste("d", 1:dtmTfBounds$nrow,sep = ""), rownames(dtmTfBounds),sep = " => ")
x <- pca$x[,1]
y <- pca$x[,2]
options(scipen = 5)


#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "pca.png",
  sep = "/"
)
png(filename = plotFile)
dev.new(width = 550, height = 330, unit = "px")
#wykres dokumentów w przestrzeni dwuwymiarowej
plot(
  x,
  y, 
  col = "purple",
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
 # xlim = c(-0.005,0.005),
 # ylim = c(0.03,0.055),
  pch = 18
)
text(
  x,
  y,
  paste("d", 1:dtmTfBounds$nrow,sep = ""),
  col = "purple",
  pos = 4,
  cex = 0.8
)
legend(
  "bottom",
  #"right",
  legend,
  cex = 0.6,
  text.col = "purple"
)

#zamknięcie pliku .png
#dev.off()

