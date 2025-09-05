library(sp)
library(sf)
library(dplyr)

setwd("D:/programacion/R-scripts/Pruebas") # Portatil
data <- "data/ariari.rda"
data2 <- "data/ariprec.rda"

ptsSample <- spsample(ariari, 10000, type = "regular")
gridded(ptsSample) <- TRUE
dfData <- ariprec %>% select("x", "y", "PRECI_TOT")
colnames(dfData)[3] <- "z"

d <- Dist(as.matrix(dfData[, c("x", "y")]), method = "euclidean")

m <- sqrt(d**2 + 0.2)
