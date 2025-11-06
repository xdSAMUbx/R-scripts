library(sp)
library(sf)
library(dplyr)
library(Rfast)

setwd("D:/programacion/R-scripts/Pruebas") # Portatil
setwd("D:/Universidad/8 Semestre/Econometria/scripts/Pruebas") # PC
data <- "data/ariari.rda"
data2 <- "data/ariprec.rda"
load(data); load(data2)
ptsSample <- spsample(ariari, 10000, type = "regular")
gridded(ptsSample) <- TRUE
dfData <- ariprec %>% select("x", "y", "PRECI_TOT")
colnames(dfData)[3] <- "z"

d <- Dist(as.matrix(dfData[, c("x", "y")]), method = "euclidean")

m <- sqrt(d^2 + 0.2^2)
f <- matrix(rep(1,nrow(m)))
b <- rbind(as.matrix(dfData[,c("z")]),0)
A <- cbind(rbind(m,t(f)),rbind(f,0)) 
x <- solve(A,b)
s0 <- c(1145199.3, 778145.5)
dp <- dista(as.matrix(dfData[,c("x", "y")]),matrix(s0, nrow = 1))
phii0 <- rbind(dp,1)
x <- solve(A,phii0)
preds0 <- t(head(x,-1))%*%head(b,-1)
