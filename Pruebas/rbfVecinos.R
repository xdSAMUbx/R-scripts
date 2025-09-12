# Prueba RBF Vecinos
library(sf)
library(sp)
library(Rfast)
library(expint)
library(FNN)
library(dplyr)

setwd("D:/programacion/R-scripts/Pruebas") # Portatil
setwd("D:/Universidad/8 Semestre/Econometria/R-scripts/Pruebas") # PC
setwd("/home/xdsamubx/Documents/R-scripts/Pruebas")
data <- "data/ariari.rda"
data2 <- "data/ariprec.rda"
load(data)
load(data2)
ptsSample <- spsample(ariari, 1000, type = "regular")
gridded(ptsSample) <- TRUE
dfData <- ariprec %>% select("x", "y", "PRECI_TOT")
colnames(dfData)[3] <- "z"
z <- c(dfData$z)

s <- as.matrix(dfData[, 1:2, drop = FALSE])
s0 <- coordinates(as(ptsSample, "SpatialPoints"))
n <- nrow(dfData)
m <- nrow(s0)

knn <- get.knnx(data = s, query = s0, k = 10) # La nn.dist es la matriz de distancia
idxMat <- knn$nn.index
d0Mat <- knn$nn.dist # Matriz de distancia a cada punto con respecto a sus vecinos más cercanos

llave <- apply(idxMat, 1L, paste, collapse = ",")
grupos <- split(seq_len(m), llave)
rho <- 1e-8
eta <- 1e-7
func <- "m"
for (g in grupos) {
  nIdx <- idxMat[g[1L], ]
  sg <- s[nIdx, , drop = FALSE]
  zg <- z[nIdx]
  sGMat <- as.matrix(Dist(sg, method = "euclidean"))
  K <- pickRBF(func, sGMat, eta)
  diag(K) <- diag(K) + rho
  s0GMat <- t(d0Mat[g, , drop = FALSE])
  K0 <- pickRBF(func, s0GMat, eta)
}

rbf <- function(formula, data, newData, eta, rho, n.neigh, func) {
  stopifnot(is.numeric(rho), is.data.frame(data), inherits(newData, "SpatialPixels"))

  # Obtiene la formula z~x+y
  formula <- getFormula(formula, data)
  z <- formula$z
  coords <- formula$x

  # Dependencia 1 Rfast (Calculo de distancias)
  dMat <- as.matrix(Dist(coords, method = "euclidean"))
  d0Mat <- as.matrix(dista(coords, coordinates(as(newData, "SpatialPoints"))))
  n <- nrow(dMat)
  nData <- nrow(data)
  if (nrow(d0Mat) != nData) d0Mat <- t(d0Mat)

  K <- pickRBF(func, dMat, eta) # Función kernel de las distancias de los datos
  K0 <- pickRBF(func, d0Mat, eta) # Función kernel de los nuevos puntos
  # ro mayor a 0
  diag(K) <- diag(K) + rho

  ch <- tryCatch(chol(K), error = function(e) NULL)
  if (!is.null(ch)) {
    res <- cholSolv(K, z)
    omega <- res$omega
    v <- res$v
  } else {
    res <- qrSolv(K, z)
    omega <- res$omega
    v <- res$v
  }
  pred <- drop(crossprod(omega, K0) + as.numeric(v))
}
