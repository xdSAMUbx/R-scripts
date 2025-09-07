library(sp)
library(sf)
library(dplyr)
library(Rfast)

setwd("D:/programacion/R-scripts/Pruebas") # Portatil
setwd("D:/Universidad/8 Semestre/Econometria/R-scripts/Pruebas") # PC
data <- "data/ariari.rda"
data2 <- "data/ariprec.rda"
load(data)
load(data2)
ptsSample <- spsample(ariari, 50000, type = "regular")
gridded(ptsSample) <- TRUE
dfData <- ariprec %>% select("x", "y", "PRECI_TOT")
colnames(dfData)[3] <- "z"

# Matriz de distancias sin considerar vecinos
phiIJ <- as.matrix(Dist(as.matrix(dfData[, c("x", "y")]), method = "euclidean"))
phiI0 <- as.matrix(dista(as.matrix(dfData[, c("x", "y")]), coordinates(as(ptsSample, "SpatialPoints"))))
eta <- 0.2
# Calula la multicuadrática
MQ <- sqrt(phiIJ^2 + eta^2)

# Calculando rho
rho <- 0.5
diag(MQ) <- diag(MQ) + rho

# Realizando el calculo vectorial
f <- matrix(rep(1, nrow(MQ)))
A <- cbind(rbind(MQ, t(f)), rbind(f, 0))
z <- as.matrix(dfData[, c("z")])

QA <- qr(A, LAPACK = TRUE)
if (nrow(phiI0) != nrow(dfData)) phiI0 <- t(phiI0)
nData <- nrow(dfData)
B <- rbind(phiI0, rep(1, ncol(phiI0)))   # (nData+1) x nPhi

time <- system.time({
  # Resolver todos los sistemas de una vez usando la factorización QR ya hecha
  coef_mat <- qr.coef(QA, B)               # (nData+1) x nPhi

  # Calcular pred con un solo producto matricial
  pred <- as.numeric(crossprod(z, coef_mat[1:nData, , drop = FALSE]))
})
ptsSample$pred <- pred
plot(ptsSample)
print(time)
