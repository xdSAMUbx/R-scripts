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
ptsSample <- spsample(ariari, 100, type = "regular")
gridded(ptsSample) <- TRUE
dfData <- ariprec %>% select("x", "y", "PRECI_TOT")
colnames(dfData)[3] <- "z"

# Matriz de distancias sin considerar vecinos
phiIJ <- as.matrix(Dist(as.matrix(dfData[, c("x", "y")]), method = "euclidean"))
phiI0 <- as.matrix(dista(as.matrix(dfData[, c("x", "y")]), coordinates(as(ptsSample, "SpatialPoints"))))
eta <- 50
rho <- 0.1
# Calula la multicuadrática
MQ <- sqrt(phiIJ^2 + eta^2)
im <- (sqrt(phiIJ^2 + eta^2))^(-1)
im0 <- (sqrt(phiI0^2 + eta^2))^(-1)

# Calculando rho
diag(im) <- diag(im) + rho
# Realizando el calculo vectorial
f <- matrix(1.0, nrow(im))
A <- matrix(0.0, nrow(im) + 1L, ncol(im) + 1L)
A[1:nrow(im), 1:ncol(im)] <- im
A[1:nrow(im), ncol(im) + 1L] <- f
A[nrow(im) + 1L, 1:ncol(im)] <- t(f)
A == t(A)
kappa(A)
z <- as.matrix(dfData[, c("z")])
b <- c(z, 0.0)
QA <- qr(A, LAPACK = TRUE)
pond <- qr.coef(QA, b)
pred <- c(rep(0.0, ncol(im0)))
for (i in 1:ncol(im0)) {
  pred <- crossprod(pond[1:nrow(im0)], im0[1:nrow(im0), i]) + pond[nrow(im0) + 1]
  print(pred)
}

sv  <- svd(A)
sv  <- svd(A)
tol <- max(dim(A)) * max(sv$d) * .Machine$double.eps   # tolerancia estándar
dInv <- ifelse(sv$d > tol, 1 / sv$d, 0)

# x = A^+ b = V diag(dInv) U^T b
pond <- sv$v %*% (dInv * (t(sv$u) %*% b))
omega <- pond[1:nrow(im)]
v     <- pond[nrow(im) + 1L]

# pred = t(omega) %*% im0 + v   (devuelve un vector de longitud m)
pred_vec <- drop(crossprod(omega, im0)) + as.numeric(v)

ptsSample$pred <- pred_vec

ptsSample$pred <- pred
plot(ptsSample)
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
print(time)
