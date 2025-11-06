# Prueba 3 de RBF
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

# Función para obtener los valores de la formula
getFormula <- function(formula, data, na.action = na.action) {
  stopifnot(inherits(formula, "character") || inherits(formula, "formula"))
  stopifnot(inherits(data, "data.frame"))

  f <- as.formula(formula)

  mf <- model.frame(f, data = data, na.action = na.action)
  if (ncol(mf) < 3) stop("Se esperaban por lo menos 3 valores")

  z <- mf[[1L]]
  X <- as.matrix(mf[, -1, drop = FALSE])

  if (!all(vapply(as.data.frame(X), is.numeric, logical(1L)))) {
    stop("Los predictores deben ser numéricos.")
  }

  list(z = z, x = X)
}

pickRBF <- function(func, dMat, eta) {
  stopifnot(is.character(func))
  stopifnot(is.numeric(eta), eta > 0)
  stopifnot(is.matrix(dMat), is.numeric(dMat))
  if (any(dMat < 0)) stop("La matriz de distancias debe contener distancias (>= 0).")

  etaDist <- pmax(dMat * eta, .Machine$double.eps)
  EULER <- 0.5772156649015329
  switch(tolower(func),
    # Multicuadrática
    m = sqrt(dMat^2 + eta^2),
    # Inversa multicuadrática
    im = (sqrt(dMat^2 + eta^2))^(-1), # revisar --> No funciona con valores cercanos a cero
    # Exponencial
    exp = exp(-(etaDist)), # Revisar
    # Gaussiana
    gau = exp(-eta * (dMat^2)), # revisar --> No funciona con valores cercanos a 0
    # Spline de capa delgada
    tps = ifelse(dMat == 0, 0, etaDist^2 * log(etaDist)),
    # Spline con tensión
    st = ifelse(dMat == 0, 0, log(etaDist * 0.5) + besselK(etaDist, 0) + EULER),
    # Spline completamente regularizado
    crs = ifelse(dMat == 0, 0, log((etaDist / 2)^2) + (ifelse(expint_E1((etaDist / 2)^2) == "NaN", 0, expint_E1((etaDist / 2)^2) + EULER))) # revisar --> funciona con valores cercanos a 0
  )
}

# Se ingresa la función de kernel
cholSolv <- function(K, z) {
  R <- chol(K)
  f <- matrix(1.0, nrow(K), 1L)
  intSolv <- function(R, b) backsolve(R, forwardsolve(t(R), b))

  Kif <- intSolv(R, f)
  Kiz <- intSolv(R, z)

  v <- drop(crossprod(f, Kiz) / crossprod(f, Kif))
  omega <- Kiz - Kif * v
  list(omega = omega, v = v)
}

qrSolv <- function(K, z) {
  n <- nrow(K)
  f <- matrix(1.0, nrow(K), ncol = 1L)
  A <- matrix(0.0, n + 1L, n + 1L)
  A[1:n, 1:n] <- K
  A[1:n, n + 1L] <- f
  A[n + 1L, 1:n] <- t(f)
  b <- c(z, 0.0)
  qa <- qr(A, LAPACK = T)
  coef <- qr.coef(qa, b)
  omega <- coef[seq_len(n)]
  v <- coef[n + 1L]
  return(list(omega = omega, v = v))
}

rbf <- function(formula, data, newData, eta, rho, func) {
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
    cat("Solucionando con Cholesky")
    res <- cholSolv(K, z)
    omega <- res$omega
    v <- res$v
  } else {
    cat("Solucionando con QR")
    res <- qrSolv(K, z)
    omega <- res$omega
    v <- res$v
  }
  pred <- drop(crossprod(omega, K0) + as.numeric(v))
}

time <- system.time({
  pred <- rbf("z~x+y", dfData, ptsSample, 1e-7, 1e-8, "")
})
print(time)
ptsSample$pred <- pred
plot(ptsSample)
