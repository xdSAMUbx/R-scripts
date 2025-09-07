library(sf)
library(sp)
library(Rfast)
library(expint)
library(dplyr)

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

# Función para obtener los valores de la formula
getFormula <- function(formula, data, na.action = na.action) {
  stopifnot(inherits(formula, "character") || inherits(formula, "formula"))
  stopifnot(inherits(data, "data.frame"))

  f <- as.formula(formula)

  mf <- model.frame(f, data = data, na.action = na.action)
  if (ncol(mf) < 3) stop("Se esperaban por lo menos 3 valores")

  z <- mf[[1L]]
  X <- as.matrix(mf[, -1, drop = FALSE])

  if (!all(vapply(as.data.frame(X), is.numeric, logical(1L))))
    stop("Los predictores deben ser numéricos.")

  list(z = z, x = X)
}

# Función para obtener el tipo de RBF
pickRBF <- function(func, dMat, eta) {
  stopifnot(is.character(func))
  stopifnot(is.numeric(eta))
  stopifnot(inherits(dMat, "matrix"))
  if (eta <= 0) stop("Eta tiene que ser mayor a 0")

  etaDist <- dMat * eta
  EULER <- 0.5772156649015329
  switch(tolower(func),
    # Multicuadrática
    m = sqrt(dMat * dMat + eta * eta),
    # Inversa multicuadrática
    im = (sqrt(dMat * dMat + eta * eta))^(-1), # revisar
    # Exponencial
    exp = exp(-(etaDist)), # Revisar
    # Gaussiana
    gau = exp(-etaDist * dMat), # revisar
    # Spline de capa delgada
    tps = ifelse(dMat == 0, 0, etaDist * etaDist * log(etaDist)),
    # Spline con tensión
    st = ifelse(dMat == 0, 0, log(etaDist * 0.5) + besselK(etaDist, 0) + EULER),
    # Spline completamente regularizado
    crs = ifelse(dMat == 0, 0, (2 * log(etaDist * 0.5)) + expint_E1(etaDist * etaDist * 0.25) + EULER) # revisar
  )
}

rbf <- function(formula, data, newData, eta, rho, func) {
  stopifnot(is.numeric(rho), is.data.frame(data), inherits(newData, "SpatialPixels"))

  formula <- getFormula(formula, data)
  z <- formula$z
  coords <- formula$x

  dMat <- as.matrix(Dist(coords, method = "euclidean"))
  d0Mat <- as.matrix(dista(coords, coordinates(as(newData, "SpatialPoints"))))
  n <- nrow(dMat)
  nData <- nrow(data)
  if (nrow(d0Mat) != nData) d0Mat <- t(d0Mat)

  phi <- pickRBF(func, dMat, eta)
  phi0 <- pickRBF(func, d0Mat, eta)
  diag(phi) <- diag(phi) + rho

  A <- matrix(0.0, n + 1L, n + 1L)
  A[1:n, 1:n] <- phi
  A[1:n, n + 1L] <- 1.0
  A[n + 1L, 1:n] <- 1.0

  B <- rbind(phi0, matrix(1.0, 1L, ncol(phi0)))
  QA <- qr(A, LAPACK = T)
  coefMat <- qr.coef(QA, B)

  pred <- as.numeric(crossprod(z, coefMat[1:n, , drop = F]))
}

ptsSample$pred <- rbf("z~x+y", dfData, ptsSample, 0.2, 0.2, "st")
plot(ptsSample)
