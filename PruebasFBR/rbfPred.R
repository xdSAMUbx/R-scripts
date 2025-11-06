library(sf)
library(sp)
library(Rfast)
library(expint)

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

rbfPred <- function(data, newdata, eta, rho, rbf) {
  if (!is.numeric(eta)) stop("El valor de eta no es un valor numérico")
  if (!is.numeric(rho)) stop("El valor de rho no es un valor numérico")
  if (!(rbf %in% c("MQ", "IM", "ST", "TPS", "CRS", "GAU", "EXP"))) stop("La función no esta definida dentro de los parametros")
  stopifnot(inherits(newdata, "SpatialPixels"))
  stopifnot(is.numeric(eta), eta > 0)

  # Cambio a futuro
  coords <- as.matrix(data[, c("x", "y")])

  # Dependencia libreria Rfast
  phiIJ <- as.matrix(Dist(coords, method = "euclidean"))
  phiI0 <- as.matrix(dista(coords, coordinates(as(newdata, "SpatialPoints"))))
  n <- nrow(phiIJ)
  nData <- nrow(data)

  # Calculo funciones de base radial
  etafi <- eta * phiIJ
  EULER  <- 0.5772156649015329  # Constante de Euler
  func <-  switch(rbf,
    MQ = sqrt(phiIJ * phiIJ + eta * eta), # Multicuadrática
    IM = 1 / sqrt(phiIJ * phiIJ + eta * eta), # Inversa Multicuadrática
    TPS = ifelse(phiIJ == 0, 0, (etafi) * (etafi) * log(etafi)), # Spline de Capa Delgada
    ST = ifelse(phiIJ == 0, 0, log(etafi * 0.5) + besselK(etafi, 0) + EULER), # Spline con tensión
    # Error CRS Revisar
    CRS = ifelse(phiIJ == 0, 0, ifelse(phiIJ > 0 & eta > 0,
      log(etafi * 0.5) * log(etafi * 0.5) + expint_E1(etafi * 0.5) * expint_E1(etafi * 0.5) + EULER)), # Spline completamente Regularizado
    GAU = exp(-etafi * phiIJ), # Gaussiana
    EXP = exp(-etafi), # Exponencial
    stop("RBF no implementado", rbf)
  )

  diag(func) <- diag(func) + rho

  A <- matrix(0.0, n + 1L, n + 1L)
  A[1:n, 1:n] <- func
  A[1:n, n + 1L] <- 1.0
  A[n + 1L, 1:n] <- 1.0

  if (nrow(phiI0) != nData) phiI0 <- t(phiI0)
  B <- rbind(phiI0, matrix(1.0, 1L, ncol(phiI0)))

  QA <- qr(A, LAPACK = TRUE)
  coef_mat <- qr.coef(QA, B)

  pred <- as.numeric(crossprod(z, coef_mat[1:nData, , drop = FALSE]))
}

ptsSample$pred <- rbfPred(dfData, ptsSample, 0.2, 0.5, "EXP")
plot(ptsSample)
