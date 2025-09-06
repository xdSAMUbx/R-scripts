library(sf)

x <- c(1:3)
y <- c(4:6)
df <- data.frame(x, y)
pts <- st_as_sf(df, coords = c("x", "y"))

var <- as.formula("z~x+y")

rbfPred <- function(formula, eta, rho, rbf) {
  if (!is.numeric(eta)) {
    stop("El valor de eta no es un valor numérico")
  }

  if (!is.numeric(rho)) {
    stop("El valor de rho no es un valor numérico")
  }
}

rbfPred("z~x+y", 0, 0, "MQ")
