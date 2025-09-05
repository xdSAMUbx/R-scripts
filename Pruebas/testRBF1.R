library(sp)
library(sf)
library(dplyr)
library(Rfast)

setwd("D:/programacion/R-scripts/Pruebas") # Portatil
setwd("D:/Universidad/8 Semestre/Econometria/scripts/Pruebas") # PC
data <- "data/ariari.rda"
data2 <- "data/ariprec.rda"
load(data); load(data2)
ptsSample <- spsample(ariari, 50000, type = "regular")
gridded(ptsSample) <- TRUE
dfData <- ariprec %>% select("x", "y", "PRECI_TOT")
colnames(dfData)[3] <- "z"

# Matriz de distancias sin considerar vecinos
phiIJ <- as.matrix(Dist(as.matrix(dfData[,c("x", "y")]), method="euclidean"))
phiI0 <- as.matrix(dista(as.matrix(dfData[,c("x", "y")]),coordinates(as(ptsSample,"SpatialPoints"))))
eta <- 0.2
# Calula la multicuadrática
MQ <- sqrt(phiIJ^2 + eta^2)

# Calculando rho
rho <- 0.5
MQ <- MQ + diag(rep(rho,nrow(MQ)))

# Realizando el calculo vectorial
f <- matrix(rep(1,nrow(MQ)))
A <- cbind(rbind(MQ,t(f)), rbind(f,0))
z <- as.matrix(dfData[,c("z")])

QA <- qr(A, LAPACK = TRUE)
if (nrow(phiI0) != nrow(dfData)) phiI0 <- t(phiI0)
rhs <- numeric(nrow(dfData) + 1L)  
rhs[nrow(dfData) + 1L] <- 1 

time <-system.time({ pred <- vapply(
  seq_len(ncol(phiI0)),
  function(j) {
    rhs <- rbind(phiI0[, j, drop = FALSE], 1)
    v <- qr.coef(QA,rhs)
    sum(z * v[seq_len(nrow(dfData)), 1])
  },
  numeric(1L)
)
})

ptsSample$pred <- pred
plot(ptsSample)
print(time)
