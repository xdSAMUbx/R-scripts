library(MASS)

source("GenA.R")
source("GMMAdj.R")
source("Condition.R")

path <- "G:\\Otros ordenadores\\Mi PC\\IGAC\\EsquemasPruebas\\EjercicioLauraSanchez1.csv"

df <- read.csv(path, header = TRUE)
dfref <- data.frame(
  nom = c("A", "B"),
  h = c(161.4390, 171.2150)
)

P1 <- GMM(data = df, ref = dfref, Weigth = T)
P2 <- corr(data = df, Weigth = T)

A <- P1$A
K <- P1$K
N <- P1$N
P <- P1$P
v <- P1$v
PI <- solve(P)
NI <- ginv(N)
sigma0 <- crossprod(v, P) %*% v / (nrow(A) - qr(A)$rank + qr(K)$rank)
sqrt(diag(P1$Nc))
