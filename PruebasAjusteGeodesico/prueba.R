library(MASS)

setwd("C:\\Users\\samuel.calderon\\Documents\\CodigoAjuste\\R")

setwd("G:\\Otros ordenadores\\Mi PC\\Universidad\\8 Semestre\\Econometria\\R-scripts\\PruebasAjusteGeodesico")

source("genA.R")
source("GMM.R")
source("CORR.R")

path = "G:\\Otros ordenadores\\Mi PC\\IGAC\\EsquemasPruebas\\EjercicioGomez1.csv"

df <- read.csv(path,header = TRUE)
dfref <- data.frame(
  nom  = c("A"),
  h = c(2600)
)

GMM(data=df,Weigth = T)
corr(data=df, Weigth = T)
 v <- P1$v
P <- P1$P
A <- P1$A
K <- P1$K
m <- qr(t(A))$rank
l <- qr(K)$rank
n <- nrow(A)
r <- nrow(A) - qr(t(A))$rank + qr(K)$rank
sigma0pred <- crossprod(v,P)%*%v/r
