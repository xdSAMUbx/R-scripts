library(MASS)

source("GenA.R")
source("GMMAdj.R")
source("Condition.R")

path = "G:\\Otros ordenadores\\Mi PC\\IGAC\\EsquemasPruebas\\EjercicioLauraSanchez1.csv"

df <- read.csv(path,header = TRUE)
dfref <- data.frame(
  nom  = c("A","B"),
  h = c(161.4390,171.2150)
)

P1 <- GMM(data=df, ref = dfref, Weigth = T)
P2 <- corr(data=df, Weigth = T)

v <- P1$v
sum((v[-1]-v[-length(v)])^2)/crossprod(v,v)
acf(v,NULL,"correlation")
