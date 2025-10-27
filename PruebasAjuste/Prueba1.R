library(MASS)

source("GenA.R")
source("GMMAdj.R")
source("Condition.R")

path = "G:\\Otros ordenadores\\Mi PC\\IGAC\\EsquemasPruebas\\EjercicioMikhail1.csv"

df <- read.csv(path,header = TRUE)
dfref <- data.frame(
  nom  = c("BMX","BMY"),
  h = c(100.0,107.5)
)

P1 <- GMM(data=df, Weigth = T)
P2 <- condition(data=df, Weigth = T)

