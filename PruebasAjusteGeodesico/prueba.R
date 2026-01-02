library(MASS)
library(Matrix)

setwd("C:/Users/samue/Documents/programacion/R-scripts/PruebasAjusteGeodesico")
source("level_adjust.R")

path <- "C:/Users/samue/Documents/programacion/AjusteNivelacion/data/EjercicioKaiBorre1.csv"
df <- read.csv(path)
level_adjust(df,T,T,"LSS")
level_adjust(df,T,T,"RLSS")
