library(MASS)
library(Matrix)

setwd("G:\\Otros ordenadores\\Mi PC\\IGAC\\Codigos\\R-scripts\\PruebasAjusteGeodesico")

source("gen_basic_matrix.R")
source("gen_matrix_weight.R")
source("leveling_adjustment.R")

path = "G:\\Otros ordenadores\\Mi PC\\IGAC\\EsquemasPruebas\\csv\\EjercicioIGAC6.csv"

df <- read.csv(path, header = TRUE, sep=";")
dfref <- data.frame(
  nom  = c("A22-CW-6","B107-CW-6"),
  h = c(1346.6917,2037.5729)
)

test_weight_matrix <- gen_matrix_weight(df)
gen_matrix_test <- gen_design_matrix(df)
class(gen_matrix_test$design_matrix)

GMM_1 <- GMM(data=df, weight=TRUE)
A <- GMM_1$IM
X <- GMM_1$x
K <- GMM_1$K
P <- GMM_1$P
e <- GMM_1$v
y_predic <- GMM_1$L_Corregida
r <- qr(K)$rank
crossprod(e,P)%*%e
crossprod(X,P)%*%e
(crossprod(y_predic,P)%*%y_predic) + crossprod(e,P)%*%e
