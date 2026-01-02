# Función que permite hacer el ajuste a traves del modelo de gauss - markov
level_adjust <- function(data, ref = TRUE, weight = FALSE,
                            solution=c("LSS","RLSS")){
  
  # Carga los paquetes para realizar la solución
  source("gen_basic_matrix.R")
  source("rlss_sol.R")
  source("lss_sol.R")
  
  # Realiza verificaciones antes de iniciar el código
  stopifnot(inherits(data,"data.frame"))
  
  if (solution == "RLSS"){
    matrices <- gen_basic_matrices(data,rlss = TRUE, ref = ref, weight = weight)
    rlss_sol(A = matrices$A,W = matrices$W, K = matrices$K,
             K0 = matrices$K0,L = matrices$L)
  } else if (solution == "LSS") {
    matrices <- gen_basic_matrices(data,rlss = FALSE, ref = ref, weight = weight)
    lss_sol(A = matrices$A,W = matrices$W,L = matrices$L)
  }
}