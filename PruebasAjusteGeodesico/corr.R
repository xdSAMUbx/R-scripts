source("genA.R")

corr<-function(data, ref = NULL, Weigth = FALSE){
  # Para la función si data no es de tipo dataframe
  stopifnot(inherits(data,"data.frame"))
  
  # Si Weigth es TRUE calcula distancias ponderadas si no, identidad
  if (!(isTRUE(Weigth))){
    P <- diag(nrow(data))
  } else {
    d <- as.numeric(data[,4])
    P <- diag(1/d)
  }
  
  # Obtiene la matriz A y la B, para calcular B utiliza BA = 0
  A <- genA(data, ref = ref)$IM
  B <- t(Null(A))
  
  L <- matrix(as.numeric(data[,3]))
  PI <- solve(P)
  W <- B%*%L
  M <- B%*%PI%*%t(B)
  V <- PI%*%crossprod(B,solve(M))%*%B%*%L
  L_Corregido = L - V
  
  list(B = B, P = P, L = L, M = M, W = W,V = V, L_Corregido = L_Corregido)
}