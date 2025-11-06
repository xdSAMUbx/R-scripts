source("genA.R")

# Función que permite hacer el ajuste a traves del modelo de gauss - markov
GMM <- function(data, ref=NULL, Weigth=FALSE){
  
  # 1) Verifica que la información este en data.frame
  stopifnot(inherits(data,"data.frame"))
  
  # 2) Genera la matriz de pesos según si esta en km, millas o no hay
  # Esto para obtenerla en terminos de metros, pies o la matriz identidad
  if (!(isTRUE(Weigth))){
    P <- diag(nrow(data))
  } else {
    d <- as.numeric(data[,4])
    P <- diag(1/d)
  }
  
  # 3) Genera la matriz A y la condiciona
  A <- as.matrix(genA(data, ref = ref )$IM)
  L <- matrix(as.numeric(data[,3]))
  ATP <- crossprod(A,P)
  N <- ATP%*%A
  c <- ATP%*%L
  
  # Obtiene las filas y columnas de N
  rowN <- nrow(N)
  colN <- ncol(N)
  
  if(!(is.null(ref))){
    # dimensiones base
    n_par <- ncol(A)         # vértices desconocidos
    n_ref <- nrow(ref)       # vértices con cota conocida
    
    # Matriz K (n_par x n_ref), inicialmente ceros
    K <- matrix(0,
                nrow = n_par,
                ncol = n_ref,
                dimnames = list(colnames(A), ref[,"nom"]))
    
    # Llenar K con 1 cuando el vértice es referencia
    rn <- rownames(K)        # nombres de vértices (incógnitas)
    cn <- colnames(K)        # nombres de referencias declaradas
    comunes <- intersect(rn, cn)
    K[cbind(match(comunes, rn), match(comunes, cn))] <- 1L
    
    # Armar matriz aumentada Nc
    Nc <- matrix(0, nrow = n_par + n_ref, ncol = n_par + n_ref)
    Nc[1:n_par, 1:n_par] <- N
    Nc[1:n_par, (n_par+1):(n_par+n_ref)] <- K
    Nc[(n_par+1):(n_par+n_ref), 1:n_par] <- t(K)
    
    # RHS aumentado Cc
    HCon <- matrix(as.numeric(ref[,"h"]), ncol = 1)
    Cc <- matrix(0, nrow = n_par + n_ref, ncol = 1)
    Cc[1:n_par, 1] <- c
    Cc[(n_par+1):(n_par+n_ref), 1] <- HCon
  } else {
    # Obtiene las filas y columns de N compuesto
    rowNc = rowN+1L
    colNc = colN+1L
    
    # Genera la matriz compuesta N y C
    Nc <- matrix(0,nrow = rowNc,ncol = colNc)
    Cc <- matrix(0,nrow = rowNc,ncol = 1L)
    
    Nc[1:rowN,1:colN] <- N
    Nc[1:rowN,colNc] <- 1L
    Nc[rowNc,1:colN] <- 1L
    Cc[1:rowN,1L] <- c
    Cc[rowNc,1L] <- 0 
  }
  
  # 5) Ajusta las observaciones
  x <- solve(Nc,Cc)
  Ax <- A%*%x[1:rowN]
  
  # 6) Calcula las medidas ajustadas, y el vector de residuos (v)
  e <- L - Ax
  L_Corregida <- L - e
  
  if(!(is.null(ref))){
    return(list(A = A, P = P, L = L, x = x, v = round(e,4), K = K, L_Corregida = round(L_Corregida,4)))
  } else {
    return(list(A = A, P = P, L = L, x = x, v = round(e,4), L_Corregida = round(L_Corregida,4)))
  }
}