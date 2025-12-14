restricted_gmm_solution <- function(data, ref = NULL, weights=FALSE){
  
  # Revisiones
  stopifnot(inherits(design_matrix, c("Matrix","dgCMatrix")))
  if (!is.null(weights)){
    stopifnot(inherits(weights,c("Matrix","dgCMatrix")))
  }
  
  # Obtiene la matriz de diseño e incidencia
  basic_matrix <- gen_design_matrix(data = data, ref = reference)
  # Obtiene la matriz de pesos
  W <- gen_matrix_weight(data = data, weight = weights, sparse = sparse)
  
  # Observaciones
  L <- matrix(as.numeric(data[,3]))
  
  # Matriz A
  A <- basic_matrix$inc_matrix
  
  # Ecuaciones normales
  ATP <- crossprod(A,W)
  N <- ATP%*%A
  c <- ATP%*%L
  
  # Obtiene las filas y columnas de N
  num_row_N <- nrow(N)
  num_col_N <- ncol(N)
  
  if(!(is.null(ref))){
    # dimensiones base
    n_par <- ncol(A)         # vértices desconocidos
    n_ref <- nrow(ref)       # vértices con cota conocida
    
    # Matriz K (n_par x n_ref), inicialmente ceros
    K <- Matrix(matrix(0, nrow = n_par, ncol = n_ref,
                dimnames = list(colnames(A), ref[,"nom"])),sparse = TRUE)
    
    # Llenar K con 1 cuando el vértice es referencia
    row_names_K <- rownames(K)        # nombres de vértices (incógnitas)
    col_names_K <- colnames(K)        # nombres de referencias declaradas
    comunes <- intersect(row_names_K, col_names_K)
    K[cbind(match(comunes, row_names_K), match(comunes, col_names_K))] <- 1L
    
    # Armar matriz aumentada Nc
    Nc <- Matrix(matrix(0, nrow = n_par + n_ref, ncol = n_par + n_ref),sparse=TRUE)
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
    return(list(IM = IM, A = A, P = P, L = L, x = x, v = e, K = K, L_Corregida = L_Corregida))
  } else {
    return(list(IM = IM, A = A, P = P, L = L, x = x, v = e, L_Corregida = L_Corregida))
  }
}