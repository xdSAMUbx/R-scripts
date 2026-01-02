library(Matrix)

gen_basic_matrices <- function(data, rlss = TRUE, ref = TRUE, weight = TRUE){
  stopifnot(inherits(data, "data.frame"))
  if (!isTRUE(rlss) && !isTRUE(ref)) {
    stop("Configuración inválida: rlss = FALSE y ref = FALSE no es permitido.")
  }
  
  # Verifica si el dataframe tiene vértices con referencias
  if (isTRUE(ref)){
    data.obs <- data[data[[5]] != "R", ]
    data.ref <- data[data[[5]] == "R", ]
    data.ref <- data.ref[, colSums(!is.na(data.ref)) > 0]
  } else {
    data.obs <- if (any(data[[5]] == "R")) {
      data[data[[5]] != "R", ]
    } else {
      data
    }
  }
  
  # Crea la matriz de pesos
  if (isTRUE(weight)){
    d <- data.obs[[4]]
    W <- Diagonal(x=1/d)
  } else {
    W <- Diagonal(x = rep(1,nrow(data.obs)))
  }
  
  # Obtiene la cantidad de nodos sin repetir así como n y m
  nodos <- sort(unique(c(data.obs$ini,data.obs$fin)))
  n <- nrow(data.obs)
  m <- length(nodos)
  # Crea la matriz de incidencia
  inc_matrix <- Matrix(0,nrow = m, ncol = n,
        dimnames = list(nodos, paste0("x", 1:n)), sparse = TRUE)
  
  fila_ini <- match(data.obs$ini, nodos)
  fila_fin <- match(data.obs$fin, nodos)
  cols <- seq_len(n)
  
  inc_matrix[cbind(fila_ini,cols)] <- -1
  inc_matrix[cbind(fila_fin,cols)] <- 1
  inc_matrix <- t(inc_matrix)
  
  if (isTRUE(rlss)){
    A <- inc_matrix
    L <- Matrix(data.obs[[3]])
    if (isTRUE(ref)) {
      l <- nrow(data.ref)
      # Matriz K (n_par x n_ref), inicialmente ceros
      K <- Matrix(0, nrow = l, ncol = m, dimnames = list(data.ref[[1]], nodos),
                  sparse = TRUE)
      
      # Llenar K con 1 cuando el vértice es referencia
      K[cbind(seq_len(l), match(data.ref[[1]], nodos))] <- 1
      K0 <- Matrix(data.ref$obs, ncol=1, dimnames = list(data.ref[[1]]), 
                   sparse = TRUE)
    } else {
      K <- Matrix(rep(1,m),ncol = m,dimnames = list("[1]",nodos),sparse = TRUE)
      K0 <- Matrix(0, nrow = 1, ncol=1, sparse = TRUE)
    }
    return (list(A = inc_matrix,W = W, K = K, K0 = K0, L = matrix(L)))
  } else {
    A <- inc_matrix[,!colnames(inc_matrix) %in% data.ref[[1]], drop = FALSE]
    ref_nodes <- data.ref[[1]]
    ref_vals  <- data.ref$obs
    
    h_ini_ref <- ref_vals[match(data.obs$ini, ref_nodes)]
    h_fin_ref <- ref_vals[match(data.obs$fin, ref_nodes)]
    
    h_ini_ref[is.na(h_ini_ref)] <- 0
    h_fin_ref[is.na(h_fin_ref)] <- 0
    
    L_corr <- data.obs[[3]] + h_ini_ref - h_fin_ref
    return (list(A = A, W = W, L = matrix(L_corr)))
  }
}
