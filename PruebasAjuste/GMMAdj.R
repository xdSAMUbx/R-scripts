source("genA.R")

# Función que permite hacer el ajuste a traves del modelo de gauss - markov
GMM <- function(data, ref=NULL, Weigth=FALSE){
  
  # 1) Verifica que la información este en data.frame
  stopifnot(inherits(data,"data.frame"))
  
  # 2) Genera la matriz de pesos según si esta en km, millas o no hay
  # Esto para obtenerla en terminos de metros, pies o la matriz identidad
  d <- as.numeric(data[,4])
  if (!(isTRUE(Weigth))){
    P <- diag(nrow(data))
  } else {
    P <- diag(1/d)
  }
  
  # 3) Genera la matriz A
  A <- as.matrix(genA(data, ref = ref )$A)
  rankA <- qr(A)$rank
  
  if (rankA < ncol(A)) {
    A <- A[ , -1, drop = FALSE]
  }
  
  if(!(is.null(ref))){
    mapaReferencias <- setNames(ref$h, ref$nom)
    corr <- apply(data, 1, function(fila) {
      if (fila[["ini"]] %in% names(mapaReferencias)) {
        mapaReferencias[fila[["ini"]]]
      } else if (fila[["fin"]] %in% names(mapaReferencias)) {
        mapaReferencias[ fila[["fin"]] ]
      } else {
        0
      }
    })
    data[,3] <- data[,3] + as.numeric(corr) 
  }
  
  # 4) Calcula A'P que es necesaria para todon el proceso
  ATP <- crossprod(A,P)
  y <- matrix(as.numeric(data[,3]))
  N <- ATP%*%A
  c <- ATP%*%y
  
  # 5) Ajusta las observaciones
  x <- solve(N,c)
  Ax <- A%*%x
  
  # 6) Calcula las medidas ajustadas, y el vector de residuos (v)
  e <- y - Ax
  Y <- y - e
  return(list(A = A, P = P, y = y, x = x, v = e, Y = Y))
}