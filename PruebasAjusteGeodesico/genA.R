# Función que me va a permitir generar la matriz de diseño A
genA <- function(data, ref = NA){
  # 1) Validar que el data este en data.frame
  stopifnot(inherits(data,"data.frame"))
  
  # 2) Valida que hallan o no ref
  if (!all(is.na(ref))) {
    stopifnot(inherits(ref, "data.frame"))
  }
  
  # 3) Valida que existe la columna ini y fin para la matriz de incidencia
  if (!("ini" %in% names(data))) stop("Falta el atributo 'ini' en data.")
  if (!("fin" %in% names(data))) stop("Falta el atributo 'fin' en data.")
  
  # 4) Obtiene los nodos y calcula el # de nodos y el # de aristas
  nodes    <- sort(unique(c(data$ini, data$fin)))
  numNodes <- length(nodes)
  numEdges <- nrow(data)
  
  # 5) Genera la matriz de incidencia dirigida (nodos x aristas) (v x e)
  IM <- matrix(0, nrow = numNodes, ncol = numEdges,
               dimnames = list(nodes, paste0("x", 1:numEdges)))
  
  # Halla las posiciones donde salen y llegan los nodos
  fila_ini <- match(data$ini, nodes)
  fila_fin <- match(data$fin, nodes)
  cols <- seq_len(numEdges)
  
  IM[cbind(fila_ini,cols)] <- -1
  IM[cbind(fila_fin,cols)] <- 1
  
  IM <- t(IM)   # aristas x nodos
  
  # 6) Si hay ref y no existe la columna nom (nomenclatura, nombre)
  # No puede continuar
  
  if (!all(is.na(ref))) {
    if (!("nom" %in% names(ref))) {
      stop("El data.frame 'ref' debe tener la columna 'nom' con los nodos conocidos")
    }
    nodos_fijos <- ref$nom
  } else {
    nodos_fijos <- character(0)  # ninguno
  }
  
  # 7. Construir matriz de diseño A quitando las columnas de vertices libres de error 
  nodos_estimar <- colnames(IM) %in% nodos_fijos
  
  # protección: si todas las columnas se eliminan, devolvemos matriz con 0 columnas pero
  # seguimos manteniendo estructura de matriz
  A <- IM[, !(nodos_estimar), drop=FALSE]
  dimnames(A) <- NULL
  return(list(A=A,IM=IM))
}