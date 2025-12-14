# Función que me va a permitir generar la matriz de diseño A
gen_design_matrix <- function(data, ref = NA){
  # Importa el paquete para manejo de matrices dispersas
  library(Matrix)
  
  # 1) Validar que el data este en data.frame
  stopifnot(inherits(data,"data.frame"))
  
  # 2) Valida que hallan o no ref
  if (!all(is.na(ref))) {
    stopifnot(inherits(ref, "data.frame"))
  }
  
  # 3) Valida que existe la columna ini y fin para la matriz de incidencia
  if (!("ini" %in% names(data))) stop("Faltan los vértices de inicio de la nivelación.")
  if (!("fin" %in% names(data))) stop("Faltan los vértices de llegada en la nivelación.")
  if (!("obs" %in% names(data))) stop("Faltan las diferencias de altura observadas.")
  if (!("dist" %in% names(data))) stop("Falta la distancia en los datos observados.")
  
  # 4) Obtiene los nodos y calcula el # de nodos y el # de aristas
  nodos    <- sort(unique(c(data$ini, data$fin)))
  num_nodos <- length(nodos)
  num_aristas <- nrow(data)
  
  # 5) Genera la matriz de incidencia dirigida (nodos x aristas) (v x e)
  inc_matrix <- Matrix(matrix(0, nrow = num_nodos, ncol = num_aristas,
               dimnames = list(nodos, paste0("x", 1:num_aristas))),sparse = TRUE)
  
  # Halla las posiciones donde salen y llegan los nodos
  fila_ini <- match(data$ini, nodos)
  fila_fin <- match(data$fin, nodos)
  cols <- seq_len(num_aristas)
  
  inc_matrix[cbind(fila_ini,cols)] <- -1
  inc_matrix[cbind(fila_fin,cols)] <- 1
  
  inc_matrix <- t(inc_matrix)   # aristas x nodos
  
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
  nodos_estimar <- colnames(inc_matrix) %in% nodos_fijos
  
  # protección: si todas las columnas se eliminan, devolvemos matriz con 0 columnas pero
  # seguimos manteniendo estructura de matriz
  A <- inc_matrix[, !nodos_estimar, drop=FALSE]
  return(list(design_matrix=A,inc_matrix=inc_matrix))
}