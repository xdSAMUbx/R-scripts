# Funciones suplementarias

# Producto punto
prodPoint <- function(x,y){
  # Validar que sean vectores atómicos
  if (!is.atomic(x) || !is.atomic(y)) {
    stop("Ambos argumentos deben ser vectores atómicos.")
  }
  
  # Validar que todos los valores sean numéricos
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Ambos vectores deben contener solo valores numéricos.")
  }
  
  # Validar que tengan misma longitud
  if (length(x) != length(y)) {
    stop("Los vectores deben tener la misma longitud.")
  }
  
  return(sum(x*y))
}

# Traza
tr <- function(X){
  if(!(class(X) %in% c("matrix","array"))){
    stop("No es una matriz")
  }
    
  return(sum(diag(X)))
}

# Ortogonalidad
is.ort <- function(x,y){
  
  # Verificar que sean vectores numéricos
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Ambos argumentos deben ser vectores numéricos")
  }
  
  # Verificar que tengan misma longitud
  if (length(x) != length(y)) {
    stop("Los vectores deben tener la misma longitud")
  }
  
  z <- x*y
  if (sum(z) == 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
