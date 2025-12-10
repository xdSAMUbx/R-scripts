moda <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

pct_true <- function(x) {
  mean(x == TRUE, na.rm = TRUE)
}

scatter <- function(data, titulo, xvar, yvar, xlab, ylab, color){
  ggplot(data, aes(x = {{ xvar }}, y = {{ yvar }})) +
    geom_point(color = color) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(x = xlab, y = ylab, title = titulo)
}

# Necesarias para los box plots

box_stats <- function(data, variable) {
  
  # Capturar expresión
  var_expr <- rlang::enquo(variable)
  
  # Evaluar la expresión dentro del data frame
  v <- dplyr::mutate(data, .v = !!var_expr)$.v
  
  # Calcular cuartiles
  q <- quantile(v, probs = seq(0, 1, 0.25), na.rm = TRUE)
  
  labels_q <- paste0(
    "(", round(q[-length(q)], 2), " – ", round(q[-1], 2), ")"
  )
  
  labels_q <- c(
    paste0("Q1 (bajo): ", labels_q[1]),
    paste0("Q2: ",        labels_q[2]),
    paste0("Q3: ",        labels_q[3]),
    paste0("Q4 (alto): ", labels_q[4])
  )
  
  list(
    breaks = q,
    labels = labels_q
  )
}

boxmap <- function(data, variable, 
                   titulo = "Box Map", 
                   palette = "mako",
                   alpha_poligonos = 1) {
  
  # 1. Cuartiles dinámicos
  bs <- box_stats(data, {{variable}})
  
  data$boxclass <- cut(
    dplyr::pull(data, {{variable}}),
    breaks = bs$breaks,
    include.lowest = TRUE,
    labels = bs$labels
  )
  
  # 2. Colores únicos por cuartil
  cols_q <- viridis::viridis(
    length(levels(data$boxclass)), 
    option = palette
  )
  names(cols_q) <- levels(data$boxclass)
  
  # 3. Color por localidad según cuartil asignado
  loc_df <- data |>
    dplyr::select(LocNombre, boxclass) |>
    sf::st_drop_geometry() |>
    dplyr::distinct()
  
  cols_loc <- cols_q[loc_df$boxclass]
  names(cols_loc) <- loc_df$LocNombre
  
  # Punto fantasma para leyenda
  bb <- sf::st_bbox(data)
  
  ggplot() +
    geom_point(
      data = loc_df,
      aes(x = bb["xmin"], y = bb["ymin"], color = boxclass),
      size = 0.1, shape = 15, inherit.aes = FALSE
    ) +
    
    scale_color_manual(
      values = cols_q,
      name = "Cuartiles"
    ) +
    
    # Polígonos con transparencia controlable
    geom_sf(
      data = data,
      aes(fill = LocNombre),
      color = "black",
      size = 0.2,
      alpha = alpha_poligonos
    ) +
    
    scale_fill_manual(
      values = cols_loc,
      name = "Localidad"
    ) +
    
    guides(
      color = guide_legend(override.aes = list(size = 6), order = 1),
      fill  = guide_legend(override.aes = list(shape = 15, alpha = 1), order = 2)
    ) +
    
    theme_minimal() +
    labs(title = titulo) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

make_boxplot <- function(data, variable, xlabel = "", titulo = "Boxplot",
                         color_fill = "skyblue") {
  
  ggplot(data, aes(x = "", y = {{ variable }})) +
    stat_boxplot(geom = "errorbar", width = 0.25) +
    geom_boxplot(outlier.colour = 1, fill = color_fill) +
    labs(title = titulo,
         x = xlabel,
         y = "") +
    theme_minimal() +
    theme(
      plot.title  = element_text(hjust = 0.5, face = "bold"),
      panel.border = element_rect(fill = NA, color = "black")
    )
}

qq_plot <- function(data, var, col_pts, col_line="red", title = "QQ Plot", 
                    xlab = "Cuantiles Teóricos", ylab="Cuantiles Observados"){
  ggplot(data, aes(sample = {{ var }})) +
    stat_qq(color = col_pts, size = 2) +
    stat_qq_line(color = col_line, linewidth=0.8) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14,face="bold",hjust=0.5),
      panel.border = element_rect(fill = NA, color = "black")
    )
}

rezagos_k <- function(x, Wmat, k = 5) {
  out <- list()
  current <- x
  
  for (i in 1:k) {
    current <- Wmat %*% current
    out[[paste0("W", i)]] <- as.numeric(current)
  }
  
  return(out)
}