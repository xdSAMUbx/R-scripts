# Librerias
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(lmtest)
library(olsrr)
library(car)
library(nortest)
library(caret)
library(psych)
library(skedastic)
library(rms)
library(nlme)
library(openxlsx)
library(patchwork)
library(GWmodel)
library(viridis)
library(spdep)
library(adespatial)
library(bispdep)
library(spatialreg)


# Ruta de los datos
setwd("G:/Otros ordenadores/Mi PC/Universidad/8 Semestre/Econometria/R-scripts/ProyFinEco")

# Funciones suplementarias
source("comp_func.R")
source("G:/Otros ordenadores/Mi PC/Universidad/7 Semestre/Geoestadistica/Libros y Material/Datos de Area/Funciones/sp.correlogram.R")
source("G:/Otros ordenadores/Mi PC/Universidad/7 Semestre/Geoestadistica/Libros y Material/Datos de Area/Funciones/test.w.R")
source("G:/Otros ordenadores/Mi PC/Universidad/7 Semestre/Geoestadistica/Libros y Material/Datos de Area/Funciones/test.w.R")

#####################
# Ordenando la data #
#####################
df <- as.data.frame(read.xlsx("data/database_scrap.xlsx"))
colnames(df) <- sub(".*\\.", "", colnames(df))
df <- df %>%
  distinct(latitud, longitud, .keep_all = TRUE)
loc <- st_transform(st_read("data/loca.gpkg"),4326)
sp_df <- st_as_sf(df, coords = c("longitud", "latitud"), crs=4326)
df_crimen <- data.frame(
  Localidad = c(
    "01-USAQUÉN","02-CHAPINERO","03-SANTA FE","04-SAN CRISTÓBAL","05-USME",
    "06-TUNJUELITO","07-BOSA","08-KENNEDY","09-FONTIBÓN","10-ENGATIVÁ",
    "11-SUBA","12-BARRIOS UNIDOS","13-TEUSAQUILLO","14-LOS MÁRTIRES",
    "15-ANTONIO NARIÑO","16-PUENTE ARANDA","17-CANDELARIA",
    "18-RAFAEL URIBE URIBE","19-CIUDAD BOLÍVAR","20 - SUMAPAZ"
  ),
  Tasa_Criminalidad = c(
    11.01042633,11.59057211,6.33376009,7.541895166,5.678022547,
    4.061020474,11.91150382,19.88542244,11.07214397,17.29328171,
    18.16967215,5.850831929,7.924544511,8.529377348,
    3.320408837,9.393424257,4.801632111,5.863175456,14.1209952,1.974964364
  )
)
df_crimen$Localidad <- sub("^\\s*\\d+\\s*-\\s*", "", df_crimen$Localidad)
quitar_tildes <- function(x) {
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}
df_crimen$Localidad <- quitar_tildes(df_crimen$Localidad)
# Detectar puntos dentro
mat <- st_within(sp_df, loc, sparse = FALSE)
inside <- apply(mat, 1, any)   # TRUE si cae en cualquier localidad
# Filtrar puntos dentro
sp_df <- sp_df[inside, ]

sp_df$num_habs[is.na(sp_df$num_habs)] <- 1
sp_df$num_huespedes[is.na(sp_df$num_huespedes)] <- 1
sp_df$num_mascotas[is.na(sp_df$num_mascotas)] <- 1
sp_df$num_beds[is.na(sp_df$num_beds)] <- 1

# Dataframe Espacial de localidades
sp_loc <- st_join(sp_df, loc["LocNombre"], left = FALSE)
summary_loc <- sp_loc |> 
  st_drop_geometry() |> 
  group_by(LocNombre) |> 
  summarise(
    n_airbnb       = n(),
    precio_prom    = mean(precio, na.rm = TRUE),
    hospedaje_mod  = moda(hospedaje),
    
    super_pct      = pct_true(superanfitrion),
    
    bano_pct       = pct_true(bano),
    lavadora_pct   = pct_true(lavadora),
    secadora_pct   = pct_true(secadora),
    entret_pct     = pct_true(entretenimiento),
    internet_pct   = pct_true(internet),
    utens_pct      = pct_true(utensilios),
    estac_pct      = pct_true(estacionamiento),
    ascensor_pct   = pct_true(ascensor)
  )

sp_df_loc <- loc |> 
  left_join(summary_loc, by = "LocNombre") |> 
  drop_na()

sp_df_loc <- sp_df_loc |> select(-LocAAdmini, -LocCodigo)
sp_df_loc$LocNombre <- quitar_tildes(sp_df_loc$LocNombre)
sp_df_loc <- sp_df_loc |>
  left_join(df_crimen, by = c("LocNombre" = "Localidad"))

# Dataframe ordenado vértices
df_fin <- sp_loc
df_fin$LocNombre <- quitar_tildes(df_fin$LocNombre)
df_fin <- df_fin |> 
  rename(
    cal_gen      = calificacion_general,
    res          = resenas,
    num_pets     = num_mascotas,
    superhost    = superanfitrion,
    localidad    = LocNombre
  )
df_fin <- df_fin |>
  left_join(df_crimen, by = c("localidad" = "Localidad"))
df_fin <- df_fin |> 
  rename(
    crime = Tasa_Criminalidad
  )
df_fin <- df_fin |> 
  select(
    precio,
    hospedaje,
    cal_gen,
    res,
    num_habs,
    num_beds,
    num_huespedes,
    num_pets,
    crime,
    superhost,
    bano,
    lavadora,
    secadora,
    entretenimiento,
    internet,
    utensilios,
    estacionamiento,
    ascensor,
    localidad,
    geometry
  )

# Generando dummys
cols_dummy <- c(
  "superhost",
  "bano",
  "lavadora",
  "secadora",
  "entretenimiento",
  "internet",
  "utensilios",
  "estacionamiento",
  "ascensor"
)

df_fin <- df_fin |> 
  mutate(across(all_of(cols_dummy), ~ as.numeric(.x)))

df_fin <- df_fin |> 
  distinct(geometry, .keep_all = TRUE)

#########################
# Análisis Exploratorio #
#########################

# Espacial distribución información
x11()
ggplot() +
  geom_sf(data = sp_df_loc, aes(fill = LocNombre), color = "black", size = 0.2) +
  scale_fill_viridis_d(option = "plasma", alpha = 0.3, name = "Localidad") + 
  geom_sf(data = df_fin, aes(color = hospedaje), size = 1.5) +
  scale_color_manual(
    values = c(
      "Habitación" = "red",
      "Alojamiento Entero" = "black"
    ),
    name = "Tipo de hospedaje"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5,     # centrar título
          color = "black"
        )) +
  guides(
    color = guide_legend(override.aes = list(size = 2))   # <-- puntos grandes en la leyenda
  ) +
  labs(
    title = "Distribución de Airbnb's en Bogotá"
  )

# Scatter plots
# Número reseñas vs precio
plot_res <- scatter(df_fin, "Relación Precio vs Número de Reseñas", log(res), log(precio),
        "Logaritmo del Número de Reseñas", "Logaritmo del Precio del Airbnb", "red")
# Calificación general vs precio
plot_cal <- scatter(df_fin, "Relación Precio vs Calificación General", cal_gen, log(precio),
        "Calificación General", "Logaritmo del Precio del Airbnb", "blue")
x11(width = 10, height = 5)
(plot_res | plot_cal) 

# Box maps y Box plots
x11(width=10, height = 6)
bm_crimen <- boxmap(sp_df_loc,Tasa_Criminalidad, 
                   "Box Map Criminalidad por Localidad","inferno",0.8)
bp_crimen <- make_boxplot(sp_df_loc,Tasa_Criminalidad,
                          "Criminalidad", "Box Plot Criminalidad por Localidad", "brown")
(bm_crimen | bp_crimen)

x11(width=10,height = 8)
bp_precio <- make_boxplot(df_fin,log(precio),
                          "Logaritmo del Precio", "Box Plot del Precio","darkgoldenrod4")
bp_cal <-  make_boxplot(df_fin,cal_gen, 
                        "Calificación General", "Box Plot de Calificación General","darkorchid4")
bp_resenas <- make_boxplot(df_fin,log(res), 
                           "Logaritmo del Número de Reseñas", "Box Plot de Número de Reseñas","aquamarine4") 
(bp_precio | bp_cal | bp_resenas)

# Summary puntos
summary(df_fin)
df.no.sp <- st_drop_geometry(df_fin)
psych::describe(df.no.sp)

# Generando dummys por habitación
df.no.sp$DummyHab <- ifelse(df.no.sp$hospedaje == "Habitación", 1, 0)
df.no.sp$DummyAloja <- ifelse(df.no.sp$hospedaje == "Alojamiento Entero", 1, 0)
df_fin$DummyHab <- ifelse(df_fin$hospedaje == "Habitación", 1, 0)
df_fin$DummyAloja <- ifelse(df_fin$hospedaje == "Alojamiento Entero", 1, 0)

#########################
# Regresiones Lineales #
########################

# MATRICES DE PESO PARA EVALUAR AUTOCORRELACIÓN ESPACIAL
pts.coords <- st_coordinates(st_geometry(df_fin))
# Usando vecinos más cercanos
knn1 <- knn2nb(knearneigh(pts.coords, k=1))
prec.k1 <-nb2listw(knn1, style="W", zero.policy = TRUE)
knn2 <- knn2nb(knearneigh(pts.coords, k=2))
prec.k2 <-nb2listw(knn2, style="W", zero.policy = TRUE)
knn3 <- knn2nb(knearneigh(pts.coords, k=3))
prec.k3 <-nb2listw(knn3, style="W", zero.policy = TRUE)
knn4 <- knn2nb(knearneigh(pts.coords, k=4))
prec.k4 <-nb2listw(knn4, style="W", zero.policy = TRUE)
knn5 <- knn2nb(knearneigh(pts.coords, k=5))
prec.k5 <-nb2listw(knn5, style="W", zero.policy = TRUE)
knn6 <- knn2nb(knearneigh(pts.coords, k=6))
prec.k6 <-nb2listw(knn6, style="W", zero.policy = TRUE)
knn7 <- knn2nb(knearneigh(pts.coords, k=7))
prec.k7 <-nb2listw(knn7, style="W", zero.policy = TRUE)
knn8 <- knn2nb(knearneigh(pts.coords, k=8))
prec.k8 <-nb2listw(knn8, style="W", zero.policy = TRUE)
knn9 <- knn2nb(knearneigh(pts.coords, k=9))
prec.k9 <-nb2listw(knn9, style="W", zero.policy = TRUE)
knn10 <- knn2nb(knearneigh(pts.coords, k=10))
prec.k10 <-nb2listw(knn10, style="W", zero.policy = TRUE)
# Umbral de distancias
ud4 <- dnearneigh(pts.coords, 0, 0.04)
ud4.w<-nb2listw(ud4, style="W", zero.policy=TRUE)
ud5 <- dnearneigh(pts.coords, 0, 0.05)
ud5.w<-nb2listw(ud5, style="W", zero.policy=TRUE)
ud6 <- dnearneigh(pts.coords, 0, 0.06)
ud6.w<-nb2listw(ud6, style="W", zero.policy=TRUE)
ud7 <- dnearneigh(pts.coords, 0, 0.04)
ud7.w<-nb2listw(ud7, style="W", zero.policy=TRUE)
# Distancias de gabriel
gab <- graph2nb(gabrielneigh(pts.coords),sym=TRUE)
gab.nb <-nb2listw(gab, style="W", zero.policy = TRUE)
# Delaunay
trinb <- tri2nb(pts.coords)
delaunay <-nb2listw(trinb, style="W", zero.policy = TRUE)
# Esfera de Influencia
soinb <- graph2nb(soi.graph(trinb,pts.coords))
esf.influencia <-nb2listw(soinb, style="W", zero.policy = TRUE)
# Vecinos relativos
relativenb <- graph2nb(relativeneigh(pts.coords),sym=TRUE)
vec.relativos <-nb2listw(relativenb, style="W", zero.policy = TRUE)

lista_nb <- list(
  # KNN
  knn1  = knn1,
  knn2  = knn2,
  knn3  = knn3,
  knn4  = knn4,
  knn5  = knn5,
  knn6  = knn6,
  knn7  = knn7,
  knn8  = knn8,
  knn9  = knn9,
  knn10 = knn10,
  
  # DISTANCIAS
  ud4 = ud4,
  ud5 = ud5,
  ud6 = ud6,
  ud7 = ud7,
  
  # GRAFOS ESPACIALES
  gab        = gab,
  delaunay   = trinb,
  esfera_inf = soinb,
  relativos  = relativenb
)


lista_w <- list(
  # KNN pesos
  wk1  = prec.k1,
  wk2 = prec.k2,
  wk3  = prec.k3,
  wk4  = prec.k4,
  wk5  = prec.k5,
  wk6  = prec.k6,
  wk7  = prec.k7,
  wk8  = prec.k8,
  wk9  = prec.k9,
  wk10 = prec.k10,
  
  # Pesos por distancia
  ud4.w = ud4.w,
  ud5.w = ud5.w,
  ud6.w = ud6.w,
  ud7.w = ud7.w,
  
  # Pesos por grafos
  gab.w        = gab.nb,
  delaunay.w   = delaunay,
  esfera_inf.w = esf.influencia,
  rel.w        = vec.relativos
)

# Seleccioón mejor matriz de pesos
best.W <- sapply(names(lista_nb), function(nm) {
  
  cat("\n==========================\n")
  cat("Evaluando matriz:", nm, "\n")
  cat("==========================\n")
  
  w <- lista_nb[[nm]]
  
  test.W(df_fin$precio, w)
})

# Modelos clásicos
summary(mod1 <- lm(precio ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + num_pets + crime, 
           data=df.no.sp))
shapiro.test(residuals(mod1))
bptest(mod1)
vif(mod1)
moran.test(residuals(mod1),esf.influencia)

summary(mod2 <- lm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + num_pets + crime, 
                   data=df.no.sp))
shapiro.test(residuals(mod2))
bptest(mod2)
vif(mod2)
moran.test(residuals(mod2),esf.influencia)

# Regresión con dummys
summary(mod3 <- lm(precio ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                     num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                   + ascensor + DummyAloja, data = df.no.sp))
shapiro.test(residuals(mod3))
bptest(mod3)
vif(mod3)
moran.test(residuals(mod3),esf.influencia)

# Modelo seleccionado
summary(mod4 <- lm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
             num_pets + crime + superhost + bano + entretenimiento + estacionamiento
           + ascensor + DummyAloja, data = df.no.sp))
shapiro.test(residuals(mod4))
bptest(mod4)
vif(mod4)
moran.test(residuals(mod4),esf.influencia)

stargazer(mod1, mod2, mod3, mod4, type = "latex",
          title = "Comparación de modelos",
          digits = 3)

############################
# Corrección de supuestos # 
###########################

# Heterocedasticidad
bptest(mod4)
x11(width = 10, height=10)
par(mfrow=c(2,2))
plot(residuals(mod4),ylab="Residuos",xlab="# Datos",main="Residuos vs # Observaciones",
     pch=21,bg="maroon")
grid()
plot(log(df.no.sp$precio),residuals(mod4),ylab="Residuos",xlab="Precio Transformado",
     main="Residuos vs Precio Transformado", pch=22, bg="lemonchiffon")
grid()
plot(df.no.sp$cal_gen,residuals(mod4),ylab="Residuos",xlab="Calificación General",
     main="Residuos vs Calificación General",pch=23, bg="seagreen1")
grid()
plot(log(df.no.sp$res),residuals(mod4),ylab="Residuos",xlab="Ln(reseñas)",
     main="Residuos vs Ln(# Reseñas)",pch=24,bg="turquoise")
grid()

summary(mod_pond <- lm(abs(residuals(mod4)) ~ log(precio), data = df.no.sp))
alpha <- coef(mod_pond)[1]
beta  <- coef(mod_pond)[2]
df.no.sp$w <- 1 / (alpha + beta * log(df.no.sp$precio))^2

summary(mod8 <- lm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
             num_pets + crime + superhost + bano + entretenimiento + estacionamiento
           + ascensor + DummyAloja, data = df.no.sp, weights = w))
bptest(mod8)

summary(mod9 <- lm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                     num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                   + ascensor + DummyAloja, data = df.no.sp, weights = 1/residuals(mod4)^2))
bptest(mod9)

stargazer(mod8, mod9, type = "latex",
          title = "Comparación de Modelos Ponderados",
          digits = 3)

# GWR
# Generando dummys por habitación
df.sf <- st_transform(df_fin,9377)

# Ancho de Banda
bw <- bw.gwr(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
               num_pets + crime + superhost + bano + entretenimiento + estacionamiento
             + ascensor + DummyAloja, data = df.sf, approach="CV", kernel="exponential",
             adaptive = T)

gwr1 <- gwr.basic(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                    num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                  + ascensor + DummyAloja, data = df.sf, bw = bw, kernel = "exponential",
                  adaptive = T, F123.test=T)
gwr1

gwr.sp <- as(gwr1$SDF,"Spatial")
names(gwr.sp)
loc.trans <- st_transform(loc,9377)

x11()
ggplot() +
  geom_sf(data = sp_df_loc, aes(fill = LocNombre), color = "black", size = 0.2) +
  scale_fill_viridis_d(option = "inferno", alpha = 0.5, name = "Localidad") + 
  geom_sf(data = st_as_sf(gwr.sp), aes(color = residual), size = 1.5) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5,     # centrar título
          color = "black"
        )) +
  guides(
    color = guide_legend(override.aes = list(size = 2))   # <-- puntos grandes en la leyenda
  ) +
  labs(
    title = "Residuos por Zona de GWR"
  )

####################
# Modelo Espacial #
###################

# Univariadas
moran.test(df_fin$precio,esf.influencia,alternative="two.sided")
moran.test(log(df_fin$precio),esf.influencia,alternative="two.sided", zero.policy = TRUE)
moran.test(df_fin$cal_gen,esf.influencia,alternative="two.sided")
moran.test(df_fin$res,esf.influencia,alternative="two.sided")
moran.test(log(df_fin$res),esf.influencia,alternative="two.sided") # Significativa espacialmente
moran.test(df_fin$num_beds,esf.influencia,alternative="two.sided")
moran.test(df_fin$num_habs,esf.influencia,alternative="two.sided")
moran.test(df_fin$num_huespedes,esf.influencia,alternative="two.sided")
moran.test(df_fin$num_pets,esf.influencia,alternative="two.sided")
moran.test(df_fin$crime,esf.influencia,alternative="two.sided") # Significativa espacialmente
joincount.test(as.factor(df_fin$superhost),esf.influencia, alternative = "two.sided")
joincount.test(as.factor(df_fin$bano),esf.influencia, alternative = "two.sided")
joincount.test(as.factor(df_fin$entretenimiento),esf.influencia, alternative = "two.sided")
joincount.test(as.factor(df_fin$estacionamiento),esf.influencia, alternative = "two.sided") # Significativa espacialmente
joincount.test(as.factor(df_fin$ascensor),esf.influencia, alternative = "two.sided") # Significativa espacialmente
joincount.test(as.factor(df_fin$DummyAloja),esf.influencia, alternative = "two.sided")

x11()
par(mfrow=c(2,2))
plot(sp.correlogram(soinb, log(df_fin$res), order=5, method="I", style="W", zero.policy=T), main="Correlograma Ln(# Reseñas)")
plot(sp.correlogram(soinb, df_fin$crime, order=5, method="I", style="W", zero.policy=T), main="Correlograma Crimen")
plot(sp.correlogram(soinb, df_fin$estacionamiento, order=5, method="I", style="W", zero.policy=T), main="Correlograma Estacionamientos")
plot(sp.correlogram(soinb, df_fin$ascensor, order=5, method="I", style="W", zero.policy=T), main = "Correlograma Ascensor")

x11()
plot(sp.correlogram(soinb, residuals(mod4), order=5, method="I", style="W", zero.policy=T), main="Correlograma Residuos")
# Bivariadas
moranbi.test(log(df_fin$precio),df_fin$cal_gen,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),log(df_fin$res),esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$num_habs,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$num_beds,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$num_huespedes,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$num_pets,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$crime,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$superhost,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$bano,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$entretenimiento,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$estacionamiento,esf.influencia,alternative = "two.sided")
moranbi.test(log(df_fin$precio),df_fin$ascensor,esf.influencia,alternative = "two.sided")

# Calculando rezagos de las variables explicativas 
w_sp <- listw2mat(esf.influencia) 
Wmat <- w_sp / rowSums(w_sp)

vars_X <- setdiff(names(df_fin), c("precio","hospedaje","localidad","geometry"))

rezagos_list <- list()

for (v in vars_X) {
  rezagos_list[[v]] <- rezagos_k(df_fin[[v]], Wmat, k = 5)
}

for (v in names(rezagos_list)) {
  for (k in 1:5) {
    new_name <- paste0(v, "_W", k)
    df_fin[[new_name]] <- rezagos_list[[v]][[paste0("W", k)]]
  }
}

formula.rezagada <- "log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
  num_pets + crime + superhost + bano + entretenimiento + estacionamiento + ascensor + 
  DummyAloja + cal_gen_W1  + cal_gen_W2 + cal_gen_W3 + cal_gen_W4 + cal_gen_W5 + log(res_W2) + log(res_W2) +
  log(res_W3) + log(res_W4) + log(res_W5) + num_habs_W2 + num_habs_W2 + num_habs_W3 + num_habs_W4 + 
  num_habs_W5 + num_beds_W1 + num_beds_W2 + num_beds_W3 + num_beds_W4 + num_beds_W5 + num_huespedes_W1 + num_huespedes_W2 + 
  num_huespedes_W3 + num_huespedes_W4 + num_huespedes_W5 + num_pets_W1 + num_pets_W2 + num_pets_W3 + num_pets_W4 +
  num_pets_W5 + crime_W1 + crime_W2 + crime_W3 + crime_W4 + crime_W5 + superhost_W1 + superhost_W2 + superhost_W3 +
  superhost_W4 + superhost_W5 + bano_W1 + bano_W2 + bano_W3 + bano_W4 + bano_W5 + entretenimiento_W1+ entretenimiento_W2 + 
  entretenimiento_W3 + entretenimiento_W4 + entretenimiento_W5 + estacionamiento_W1 + estacionamiento_W2 + estacionamiento_W3 +
  estacionamiento_W4 + estacionamiento_W5 + ascensor_W1 + ascensor_W2 + ascensor_W3 + ascensor_W4 + ascensor_W5 +
  DummyAloja_W1 + DummyAloja_W2 + DummyAloja_W3 + DummyAloja_W4 + DummyAloja_W5"

f.rez.better <- "log(precio) ~ num_beds + num_huespedes + num_pets + bano"
f.rez.better.W1 <- "log(precio) ~ num_beds + num_huespedes + num_pets + bano + num_beds_W1 + num_huespedes_W1 + 
  num_pets_W1 + bano_W1"
f.rez.better.W2 <- "log(precio) ~ num_beds + num_huespedes + num_pets + bano + num_beds_W1 + num_huespedes_W1 + 
  num_pets_W1 + bano_W1 + num_beds_W2 + num_huespedes_W2 + num_pets_W2 + bano_W2"
f.rez.better.W3 <- "log(precio) ~ num_beds + num_huespedes + num_pets + bano + num_beds_W1 + num_huespedes_W1 + 
  num_pets_W1 + bano_W1 + num_beds_W2 + num_huespedes_W2 + num_pets_W2 + bano_W2 + num_beds_W3 + 
  num_huespedes_W3 + num_pets_W3 + bano_W3"
f.rez.better.W4 <- "log(precio) ~ num_beds + num_huespedes + num_pets + bano + num_beds_W1 + num_huespedes_W1 + 
  num_pets_W1 + bano_W1 + num_beds_W2 + num_huespedes_W2 + num_pets_W2 + bano_W2 + num_beds_W3 + 
  num_huespedes_W3 + num_pets_W3 + bano_W3 + num_beds_W4 + num_huespedes_W4 + num_pets_W4 + bano_W4"
f.rez.better.W5 <- "log(precio) ~ num_beds + num_huespedes + num_pets + bano + num_beds_W1 + num_huespedes_W1 + 
  num_pets_W1 + bano_W1 + num_beds_W2 + num_huespedes_W2 + num_pets_W2 + bano_W2 + num_beds_W3 + 
  num_huespedes_W3 + num_pets_W3 + bano_W3 + num_beds_W4 + num_huespedes_W4 + num_pets_W4 + bano_W4 + 
  num_beds_W5 + num_huespedes_W5 + num_pets_W5 + bano_W5"

# Modelos Espaciales rezagados en primer orden
summary(GNS.1 <- sacsarlm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                            num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                          + ascensor + DummyAloja, data = df_fin, listw = esf.influencia, Durbin = TRUE))
shapiro.test(residuals(GNS.1))
bptest.Sarlm(GNS.1)
moran.test(residuals(GNS.1),esf.influencia)

summary(SACSAR.1 <- sacsarlm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                               num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                             + ascensor + DummyAloja, data = df_fin, listw = esf.influencia, Durbin = FALSE))
shapiro.test(residuals(SACSAR.1))
bptest.Sarlm(SACSAR.1)
moran.test(residuals(SACSAR.1),esf.influencia)

summary(SDM.1 <- lagsarlm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                            num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                          + ascensor + DummyAloja, data = df_fin, listw = esf.influencia, Durbin = TRUE))
shapiro.test(residuals(SDM.1))
bptest.Sarlm(SDM.1)
moran.test(residuals(SDM.1),esf.influencia)

summary(SDEM.1 <- errorsarlm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                               num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                             + ascensor + DummyAloja, data = df_fin, listw = esf.influencia, Durbin = TRUE))
shapiro.test(residuals(SDEM.1))
bptest.Sarlm(SDEM.1)
moran.test(residuals(SDEM.1),esf.influencia)

summary(SEM.1 <- errorsarlm(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                    num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                  + ascensor + DummyAloja, data = df_fin, listw = esf.influencia))
shapiro.test(residuals(SEM.1))
bptest.Sarlm(SEM.1)
moran.test(residuals(SEM.1),esf.influencia)

summary(SLX.1 <- lmSLX(log(precio) ~ cal_gen + log(res) + num_habs + num_beds + num_huespedes + 
                         num_pets + crime + superhost + bano + entretenimiento + estacionamiento
                       + ascensor + DummyAloja, data = df_fin, listw = esf.influencia))
shapiro.test(residuals(SLX.1))
bptest(SLX.1)
moran.test(residuals(SLX.1),esf.influencia)

# Modelos espaciales sin variables basura
print(summary(GNS.1.1 <- sacsarlm(as.formula(f.rez.better),data = df_fin, listw = esf.influencia, Durbin = TRUE),
              Nagelkerke=TRUE),signif.stars=TRUE)
shapiro.test(residuals(GNS.1.1))
bptest.Sarlm(GNS.1.1)
moran.test(residuals(GNS.1.1),esf.influencia)

print(summary(SACSAR.1.1 <- sacsarlm(as.formula(f.rez.better), data = df_fin, listw = esf.influencia, Durbin = FALSE),
        Nagelkerke=TRUE),signif.stars=TRUE)
shapiro.test(residuals(SACSAR.1))
bptest.Sarlm(SACSAR.1)
moran.test(residuals(SACSAR.1),esf.influencia)

print(summary(SDM.1 <- lagsarlm(as.formula(f.rez.better), data = df_fin, listw = esf.influencia, Durbin = TRUE),
        Nagelkerke=TRUE),signif.stars=TRUE)
shapiro.test(residuals(SDM.1))
bptest.Sarlm(SDM.1)
moran.test(residuals(SDM.1),esf.influencia)

print(summary(SDEM.1 <- errorsarlm(as.formula(f.rez.better), data = df_fin, listw = esf.influencia, Durbin = TRUE),
              Nagelkerke=TRUE),signif.stars=TRUE)
shapiro.test(residuals(SDEM.1))
bptest.Sarlm(SDEM.1)
moran.test(residuals(SDEM.1),esf.influencia)

print(summary(SEM.1 <- errorsarlm(as.formula(f.rez.better), data = df_fin, listw = esf.influencia), Nagelkerke = TRUE),
      signif.stars = TRUE)
shapiro.test(residuals(SEM.1))
bptest.Sarlm(SEM.1)
moran.test(residuals(SEM.1),esf.influencia)

summary(SLX.1 <- lmSLX(as.formula(f.rez.better), data = df_fin, listw = esf.influencia),Nagelkerke = TRUE)
shapiro.test(residuals(SLX.1))
bptest(SLX.1)
moran.test(residuals(SLX.1),esf.influencia)


# Modelos rezagados hasta el 5to Orden
print(summary(GNS.5 <- sacsarlm(as.formula(f.rez.better.W5), data = df_fin, listw = esf.influencia, Durbin = FALSE),
        Nagelkerke = TRUE),signif.stars=TRUE)
shapiro.test(residuals(GNS.5))
bptest.Sarlm(GNS.5)
moran.test(residuals(GNS.5),esf.influencia)

print(summary(SACSAR.5 <- sacsarlm(as.formula(f.rez.better.W5), data = df_fin, listw = esf.influencia, Durbin = FALSE),
              Nagelkerke = TRUE),signif.stars=TRUE)
shapiro.test(residuals(SACSAR.5))
bptest.Sarlm(SACSAR.5)
moran.test(residuals(SACSAR.5),esf.influencia)

print(summary(SDM.5 <- lagsarlm(as.formula(f.rez.better.W5), data = df_fin, listw = esf.influencia, Durbin = FALSE),
              Nagelkerke = TRUE),signif.stars=TRUE)
shapiro.test(residuals(SDM.5))
bptest.Sarlm(SDM.5)
moran.test(residuals(SDM.5),esf.influencia)

print(summary(SDEM.5 <- errorsarlm(as.formula(f.rez.better.W5), data = df_fin, listw = esf.influencia, Durbin = FALSE),
              Nagelkerke = TRUE),signif.stars=TRUE)
shapiro.test(residuals(SDEM.5))
bptest.Sarlm(SDEM.5)
moran.test(residuals(SDEM.5),esf.influencia)

print(summary(SEM.5 <- errorsarlm(as.formula(f.rez.better.W5), data = df_fin, listw = esf.influencia, Durbin = FALSE),
        Nagelkerke = TRUE),signif.stars=TRUE)
shapiro.test(residuals(SEM.5))
bptest.Sarlm(SEM.5)
moran.test(residuals(SEM.5),esf.influencia)

# Análisis modelos
library(texreg)
mods.sp <- list("SDM 1" = SDM.1,"SDM 5" = SDM.5,"SDEM 1" = SDEM.1,
                "SDEM 5" = SDEM.5,"SEM 1" = SEM.1,"SEM 5" = SEM.5,
                "SLX 1" = SLX.1)
texreg(
  mods.sp,
  stars = c(0.01, 0.05, 0.1),  # niveles de significancia
  digits = 3,
  longtable = TRUE
)
# Análisis de impactos 
summary(impacts(GNS.1, listw = esf.influencia, R=200), zstats=TRUE, short = TRUE)
summary(impacts(GNS.5, listw = esf.influencia, R=200), zstats=TRUE, short = TRUE)
summary(impacts(SACSAR.1, listw = esf.influencia, R=200), zstats=TRUE, short = TRUE)
summary(impacts(SACSAR.5, listw = esf.influencia, R=200), zstats=TRUE, short = TRUE)
summary(impacts(SDM.1, listw = esf.influencia, R=200), zstats=TRUE, short = TRUE)
summary(impacts(SDM.5, listw = esf.influencia, R=200), zstats=TRUE, short = TRUE)
summary(impacts(SLX.1, listw = esf.influencia, R=200), zstats=TRUE, short = TRUE)
