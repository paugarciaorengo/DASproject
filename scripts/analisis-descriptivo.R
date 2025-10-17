
install.packages("corrplot")
install.packages("sf")
install.packages("leaflet")

library(dplyr)
library(ggplot2)
library(lubridate)

library(corrplot)
library(sf)
library(leaflet)

# Cargar el archivo CSV con cabeceras
accidents_clean_data <- read.csv("data/processed/accidentes_madrid_con_weather.csv", 
                         header = TRUE, 
                         sep = ",")

# ------------------------------------------------------------------------------
# 1 ANÁLISIS GENERAL DEL CONJUNTO DE DATOS
# ------------------------------------------------------------------------------

# Objetivo: conocer la estructura, la calidad de los datos y las variables disponibles.
# Indicadores interesantes:
#   Número total de accidentes (registros).
#   Periodo temporal cubierto.
#   Porcentaje de valores faltantes o “No se registró”.
#   Tipos de accidente más frecuentes.
#   Distribución por sexo, rango de edad, tipo de vehículo.
# 
# Visualizaciones:
#   Barras o pastel de tipo_accidente.
#   Barras apiladas por sexo y tipo_persona.
#   Gráfico de barras de distrito (accidentes por distrito).

# Resumen general
str(accidents_clean_data)
summary(accidents_clean_data)
sapply(accidents_clean_data, function(x) sum(is.na(x)))


# ------- Accidentes por tipo
ggplot(accidents_clean_data, aes(x = tipo_accidente)) + 
  geom_bar(fill = "steelblue") + 
  coord_flip()


# ------- Accidentes por distrito
accidentes_por_distrito <- accidents_clean_data %>%
  group_by(distrito) %>%
  summarise(total_accidentes = n()) %>%
  arrange(desc(total_accidentes))

ggplot(accidentes_por_distrito, aes(x = reorder(distrito, total_accidentes), y = total_accidentes)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Accidentes de tráfico por distrito (Madrid 2019–2023)",
    x = "Distrito",
    y = "Número de accidentes"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# 2 ANÁLISIS METEOROLÓGICO
# ------------------------------------------------------------------------------

# Objetivo: entender cómo influyen las condiciones del tiempo en los accidentes.
# Qué hacer:
#   Distribución de accidentes por estado_meteorol_gico.
#   Promedios de wx_temperature, wx_wind_speed, wx_precipitation.
#   Relación entre clima y tipo de accidente.
# 
# Visualizaciones sugeridas:
#   Gráfico de barras: accidentes vs. tipo de clima.
#   Boxplots: temperatura o velocidad del viento según tipo de accidente.
#   Heatmap o scatterplot: temperatura vs precipitación, coloreado por tipo_accidente.

# ------- Accidentes por estado meteorológico
table(accidents_clean_data$estado_meteorol_gico)
ggplot(accidents_clean_data, aes(x = estado_meteorol_gico)) + 
  geom_bar(fill = "skyblue") + 
  coord_flip()

# ------------------------------------------------------------------------------
# 3 ANÁLISIS TEMPORAL
# ------------------------------------------------------------------------------

# Objetivo: encontrar patrones a lo largo del tiempo.
# Qué hacer:
#   Accidentes por año, mes, día de la semana, hora.
#   Tendencias y estacionalidad (más accidentes en invierno, verano, etc.).
#   
# Visualizaciones:
#   Serie temporal de accidentes (por mes o trimestre).
#   Distribución por hora del día.
#   Accidentes por día de la semana (lunes–domingo).

# ------- Accidentes por mes

accidents_clean_data$fecha <- as.Date(accidents_clean_data$time)
accidents_clean_data$anio <- year(accidents_clean_data$fecha)
accidents_clean_data$mes <- month(accidents_clean_data$fecha, label = TRUE)
accidents_clean_data$hora <- hour(accidents_clean_data$time)

ggplot(accidents_clean_data, aes(x = mes)) + 
  geom_bar(fill = "orange") + 
  labs(title = "Accidentes por mes")

# ------- Accidentes por año
accidentes_por_anio <- accidents_clean_data %>%
  group_by(anio) %>%
  summarise(total_accidentes = n())

ggplot(accidentes_por_anio, aes(x = factor(anio), y = total_accidentes)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = total_accidentes), vjust = -0.5, size = 4) +
  labs(
    title = "Número de accidentes por año en Madrid (2019–2023)",
    x = "Año",
    y = "Número de accidentes"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# 4 ANÁLISIS ESPACIAL
# ------------------------------------------------------------------------------

# Objetivo: descubrir zonas o distritos con mayor siniestralidad.
# Qué hacer:
#   Accidentes por distrito.
#   Mapas de calor (usando coordenada_x_utm, coordenada_y_utm).
#   Relación entre clima y ubicación (¿dónde llueve más y hay más choques?).
# 
# Visualizaciones:
#   Mapas con puntos de accidentes.
#   Mapas de calor por distrito.
#   Accidentes con lluvia vs sin lluvia, representados en distintos colores.

# ------- Accidentes en el mapa

accidents_clean_data_filtered <- accidents_clean_data_filtered %>%
  filter(!is.na(coordenada_x_utm) & !is.na(coordenada_y_utm)) %>%
  mutate(
    coordenada_x_utm = as.numeric(coordenada_x_utm) / 1000,  # <-- corregimos la escala
    coordenada_y_utm = as.numeric(coordenada_y_utm) / 1000
  )

# Convertir coordenadas a sf (sistema espacial)
datos_sf <- st_as_sf(accidents_clean_data_filtered,
                     coords = c("coordenada_x_utm", "coordenada_y_utm"),
                     crs = 25830)

ggplot(datos_sf) + 
  geom_sf(alpha = 0.4, color = "red") + 
  labs(title = "Distribución espacial de los accidentes en Madrid")

#-------- Mapa Interactivo

# Cargar el shapefile de los distritos
distritos <- st_read("data/raw/distritos/distritos.shp")

# Comprobar la proyección del shapefile
st_crs(distritos)

distritos_wgs84 <- st_transform(distritos, crs = 4326)

# Crear paleta de colores para los distritos
n_distritos <- length(unique(distritos_wgs84$NOMBRE))
pal <- colorFactor(
  palette = rainbow(n_distritos),  # genera colores suficientes para todos
  domain = distritos_wgs84$NOMBRE
)

leaflet(distritos_wgs84) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(NOMBRE),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = ~paste("Distrito: ", NOMBRE)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~NOMBRE,
    title = "Distritos de Madrid",
    opacity = 1
  )



# Convertir tus datos de accidentes a sf y al mismo CRS
accidentes_sf <- st_as_sf(accidents_clean_data_filtered,
                          coords = c("coordenada_x_utm", "coordenada_y_utm"),
                          crs = 25830)   # UTM zona 30N

accidentes_sf <- st_transform(accidentes_sf, crs = 4326)


# Mapa final con distritos + accidentes
leaflet() %>%
  addTiles() %>%
  addPolygons(data = distritos_wgs84,
              fillColor = ~pal(NOMBRE),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = ~paste("Distrito: ", NOMBRE)) %>%
  addCircleMarkers(data = accidentes_sf,
                   radius = 3,
                   color = "red",
                   fillOpacity = 0.7,
                   popup = ~paste("Tipo accidente: ", tipo_accidente,
                                  "<br>Distrito: ", distrito,
                                  "<br>Clima: ", estado_meteorol_gico,
                                  "<br>Fecha: ", time)) %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = distritos_wgs84$NOMBRE,
            title = "Distritos de Madrid",
            opacity = 1)


# ------------------------------------------------------------------------------
# 5 ANÁLISIS DE FACTORES HUMANOS
# ------------------------------------------------------------------------------

# Objetivo: estudiar las características de las personas implicadas.
# Qué hacer:
#   Distribución de accidentes por rango de edad y sexo.
#   Comparar tipo de persona (Conductor vs Pasajero).
#   Cruce entre positiva_alcohol, positiva_droga y tipo de accidente.
# 
# Visualizaciones:
#   Barras apiladas por edad y sexo.
#   Gráfico de barras para alcohol/drogas positivos.
#   Comparación entre conductores y pasajeros.

# ------- Accidentes por Rango de Edad y Sexo
ggplot(accidents_clean_data, aes(x = rango_edad, fill = sexo)) + 
  geom_bar(position = "dodge") +
  coord_flip()

# ------------------------------------------------------------------------------
#6 ANÁLISIS MULTIVARIADO (CORRELACIONES)
# ------------------------------------------------------------------------------

# Objetivo: ver relaciones entre variables meteorológicas y tipos de accidente.
# Qué hacer:
#   Calcular correlaciones entre wx_temperature, wx_wind_speed, wx_precipitation y variables como lesividad o tipo_accidente.
#   Tablas cruzadas y tests de Chi-cuadrado para variables categóricas.

# ------- Correlaciones

numericas <- accidents_clean_data[, c("wx_temperature", "wx_wind_speed", "wx_precipitation")]
cor_matrix <- cor(numericas, use = "complete.obs")
corrplot(cor_matrix, method = "color")


# ------------------------------------------------------------------------------
#📊 Conclusión: ruta sugerida en R
# ------------------------------------------------------------------------------

1️⃣# Carga y limpieza de datos (missing, tipos de datos, etc.)
2️⃣# Análisis univariante (cada variable por separado)
3️⃣# Análisis bivariante (relaciones entre clima y accidentes)
4️⃣# Análisis temporal y espacial
5️⃣# Conclusiones y visualizaciones clave


