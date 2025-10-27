
install.packages("corrplot")
install.packages("sf")
install.packages("leaflet")

library(dplyr)
library(ggplot2)
library(lubridate)

library(corrplot)
library(sf)
library(leaflet)

library(stringr)
library(tidyr)


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


#================IDENTIFICANDO NA=========================================

# ✅ 1️⃣ Crear una copia del dataset original
accidents_clean_data <- accidents_clean_data %>%
  # Eliminar espacios en blanco al inicio y al final en todas las columnas de texto
  mutate(across(where(is.character), ~str_trim(.))) %>%
  
  # Reemplazar valores problemáticos por NA en todas las columnas de texto
  mutate(across(where(is.character),
                ~case_when(
                  . %in% c("", "NA", "N/A", "No se registró", "No se registro",
                           "null", "NULL", "Sin dato", "Desconocido") ~ NA_character_,
                  TRUE ~ .
                )))


# Calcular porcentaje de valores faltantes por columna
missing_summary <- accidents_clean_data %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "columna", values_to = "porcentaje_na") %>%
  arrange(desc(porcentaje_na))

# Ver los resultados
print(missing_summary)


#===============================================================================


# Resumen general
str(accidents_clean_data)
summary(accidents_clean_data)
sapply(accidents_clean_data, function(x) sum(is.na(x)))
head(accidents_clean_data)
colnames(accidents_clean_data)

# ------- Accidentes por tipo

accidents_clean_data %>%
  filter(!is.na(tipo_accidente)) %>%
  count(tipo_accidente) %>%           # crea columna n con el conteo
  mutate(tipo_accidente = reorder(tipo_accidente, n)) %>%
  ggplot(aes(x = tipo_accidente, y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.1) +  # agrega el número de accidentes
  coord_flip() +
  labs(
    title = "Distribución de tipos de accidente en Madrid (2019–2023)",
    x = "Tipo de accidente",
    y = "Número de accidentes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

# ------- Accidentes por distrito
accidentes_por_distrito <- accidents_clean_data %>%
  filter(!is.na(distrito)) %>%
  group_by(distrito) %>%
  summarise(total_accidentes = n()) %>%
  arrange(desc(total_accidentes))


ggplot(accidentes_por_distrito, aes(x = reorder(distrito, total_accidentes), y = total_accidentes, fill=distrito)) +
  geom_col() +
  geom_text(aes(label = total_accidentes), vjust = 0.5, hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Accidentes de tráfico por distrito (Madrid 2019–2023)",
    x = "Distrito",
    y = "Número de accidentes"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"  # opcional si no quieres la leyenda
  )



# ------- Accidentes por tipo de vehículo
accidentes_por_tipo_vehiculo <- accidents_clean_data %>%
  filter(!is.na(tipo_vehiculo)) %>%
  group_by(tipo_vehiculo) %>%
  summarise(total_accidentes = n()) %>%
  arrange(desc(total_accidentes))

ggplot(accidentes_por_tipo_vehiculo, aes(x = reorder(tipo_vehiculo, total_accidentes), y = total_accidentes)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Accidentes de tráfico por tipo de vehículo (Madrid 2019–2023)",
    x = "Tipo de vehículo",
    y = "Número de accidentes"
  ) +
  theme_minimal()


# ------- Accidentes por el consumo de alcohol

accidentes_alcohol <- accidents_clean_data %>%
  filter(!is.na(positiva_alcohol)) %>%     # eliminar NA
  group_by(positiva_alcohol) %>%
  summarise(total_accidentes = n()) %>%
  arrange(desc(total_accidentes))

#cantidad
ggplot(accidentes_alcohol, aes(x = factor(positiva_alcohol, labels = c("Negativo", "Positivo")), 
                               y = total_accidentes)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = total_accidentes), vjust = -0.5) +
  labs(
    title = "Accidentes según resultado de alcohol (Madrid 2019–2023)",
    x = "Resultado de alcohol",
    y = "Número de accidentes"
  ) +
  theme_minimal()

accidentes_alcohol <- accidentes_alcohol %>%
  mutate(porcentaje = total_accidentes / sum(total_accidentes) * 100)

#porcentaje
ggplot(accidentes_alcohol, aes(x = factor(positiva_alcohol, labels = c("Negativo", "Positivo")), 
                               y = porcentaje)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.5) +
  labs(
    title = "Porcentaje de accidentes según resultado de alcohol (Madrid 2019–2023)",
    x = "Resultado de alcohol",
    y = "Porcentaje de accidentes"
  ) +
  theme_minimal()


# ------- Accidentes por el uso de drogas

accidentes_droga <- accidents_clean_data %>%
  filter(!is.na(positiva_droga)) %>%       # eliminar NA
  group_by(positiva_droga) %>%
  summarise(total_accidentes = n()) %>%
  arrange(desc(total_accidentes))

#cantidad
ggplot(accidentes_droga, aes(x = factor(positiva_droga, labels = c("Negativo", "Positivo")), 
                             y = total_accidentes)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = total_accidentes), vjust = -0.5) +
  labs(
    title = "Accidentes según resultado de drogas (Madrid 2019–2023)",
    x = "Resultado de drogas",
    y = "Número de accidentes"
  ) +
  theme_minimal()

accidentes_droga <- accidentes_droga %>%
  mutate(porcentaje = total_accidentes / sum(total_accidentes) * 100)

#porcentaje
ggplot(accidentes_droga, aes(x = factor(positiva_droga, labels = c("Negativo", "Positivo")), 
                             y = porcentaje)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.5) +
  labs(
    title = "Porcentaje de accidentes según resultado de drogas",
    x = "Resultado de drogas",
    y = "Porcentaje de accidentes"
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

# Contar accidentes por estado meteorológico y ordenar
accidentes_clima <- accidents_clean_data %>%
  filter(!is.na(estado_meteorol_gico)) %>%
  count(estado_meteorol_gico) %>%
  arrange(desc(n))

# Gráfico mejorado
ggplot(accidentes_clima, aes(x = reorder(estado_meteorol_gico, n), y = n)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = n), hjust = -0.1) +  # número de accidentes al lado de la barra
  coord_flip() +
  labs(
    title = "Accidentes según estado meteorológico (Madrid 2019–2023)",
    x = "Estado meteorológico",
    y = "Número de accidentes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )

#-- La media

accidentes_clima <- accidents_clean_data %>%
  filter(!is.na(estado_meteorol_gico)) %>%
  mutate(fecha = as.Date(time)) %>%
  group_by(estado_meteorol_gico, fecha) %>%
  summarise(accidentes_dia = n(), .groups = 'drop') %>%
  group_by(estado_meteorol_gico) %>%
  summarise(media = mean(accidentes_dia)) %>%
  arrange(desc(media))

# Gráfico
ggplot(accidentes_clima, aes(x = reorder(estado_meteorol_gico, media), y = media)) +
  geom_col(fill = "grey") +
  geom_text(aes(label = round(media, 1)), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Media de accidentes por día según estado meteorológico (Madrid 2019–2023)",
    x = "Estado meteorológico",
    y = "Media de accidentes por día"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )

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


accidents_clean_data_filtered <- accidents_clean_data %>%
  filter(!is.na(coordenada_x_utm) & !is.na(coordenada_y_utm)) %>%
  mutate(
    coordenada_x_utm = as.numeric(coordenada_x_utm) / 1000,
    coordenada_y_utm = as.numeric(coordenada_y_utm) / 1000
  ) %>%
  # Filtrar solo coordenadas válidas para Madrid en UTM
  # Madrid está aproximadamente en: X: 400-450, Y: 4460-4490 (UTM zona 30N)
  filter(
    coordenada_x_utm >= 430000 & coordenada_x_utm <= 450000,
    coordenada_y_utm >= 4465000 & coordenada_y_utm <= 4485000
  )



# Convertir coordenadas a sf (sistema espacial)
datos_sf <- st_as_sf(accidents_clean_data_filtered,
                     coords = c("coordenada_x_utm", "coordenada_y_utm"),
                     crs = 25830)

ggplot(accidentes_sf) + 
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
  coord_flip() +
  labs(
    title = "Distribución de accidentes por rango de edad y sexo",
    x = "Rango de edad",
    y = "Número de accidentes",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )
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


library(sf)
library(tidyverse)
library(plotly)


# Convertir UTM a lat/long

accidents_clean_data_filtered <- accidents_clean_data %>%
  filter(!is.na(coordenada_x_utm) & !is.na(coordenada_y_utm)) %>%
  mutate(
    coordenada_x_utm = as.numeric(coordenada_x_utm) / 1000,  # <-- corregimos la escala
    coordenada_y_utm = as.numeric(coordenada_y_utm) / 1000
  )%>%
  # Filtrar solo coordenadas válidas para Madrid en UTM
  # Madrid está aproximadamente en: X: 400-450, Y: 4460-4490 (UTM zona 30N)
  filter(
    coordenada_x_utm >= 430000 & coordenada_x_utm <= 450000,
    coordenada_y_utm >= 4465000 & coordenada_y_utm <= 4485000
  )

accidentes_geo <- st_as_sf(accidents_clean_data_filtered,
                          coords = c("coordenada_x_utm", "coordenada_y_utm"),
                          crs = 25830)   # UTM zona 30N

accidentes_geo <- st_transform(accidentes_sf, crs = 4326)



# Extraer coordenadas
coords <- st_coordinates(accidentes_geo)
accidentes_geo <- accidentes_geo %>%
  mutate(long = coords[,1],
         lat = coords[,2]) %>%
  st_drop_geometry()

# Extraer año
accidentes_geo <- accidentes_geo %>%
  mutate(year = lubridate::year(lubridate::ymd_hm(time)))


cat("Total de accidentes:", nrow(accidentes_geo), "\n")
cat("Años disponibles:\n")
print(table(accidentes_geo$year))

library(tidyverse)
library(sf)
library(lubridate)
library(plotly)
library(ggthemes)
library(maps)

# 4. AGREGAR DATOS PARA ANIMACIÓN
# ========================================
# Un punto por accidente único (agrupando personas involucradas)
accidentes_animacion <- accidentes_geo %>%
  group_by(localizacion, long, lat, year, distrito, tipo_accidente) %>%
  summarise(n_personas = n(), .groups = "drop")

accidentes_animacion

# 5. OBTENER MAPA BASE
# ========================================
mapa_espana <- map_data("world", region = "Spain")



# Preparar datos
accidentes_animacion <- accidentes_geo %>%
  group_by(long, lat, year, distrito, tipo_accidente) %>%
  summarise(n_personas = n(), .groups = "drop")

# Obtener mapa
#mapa_espana <- map_data("world", region = "Spain")
mapa_espana <- map_data("world", region = "Spain") %>%
  filter(
    long >= -4.0 & long <= -3.4,
    lat >= 40.3 & lat <= 40.6
  )


distritos_df <- distritos_wgs84 %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(long = X, lat = Y) %>%
  mutate(group = L2)  # L2 es el identificador del polígono


# Crear mapa animado
mapa_animado <- distritos_df %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "grey20",
               color = "white",
               size = 0.01) +
  geom_point(data = accidentes_animacion,
             aes(x = long,
                 y = lat,
                 frame = year,
                 size = n_personas,
                 color = distrito),
             alpha = 0.7) +
  labs(title = "Accidentes de Tráfico en Madrid\n2019-2023",
       caption = "Datos: Ayuntamiento de Madrid") +
  theme_map() +
  scale_size_continuous(guide = FALSE, range = c(1, 8)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# Animar
ggplotly(mapa_animado) %>% 
  animation_slider(currentvalue = list(prefix = "Año ", font = list(color = "orange")))

