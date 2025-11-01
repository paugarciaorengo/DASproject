
install.packages("corrplot")
install.packages("sf")
install.packages("leaflet")
install.packages(c("sf", "dplyr", "ggplot2", "scales"))

library(dplyr)
library(ggplot2)
library(lubridate)

library(corrplot)
library(sf)
library(leaflet)

library(stringr)
library(tidyr)
# Instalar paquetes si no los tienes
library(scales)
library(cluster)
library(factoextra)



# Cargar el archivo CSV con cabeceras
accidents_clean_data <- read.csv("data/processed/accidentes_madrid_con_weather.csv", 
                         header = TRUE, 
                         sep = ",")

names(accidents_clean_data)
str(accidents_clean_data)
unique(accidents_clean_data$lesividad)

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


#================IDENTIFICANDO NA===============================================

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

# Valores únicos por columna categórica
sapply(accidents_clean_data[, sapply(accidents_clean_data, is.character)], function(x) length(unique(x)))


# CREAR ATRIBUTOS DERIVADOS ====================================================


# 1. Extraer hora, día de la semana, mes, estación

accidents_clean_data$fecha <- as.Date(accidents_clean_data$time)
accidents_clean_data$anio <- year(accidents_clean_data$fecha)
accidents_clean_data$hora <- hour(accidents_clean_data$time)
accidents_clean_data$dia_semana <- wday(accidents_clean_data$time, label = TRUE, abbr = FALSE)
accidents_clean_data$mes <- month(accidents_clean_data$time, label = TRUE)
accidents_clean_data$estacion <- case_when(
  accidents_clean_data$mes %in% c("diciembre", "enero", "febrero") ~ "Invierno",
  accidents_clean_data$mes %in% c("marzo", "abril", "mayo") ~ "Primavera",
  accidents_clean_data$mes %in% c("junio", "julio", "agosto") ~ "Verano",
  accidents_clean_data$mes %in% c("septiembre", "octubre", "noviembre") ~ "Otoño"
)

# 2. Crear franja horaria
accidents_clean_data$franja_horaria <- case_when(
  accidents_clean_data$hora >= 6 & accidents_clean_data$hora < 12 ~ "Mañana",
  accidents_clean_data$hora >= 12 & accidents_clean_data$hora < 18 ~ "Tarde",
  accidents_clean_data$hora >= 18 & accidents_clean_data$hora < 24 ~ "Noche",
  TRUE ~ "Madrugada"
)

# 3. Fin de semana vs entre semana
accidents_clean_data$es_fin_de_semana <- ifelse(accidents_clean_data$dia_semana %in% c("sábado", "domingo"), "Sí", "No")

# 4. Categorizar temperatura
accidents_clean_data$categoria_temp <- case_when(
  accidents_clean_data$wx_temperature < 5 ~ "Frío (< 5°C)",
  accidents_clean_data$wx_temperature >= 5 & accidents_clean_data$wx_temperature < 15 ~ "Templado (5-15°C)",
  accidents_clean_data$wx_temperature >= 15 & accidents_clean_data$wx_temperature < 25 ~ "Cálido (15-25°C)",
  accidents_clean_data$wx_temperature >= 25 ~ "Caluroso (> 25°C)"
)

# 5. ¿Hubo precipitación?
accidents_clean_data$hubo_lluvia <- ifelse(accidents_clean_data$wx_precipitation > 0, "Sí", "No")

# 6. Gravedad binaria de lesividad
accidents_clean_data <- accidents_clean_data %>%
  mutate(gravedad_binaria = case_when(
    # LEVES
    grepl("sólo en el lugar|Sin asistencia|ambulatoria", lesividad, ignore.case = TRUE) ~ "Leve",
    
    # GRAVES
    grepl("Ingreso|urgencias|centro de salud|mutua|Fallecido", lesividad, ignore.case = TRUE) ~ "Grave",
    
    # DESCONOCIDO
    lesividad == "Se desconoce" | is.na(lesividad) ~ "Desconocido",
    
    # Por si acaso queda algo
    TRUE ~ "Otro"
  ))

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

accidents_clean_data_sin_na = accidents_clean_data%>%
  filter(!is.na(tipo_accidente))

ggplot(accidents_clean_data_sin_na, aes(x = tipo_accidente, fill = gravedad_binaria)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Gravedad según tipo de accidente",
       x = "Tipo de accidente", y = "Proporción") +
  theme_minimal()


# --Accidentes por día de la semana

ggplot(accidents_clean_data, aes(x = dia_semana)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Accidentes por día de la semana",
       x = "Día", y = "Número de accidentes") +
  theme_minimal()

#Accidentes por mes/estación

ggplot(accidents_clean_data, aes(x = mes)) +
  geom_bar(fill = "forestgreen") +
  labs(title = "Accidentes por mes",
       x = "Mes", y = "Número de accidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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

accidents_clean_data_temp = accidents_clean_data %>%
  filter(!is.na(categoria_temp))

#Accidentes vs Temperatura
ggplot(accidents_clean_data_temp, aes(x = categoria_temp)) +
  geom_bar(fill = "orange") +
  labs(title = "Accidentes según categoría de temperatura",
       x = "Temperatura", y = "Número de accidentes") +
  theme_minimal()


#--Accidentes con/sin lluvia

accidents_clean_data_lluvia = accidents_clean_data %>%
              filter(!is.na(hubo_lluvia))

ggplot(accidents_clean_data_lluvia, aes(x = hubo_lluvia)) +
  geom_bar(fill = "dodgerblue") +
  labs(title = "Accidentes según presencia de lluvia",
       x = "¿Llovió?", y = "Número de accidentes") +
  theme_minimal()


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


# Grafico por Hora

ggplot(accidents_clean_data, aes(x = hora )) +
  geom_bar(fill = "#DE2D27") +
  labs(title = "Distribución de accidentes por hora del día",
       x = "Hora", y = "Número de accidentes") +
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

# Convertir tus datos de accidentes a sf y al mismo CRS
accidentes_sf <- st_as_sf(accidents_clean_data_filtered,
                          coords = c("coordenada_x_utm", "coordenada_y_utm"),
                          crs = 25830)   # UTM zona 30N

accidentes_sf <- st_transform(accidentes_sf, crs = 4326)

#Mapa de calor

# 1. Leer shapefile de distritos
madrid_sf <- st_read("data/raw/distritos/DISTRITOS.shp")

# 2. Asegurarse de que los datos de accidentes estén en el mismo CRS (proyección)
#    Suponiendo que el shapefile está en EPSG:25830 (UTM zona 30N, típico de Madrid)
#    y que tus coordenadas están en ese mismo sistema (coordenada_x_utm, coordenada_y_utm)
#    si no, puedes transformar así:
# madrid_sf <- st_transform(madrid_sf, crs = 25830)

# 3. Mapa combinado: base + mapa de calor
ggplot() +
  # Mapa base de distritos
  geom_sf(data = madrid_sf, fill = NA, color = "gray40", size = 0.3) +
  
  # Mapa de calor
  stat_density_2d(
    data = accidents_clean_data_filtered,
    aes(x = coordenada_x_utm, y = coordenada_y_utm, fill = ..level..),
    geom = "polygon",
    alpha = 0.6
  ) +
  
  # Paleta de color
  scale_fill_gradient(low = "yellow", high = "red", name = "Densidad") +
  
  # Títulos y etiquetas
  labs(
    title = "Mapa de calor de accidentes en Madrid",
    subtitle = "Superpuesto sobre distritos (2019-2023)",
    x = "Coordenada X (UTM)",
    y = "Coordenada Y (UTM)",
    caption = "Fuente: Datos Abiertos Madrid"
  ) +
  
  # Tema limpio y mapa ajustado
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  )



# 1. Leer shapefile de distritos
# ya se hizo anteriormente

# 2. Normalizar nombres de distrito para evitar problemas de match
madrid_sf$NOMBRE <- toupper(trimws(madrid_sf$NOMBRE))
accidents_clean_data_filtered$distrito <- toupper(trimws(accidents_clean_data_filtered$distrito))

# 3. Calcular centroides de cada distrito
centroides_sf <- st_point_on_surface(madrid_sf)
coords <- st_coordinates(centroides_sf)

# 4. Contar accidentes por distrito
accidentes_distrito <- accidents_clean_data_filtered %>%
  count(distrito, name = "n_accidentes")

# 5. Preparar datos para el mapa
datos_mapa <- data.frame(
  distrito = madrid_sf$NOMBRE,
  x = coords[,1],
  y = coords[,2]
) %>%
  left_join(accidentes_distrito, by = c("distrito" = "distrito")) %>%
  mutate(n_accidentes = ifelse(is.na(n_accidentes), 0, n_accidentes))  # reemplaza NA por 0

# 6. Crear mapa con burbujas proporcionales y color
ggplot() +
  # Mapa base de distritos
  geom_sf(data = madrid_sf, fill = "gray95", color = "gray50", size = 0.3) +
  
  # Burbujas proporcionales al número de accidentes
  geom_point(data = datos_mapa,
             aes(x = x, y = y, size = n_accidentes, color = n_accidentes),
             alpha = 0.7) +
  
  # Escala de color (amarillo → rojo) y tamaño de burbuja
  scale_color_gradient(low = "green", high = "#de2d27", name = "Accidentes") +
  scale_size_continuous(range = c(3, 20), name = "Accidentes", labels = scales::comma) +
  
  # Etiquetas de distrito
  geom_text(data = datos_mapa,
            aes(x = x, y = y, label = distrito),
            size = 2.5, color = "black", vjust = 1.2) +
  
  # Títulos y etiquetas
  labs(title = "Cantidad de accidentes por distrito",
       subtitle = "Madrid 2019–2023",
       caption = "Fuente: Datos Abiertos Madrid") +
  
  # Tema limpio
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  )



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



#¿Cuándo hacer clustering?
 # SÍ, deberías hacer clustering para identificar perfiles de accidentes similares.
#Paso 3.1: Clustering de zonas geográficas


# Preparar datos (solo coordenadas)
datos_geo <- accidents_clean_data_filtered[, c("coordenada_x_utm", "coordenada_y_utm")]

# K-means con k=5 (5 zonas)
set.seed(123)
km_result <- kmeans(datos_geo, centers = 5, nstart = 25)

# Añadir cluster al dataset
accidents_clean_data_filtered$cluster_zona <- as.factor(km_result$cluster)

# Visualizar
fviz_cluster(km_result, data = datos_geo,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Clustering de zonas con accidentes")