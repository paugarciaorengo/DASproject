# ================================================================
# 00_pipeline_accidentes_meteo.R
# Proyecto DAS (UPV): Accidentes Madrid + Meteorología (2019-2023 / 2019-2022)
# Autor: Pau García Orengo / equipo
# Descripción: Script único y reproducible para:
#  - Cargar CSV de accidentes y meteorología.
#  - Limpiar/normalizar (encoding, cabeceras, hora con AM/PM).
#  - Integrar por fecha-hora (redondeada a la hora).
#  - Crear variables derivadas (is_rainy, temp_cat).
#  - Validar y guardar dataset procesado.
#  - (Opcional) guardar 3 figuras rápidas de EDA.
# Uso: source("scripts/00_pipeline_accidentes_meteo.R", echo = TRUE)
# ================================================================

message(">>> Inicio pipeline: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# -----------------------------
# 0) Parámetros de entrada/salida
# -----------------------------
RAW_DIR  <- "data/raw"
OUT_DIR  <- "data/processed"
ACC_FILE <- file.path(RAW_DIR, "datos_madrid.csv")
WX_FILE  <- file.path(RAW_DIR, "weather_madrid_2019-2022.csv")
OUT_CSV  <- file.path(OUT_DIR, "accidentes_madrid_weather.csv")

DO_PLOTS <- TRUE  # Cambia a FALSE si no quieres generar figuras PNG

# -----------------------------
# 1) Paquetes
# -----------------------------
req <- c("data.table","readr","janitor","lubridate","ggplot2")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)

library(data.table)
library(readr)
library(janitor)
library(lubridate)
library(ggplot2)

data.table::setDTthreads(1)

# -----------------------------
# 2) Comprobar archivos
# -----------------------------
if (!file.exists(ACC_FILE)) stop("No existe: ", ACC_FILE)
if (!file.exists(WX_FILE))  stop("No existe: ", WX_FILE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Archivos detectados OK.")
message("  Accidentes:  ", ACC_FILE)
message("  Meteorología:", WX_FILE)

# -----------------------------
# 3) Carga robusta
# -----------------------------
message("Leyendo ACCIDENTES...")
acc <- readr::read_csv(
  ACC_FILE,
  locale   = locale(encoding = "Latin1"),
  progress = TRUE,
  trim_ws  = TRUE
) |>
  janitor::clean_names()

message("Leyendo METEOROLOGÍA...")
wx <- data.table::fread(
  WX_FILE,
  encoding     = "UTF-8",
  showProgress = TRUE
) |>
  janitor::clean_names()

if ("v1" %in% names(wx)) wx[, v1 := NULL]
if (any(is.na(names(acc)))) {
  acc <- acc[, ..names(acc)[!is.na(names(acc))]]
}

message("Dimensiones leídas:")
message("  acc: ", paste(dim(acc), collapse = " x "))
message("  wx : ", paste(dim(wx),  collapse = " x "))

# -----------------------------
# 4) Normalización de hora/fecha
# -----------------------------
hora_utf  <- iconv(acc$hora, from = "", to = "UTF-8")
hora_norm <- tolower(hora_utf)
hora_norm <- gsub("\\s+", " ", hora_norm)
hora_norm <- gsub("\\.", "", hora_norm)
hora_norm <- gsub("a m|a\\s?m", "AM", hora_norm)
hora_norm <- gsub("p m|p\\s?m", "PM", hora_norm)
hora_norm <- toupper(hora_norm)
acc$hora_limpia <- hora_norm

acc$fecha_hora_raw <- paste(acc$fecha, acc$hora_limpia)
acc$fecha_hora <- suppressWarnings(parse_date_time(
  acc$fecha_hora_raw,
  orders = c("dmy HMS p", "dmy HM p", "dmy HMS", "dmy HM"),
  tz = "Europe/Madrid"
))
acc$fecha_hora <- floor_date(acc$fecha_hora, unit = "hour")

wx$fecha_hora <- suppressWarnings(ymd_hms(wx$time, tz = "Europe/Madrid"))
wx$fecha_hora <- floor_date(wx$fecha_hora, unit = "hour")

# -----------------------------
# 5) Integración por hora (left join: accidentes <- meteo)
# -----------------------------
setDT(acc); setDT(wx)
setkey(acc, fecha_hora)
setkey(wx,  fecha_hora)
datos <- wx[acc]

# -----------------------------
# 6) Feature engineering
# -----------------------------
if ("precipitation" %in% names(datos)) setnames(datos, "precipitation", "precipitacion")
if ("temperature"   %in% names(datos)) setnames(datos, "temperature",   "temperatura")

datos[, is_rainy := fifelse(!is.na(precipitacion) & precipitacion > 0, 1L, 0L)]
datos[, temp_cat := fcase(
  !is.na(temperatura) & temperatura < 10,                      "frio",
  !is.na(temperatura) & temperatura >= 10 & temperatura < 25,  "templado",
  !is.na(temperatura) & temperatura >= 25,                     "caluroso",
  default = NA_character_
)]

# -----------------------------
# 7) Validación rápida
# -----------------------------
message("Validación rápida:")
message("  dim(datos): ", paste(dim(datos), collapse = " x "))
message("  Temperatura:"); print(summary(datos$temperatura))
message("  Precipitación:"); print(summary(datos$precipitacion))
message("  is_rainy:"); print(table(datos$is_rainy, useNA = "ifany"))
message("  temp_cat:"); print(table(datos$temp_cat, useNA = "ifany"))

# -----------------------------
# 8) Guardar dataset completo (2019–2023)
# -----------------------------
data.table::fwrite(datos, OUT_CSV)
message("Guardado dataset completo: ", OUT_CSV)

# -----------------------------
# 9) Filtro analítico 2019–2022
# -----------------------------
message("Creando subset 2019–2022 con datos meteorológicos disponibles...")
datos_19_22 <- datos[
  year(fecha_hora) >= 2019 & year(fecha_hora) <= 2022 & !is.na(temperatura)
]

OUT_CSV_19_22 <- file.path(OUT_DIR, "accidentes_madrid_weather_2019_2022.csv")
data.table::fwrite(datos_19_22, OUT_CSV_19_22)
message("Guardado subset analítico: ", OUT_CSV_19_22)

# -----------------------------
# 10) (Opcional) EDA mínima con 3 figuras
# -----------------------------
if (DO_PLOTS) {
  message("Generando figuras rápidas (EDA mínima)...")
  
  library(dplyr); library(tidyr)
  
  by_hour <- datos_19_22 %>%
    mutate(h = lubridate::hour(fecha_hora)) %>%
    count(h, name = "n") %>%
    complete(h = 0:23, fill = list(n = 0))
  
  ggplot(by_hour, aes(x = h, y = n)) +
    geom_col(fill = "#69b3a2") +
    scale_x_continuous(breaks = 0:23) +
    labs(title = "Distribución de accidentes por hora del día (2019–2022)",
         x = "Hora del día (0–23)", y = "Número de accidentes") +
    theme_minimal()
  
  
  
  p2 <- ggplot(datos_19_22, aes(x = factor(is_rainy, labels = c("Sin lluvia","Con lluvia")))) +
    geom_bar() +
    labs(title = "Accidentes con vs sin lluvia (2019–2022)",
         x = "Condición", y = "Nº accidentes") +
    theme_minimal()
  
  p3 <- ggplot(datos_19_22, aes(x = temp_cat)) +
    geom_bar() +
    labs(title = "Accidentes por categoría de temperatura (2019–2022)",
         x = "Categoría", y = "Nº accidentes") +
    theme_minimal()
  
  f1 <- file.path(OUT_DIR, "fig_accidentes_por_hora_2019_2022.png")
  f2 <- file.path(OUT_DIR, "fig_lluvia_vs_no_lluvia_2019_2022.png")
  f3 <- file.path(OUT_DIR, "fig_accidentes_por_temp_cat_2019_2022.png")
  ggsave(f1, p1, width = 8, height = 5, dpi = 120)
  ggsave(f2, p2, width = 8, height = 5, dpi = 120)
  ggsave(f3, p3, width = 8, height = 5, dpi = 120)
  message("Figuras guardadas en: ", OUT_DIR)
}

# message(">>> Fin pipeline: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
