# ============================================================
# Script: scripts/script1.R
# Lee desde data/raw y guarda en data/processed
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(janitor)

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# ----------------- Helpers -----------------
# Detectar AM/PM por 'a'/'p' después de la hora y pasar a 24h (HH:MM)
normalize_ampm <- function(hora_raw) {
  h <- iconv(hora_raw, from = "", to = "ASCII//TRANSLIT")
  h <- tolower(h)
  h <- str_squish(h)
  
  hora_parte <- str_extract(h, "^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?")
  hora_hm <- ifelse(!is.na(hora_parte), str_replace(hora_parte, "^([0-9]{1,2}:[0-9]{2}).*$", "\\1"), NA)
  
  es_pm <- !is.na(hora_parte) & str_detect(h, "^[0-9:]+\\s*p")
  es_am <- !is.na(hora_parte) & str_detect(h, "^[0-9:]+\\s*a")
  am_pm <- ifelse(es_pm, "pm", ifelse(es_am, "am", NA))
  
  h24 <- suppressWarnings(
    ifelse(!is.na(am_pm) & !is.na(hora_hm),
           format(parse_date_time(paste(hora_hm, am_pm), orders = "I:M p"), "%H:%M"),
           format(parse_date_time(hora_hm, orders = c("H:M", "I:M")), "%H:%M"))
  )
  h24
}

# Reparar mojibake detectado en tu archivo
fix_mojibake <- function(x) {
  if (!is.character(x)) return(x)
  str_replace_all(
    x,
    c(
      # Vocales y tildes
      "ê"="í", "Ê"="Í",
      "ç"="á", "Ç"="Á",
      "—"="ó", "–"="ñ",
      "œ"="ú", "’"="í",
      "‡"="á", "Ž"="é", "ž"="é",
      # Ñ mal codificada
      "„"="Ñ",  # Ej. LOGRO„O → LOGROÑO
      "„a"="ña", "„A"="ÑA",
      "„o"="ño", "„O"="ÑO",
      "„u"="ñu", "„U"="ÑU",
      "„e"="ñe", "„E"="ÑE",
      "„i"="ñi", "„I"="ÑI",
      # Fracciones y símbolos corruptos
      "¼"="1/4", "½"="1/2", "¾"="3/4",
      "»"="", "«"="",
      # Mayúsculas extrañas vistas en datos
      "¢"="ó", "¡"="á"
    )
  )
}





# ----------------- 1) datos_madrid -----------------
# Codificación que mejor funciona con tu archivo
datos <- read_delim(
  "data/raw/datos_madrid.csv",
  delim = ";",
  locale = locale(encoding = "Windows-1252", decimal_mark = ","),
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  clean_names()

stopifnot(all(c("fecha","hora") %in% names(datos)))

datos <- datos |>
  # Normalización general de textos + fix mojibake
  mutate(across(where(is.character), ~str_squish(.x))) |>
  mutate(across(where(is.character), fix_mojibake)) |>
  # 'NULL' literales a NA
  mutate(distrito = na_if(distrito, "NULL")) |>
  # Hora a 24h
  mutate(
    hora_norm = normalize_ampm(hora),
    fecha_str = as.character(fecha),
    fecha_hora_raw = paste(fecha_str, hora_norm)
  )

# fecha en dmy + hora ya en 24h
datos <- datos |>
  mutate(
    dt = parse_date_time(fecha_hora_raw, orders = "dmy HM", tz = "Europe/Madrid", quiet = TRUE)
  )

datos_clean <- datos |>
  mutate(time = format(dt, "%Y-%m-%d %H:%M")) |>
  select(-hora_norm, -fecha_str, -fecha_hora_raw, -dt)

# ----------------- 2) weather_madrid_2019-2022 -----------------
weather <- read_csv(
  "data/raw/weather_madrid_2019-2022.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) |>
  clean_names() |>
  select(-any_of(c("unnamed_0","...1","x1")))

stopifnot("time" %in% names(weather))

weather <- weather |>
  mutate(
    dt = parse_date_time(time, orders = c("Ymd HMS", "Ymd HM"),
                         tz = "Europe/Madrid", quiet = TRUE),
    time = format(dt, "%Y-%m-%d %H:%M")
  ) |>
  select(-dt)

# ----------------- 3) Guardar -----------------
write_csv(datos_clean, "data/processed/datos_madrid_clean.csv")
write_csv(weather,     "data/processed/weather_madrid_2019-2022_clean.csv")

cat("✅ Archivos procesados en data/processed:\n",
    "- datos_madrid_clean.csv\n",
    "- weather_madrid_2019-2022_clean.csv\n")

# =======================
# 4) VALIDACIÓN RÁPIDA
# =======================

# Vuelve a cargar el limpio para validar (evita mirar objetos viejos en memoria)
df <- readr::read_csv("data/processed/datos_madrid_clean.csv", show_col_types = FALSE)

# 4.1 ¿Quedaron NAs en la columna time?
na_time <- sum(is.na(df$time))
cat("🔎 NAs en 'time':", na_time, "\n")

# 4.2 Búsqueda de mojibake frecuente en todas las columnas de texto
bad_chars <- c("„","¼","½","¾","»","«","œ","‡","Ž","ž","’","–","—","¢","¡")
# escapar para regex
esc <- function(x) stringr::str_replace_all(x, "([\\\\.^$|()*+?\\[\\{\\]])", "\\\\\\1")
pattern <- paste0("(", paste(vapply(bad_chars, esc, character(1)), collapse="|"), ")")

char_cols <- names(df)[vapply(df, is.character, logical(1))]
hallazgos <- list()

for (col in char_cols) {
  vals <- df[[col]]
  hit <- which(stringr::str_detect(vals, pattern))
  if (length(hit)) {
    # guarda hasta 10 ejemplos por columna
    ejemplos <- unique(vals[hit])[1:min(10, length(unique(vals[hit])))]
    hallazgos[[col]] <- tibble::tibble(
      columna = col,
      n_filas_afectadas = length(hit),
      ejemplos = paste(ejemplos, collapse = " | ")
    )
  }
}

if (length(hallazgos)) {
  reporte <- dplyr::bind_rows(hallazgos)
  print(reporte, n = nrow(reporte))
  # opcional: guardar el informe
  readr::write_csv(reporte, "data/processed/_mojibake_report.csv")
  cat("📄 Informe guardado en: data/processed/_mojibake_report.csv\n")
} else {
  cat("✅ No se encontraron caracteres sospechosos en columnas de texto.\n")
}

# =======================
# 5) JOIN SIMPLE POR HORA (truncado) + LIMPIEZA FINAL
# =======================
library(data.table)

# 1) Cargar
acc <- fread("data/processed/datos_madrid_clean.csv")
met <- fread("data/processed/weather_madrid_2019-2022_clean.csv")

# 2) Clave por hora (truncado): "YYYY-MM-DD HH"
acc[, hour_key := substr(time, 1, 13)]
met[, hour_key := substr(time, 1, 13)]

# 3) Agregar meteo por hora (media de numéricas)
num_cols <- names(which(sapply(met, is.numeric)))
met_hour <- met[, c(
  .(wx_time = paste0(unique(hour_key), ":00")),
  lapply(.SD, mean, na.rm = TRUE)
), by = hour_key, .SDcols = num_cols]

# 4) Left join (mantiene todos los accidentes)
res <- met_hour[acc, on = .(hour_key)]

# 5) Prefijar columnas meteorológicas con 'wx_'
met_cols_to_prefix <- setdiff(names(met_hour), c("hour_key", "wx_time"))
present_to_prefix  <- intersect(names(res), met_cols_to_prefix)
setnames(res, present_to_prefix, paste0("wx_", present_to_prefix))

# 6) Eliminar columnas innecesarias
cols_to_drop <- c(
  "cod_lesividad", "cod_distrito", "hora", "fecha", "wx_time", "hour_key",
  "wx_solar_radiation", "wx_barometric_pressure", "wx_humidity", "wx_wind_direction"
  , "numero"
)
cols_to_drop <- intersect(cols_to_drop, names(res))
res[, (cols_to_drop) := NULL]

# 7) Guardar
outfile <- "data/processed/accidentes_madrid_con_weather.csv"
fwrite(res, outfile)

# 8) Resumen rápido
cat(
  "✅ JOIN por hora listo.\n",
  "Horas meteo: ", nrow(met_hour),
  " | Accidentes: ", nrow(acc),
  " | Accidentes con meteo: ", sum(!is.na(res$wx_temperature)),  # suponiendo que 'temperature' existe
  " | Sin meteo: ", sum(is.na(res$wx_temperature)), "\n",
  "Archivo guardado en: ", outfile, "\n", sep = ""
)

