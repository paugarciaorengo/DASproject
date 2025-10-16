#!/usr/bin/env Rscript
# ================================================================
# accidentes_por_dia_semana_base.R
# Objetivo: Leer CSV, obtener día de la semana (base R) y graficar.
# Salida: data/processed/fig_accidentes_por_dia_semana_base.png
# ================================================================

# ---- Parámetros
INPUT_CSV <- "data/processed/accidentes_madrid_weather_2019_2022.csv"  
OUT_PNG   <- "data/processed/fig_accidentes_por_dia_semana_base.png"

# ---- Lectura
if (!file.exists(INPUT_CSV)) stop("No se encontró: ", INPUT_CSV)
df <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)

# ---- Detectar columna de fecha y parsear
if ("fecha" %in% names(df)) {
  fecha_txt <- df$fecha
} else if ("fecha_hora" %in% names(df)) {         
  fecha_txt <- df$fecha_hora
} else {
  stop("El CSV no tiene columnas 'fecha' ni 'fecha_hora'.")
}

# Intento de parseo (varios formatos comunes).
parse_try <- function(x) {
  x2 <- sub("T", " ", x, fixed = TRUE)
  fmts <- c(
    "%Y-%m-%d",            # 2023-10-27
    "%d/%m/%Y",            # 27/10/2023
    "%Y-%m-%d %H:%M:%S",   # 2023-10-27 14:00:00
    "%d/%m/%Y %H:%M:%S"    # 27/10/2023 14:00:00
  )
  for (f in fmts) {
    d <- as.Date(x2, format = f)
    if (any(!is.na(d))) return(d)  
  }
  as.Date(x2)
}

fecha <- parse_try(fecha_txt)
if (all(is.na(fecha))) stop("No se pudo parsear la columna de fecha/fecha_hora.")

# ---- Día de la semana con base R
dia_sem <- weekdays(fecha)              # depende de LC_TIME para el idioma

# ---- Tabla de frecuencias (quitamos NA)
tab <- sort(table(dia_sem[!is.na(dia_sem)]))

# ---- Mostrar por consola
cat("Accidentes por día de la semana:\n")
print(tab)

# ---- Guardar gráfica base R
dir.create(dirname(OUT_PNG), recursive = TRUE, showWarnings = FALSE)
png(OUT_PNG, width = 900, height = 550, res = 120)
barplot(tab,
        main = "Accidentes por día de la semana (2019–2022)",
        xlab = "Día de la semana", ylab = "Número de accidentes",
        las = 2)  # etiquetas verticales
grid(nx = NA, ny = NULL)
dev.off()
cat("Gráfica guardada en:", OUT_PNG, "\n")
