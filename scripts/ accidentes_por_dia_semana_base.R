#!/usr/bin/env Rscript
# ================================================================
# accidentes_por_dia_semana_base.R
# Objetivo: Leer CSV, obtener día de la semana (base R) y graficar.
# Salida: data/processed/fig_accidentes_por_dia_semana_base.png
# ================================================================

# ---- Parámetros
INPUT_CSV <- "../data/processed/accidentes_madrid_con_weather.csv"
OUT1_PNG  <- "../data/processed/fig_accidentes_por_dia_semana_base.png"
OUT2_PNG  <- "../data/processed/fig_accidentes_holiday_vs_nonholiday.png"
OUT3_PNG <- "../data/processed/fig_accidentes_media_festivo_vs_nofestivo.png"
TZ_LOCAL  <- "Europe/Madrid"
COUNTRY   <- "ES"

# ---- Lectura
if (!file.exists(INPUT_CSV)) stop("No se encontró: ", INPUT_CSV)
df <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)

# ---- Detectar columna de time y parsear
if ("time" %in% names(df)) {
  fecha_txt <- df$time
} else if ("time" %in% names(df)) {         
  fecha_txt <- df$fecha_hora
} else {
  stop("El CSV no tiene columna 'time'")
}

# Intento de parseo (varios formatos comunes).
parse_try_dt <- function(x, tz = "UTC") {
  x2 <- sub("T", " ", x, fixed = TRUE)  
  fmts <- c(
    "%Y-%m-%d %H:%M:%S",   # 2019-01-01 03:50:00
    "%Y-%m-%d %H:%M",      # 2019-01-01 03:50
    "%Y-%m-%d",            # 2019-01-01
    "%d/%m/%Y %H:%M:%S",   # 27/10/2023 14:00:00
    "%d/%m/%Y %H:%M",      # 27/10/2023 14:00
    "%d/%m/%Y"             # 27/10/2023
  )
  as.POSIXct(x2, tz = tz, tryFormats = fmts)
}

fecha <- parse_try(fecha_txt)
if (all(is.na(fecha))) stop("No se pudo parsear la columna time.")

# ---------- Day of week ----------
# accidentes por fecha
daily <- as.data.frame(table(fecha_date))
names(daily) <- c("fecha", "accidents")
daily$fecha <- as.Date(daily$fecha)

wd <- as.POSIXlt(daily$fecha)$wday 
map_es <- c("domingo","lunes","martes","miércoles","jueves","viernes","sábado")
day_es <- map_es[wd + 1]
lvl_es <- c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")
day_es <- factor(day_es, levels = lvl_es, ordered = TRUE)
fecha_date <- as.Date(fecha, tz = TZ_LOCAL)
daily$day <- factor(map_es[wd + 1], levels = lvl_es, ordered = TRUE)

# --- Para cada día-de-semana: total y nº de días existentes en el periodo
sum_by <- aggregate(accidents ~ day, data = daily, sum)       # total accidentes en lunes, martes, ...
n_by   <- aggregate(fecha ~ day, data = daily, function(x) length(unique(x)))  # nº de lunes, martes, ...
names(n_by)[2] <- "n_dias"

# --- Media = total / nº de días
stats <- merge(sum_by, n_by, by = "day", all = TRUE)
stats$mean_acc <- stats$accidents / stats$n_dias

# ---------- Tabla de frecuencias ----------
tab <- table(day_en[!is.na(day_en)])        
tab_df <- data.frame(
  day = factor(names(tab), levels = levels(day_en), ordered = TRUE),
  accidents = as.integer(tab)
)

# ---- Mostrar por consola
cat("Accidentes por día de la semana:\n")
print(tab)

# ---------- Gráfica 1: Accidents by day of the week ----------
p1 <- ggplot(stats, aes(x = day, y = mean_acc)) +
  geom_col(fill = "grey70", color = "grey30") +
  geom_text(aes(label = sprintf("%.2f", mean_acc)), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Media de accidentes por día de la semana (2019–2023)",
       x = "Día de la semana", y = "Accidentes por día de la semana (media)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid.minor = element_blank()
  )
ggsave(OUT1_PNG, p1, width = 9, height = 5.5, dpi = 120)
message("Gráfica 1 guardada en: ", OUT1_PNG)

# ---------- Festivos (Nager.Date) ----------
years <- sort(unique(format(as.Date(fecha, tz = TZ_LOCAL), "%Y")))
get_holidays <- function(year, country = COUNTRY) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/", country)
  json <- try(jsonlite::fromJSON(url), silent = TRUE)
  if (inherits(json, "try-error") || length(json) == 0) {
    warning("No se pudieron obtener festivos para ", year)
    return(data.frame(date = as.Date(character()), stringsAsFactors = FALSE))
  }
  data.frame(
    date = as.Date(json$date),
    localName = json$localName,
    name = json$name,
    fixed = json$fixed,
    global = json$global,
    stringsAsFactors = FALSE
  )
}
hols_list <- lapply(years, get_holidays)
hols <- if (length(hols_list)) do.call(rbind, hols_list) else data.frame(date = as.Date(character()))
hols_days <- unique(hols$date)

# ---------- Marcar festivo vs no festivo ----------
fecha_date <- as.Date(fecha, tz = TZ_LOCAL)
is_holiday <- fecha_date %in% hols_days
holiday_lbl <- factor(ifelse(is_holiday, "Holiday (ES)", "Non-holiday"),
                      levels = c("Non-holiday","Holiday (ES)"))

# ---------- Comparativa por día: Festivo vs No festivo (MEDIA) ----------

# (partimos de 'daily' ya creado: accidentes por fecha)
# daily: data.frame(fecha, accidents)
# añade día de semana (es) y etiqueta festivo
wd_daily <- as.POSIXlt(daily$fecha)$wday
map_es  <- c("domingo","lunes","martes","miércoles","jueves","viernes","sábado")
lvl_es  <- c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")
daily$day <- factor(map_es[wd_daily + 1], levels = lvl_es, ordered = TRUE)

daily$holiday <- factor(
  ifelse(daily$fecha %in% hols_days, "Festivo (ES)", "No festivo"),
  levels = c("No festivo","Festivo (ES)")
)

# MEDIA diaria por (día-de-semana, festivo)
mean_by <- aggregate(accidents ~ day + holiday, data = daily, FUN = mean, na.rm = TRUE)
names(mean_by)[names(mean_by)=="accidents"] <- "mean_acc"

# nº de días en cada celda
n_by <- aggregate(fecha ~ day + holiday, data = daily, function(x) length(unique(x)))
names(n_by)[names(n_by)=="fecha"] <- "n_dias"

comp_mean <- merge(mean_by, n_by, by = c("day","holiday"), all = TRUE)
comp_mean <- comp_mean[order(comp_mean$day, comp_mean$holiday), ]

# gráfica: media (no totales)
pd <- position_dodge(width = 0.8)
p2 <- ggplot(comp_mean, aes(x = day, y = mean_acc, fill = holiday)) +
  geom_col(position = pd, color = "grey30") +
  geom_text(aes(label = sprintf("%.2f", mean_acc)), position = pd,
            vjust = -0.35, size = 3.3, na.rm = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  scale_fill_manual(values = c("#9ecae1", "#de2d26")) +
  labs(
    title    = "Media de accidentes diarios — Festivo vs. No festivo (2019–2023)",
    subtitle = "Para cada día de la semana: total de accidentes / nº de días de ese tipo (festivo/no festivo)",
    x = "Día de la semana", y = "Media de accidentes diarios", fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(OUT2_PNG, p2, width = 9, height = 5.5, dpi = 120)
message("Gráfica 2 (media) guardada en: ", OUT2_PNG)

# gráfica3: festivo / no festivo (sin usar día de la semana)
daily$holiday <- factor(
  ifelse(daily$fecha %in% hols_days, "Festivo (ES)", "No festivo"),
  levels = c("No festivo","Festivo (ES)")
)

# Media diaria por condición (festivo vs no festivo)
mean_h <- aggregate(accidents ~ holiday, data = daily, FUN = mean, na.rm = TRUE)
names(mean_h)[2] <- "mean_acc"

# (opcional) nº de días en cada grupo, por si quieres mostrarlo
n_h <- aggregate(fecha ~ holiday, data = daily, function(x) length(unique(x)))
names(n_h)[2] <- "n_dias"

mean_h <- merge(mean_h, n_h, by = "holiday", all = TRUE)

# --- Gráfica 3: dos barras (media de accidentes diarios)
p3 <- ggplot(mean_h, aes(x = holiday, y = mean_acc, fill = holiday)) +
  geom_col(color = "grey30", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", mean_acc)), vjust = -0.35, size = 3.6) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  scale_fill_manual(values = c("#9ecae1", "#de2d26")) +
  labs(
    title    = "Media de accidentes — Festivo vs. No festivo (2019–2023)",
    subtitle = "Total de accidentes por condición / nº de días de esa condición en el período",
    x = "", y = "Media de accidentes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(OUT3_PNG, p3, width = 7.5, height = 5, dpi = 120)
message("Gráfica 3 guardada en: ", OUT3_PNG)

# ---------- Resumen por consola ----------
totales <- aggregate(accidents ~ holiday, comp_tab, sum)
top_days <- head(tab_df[order(-tab_df$accidents), ], 3)
prop_holiday <- with(totales, accidents[holiday == "Holiday (ES)"] / sum(accidents))

cat("\n== Summary ==\n")
print(tab_df)
cat("\nTop 3 days with most crashes:\n")
print(top_days)
cat(sprintf("\nShare of crashes on holidays: %.2f%%\n", 100 * prop_holiday))
cat("\nDone.\n")









