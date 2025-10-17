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

# ---------- Day of week en INGLÉS, orden Mon..Sun ----------
wd <- as.POSIXlt(fecha)$wday
map_en <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
day_en <- map_en[wd + 1]
lvl_en <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
day_en <- factor(day_en, levels = lvl_en, ordered = TRUE)

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
p1 <- ggplot(tab_df, aes(x = day, y = accidents)) +
  geom_col(fill = "grey70", color = "grey30") +
  geom_text(aes(label = accidents), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Crashes by day of the week (2019–2022)",
       x = "Day of the week", y = "Number of crashes") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid.minor = element_blank()
  )
dir.create(dirname(OUT1_PNG), recursive = TRUE, showWarnings = FALSE)
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

# ---------- Comparativa por día: Holiday vs Non-holiday ----------
comp_df <- data.frame(day = day_en, holiday = holiday_lbl)
comp_df <- comp_df[!is.na(comp_df$day), ]
comp_tab <- as.data.frame(table(comp_df$day, comp_df$holiday))
names(comp_tab) <- c("day", "holiday", "accidents")
comp_tab$day <- factor(comp_tab$day, levels = lvl_en, ordered = TRUE)

pd <- position_dodge(width = 0.8)
p2 <- ggplot(comp_tab, aes(x = day, y = accidents, fill = holiday)) +
  geom_col(position = pd, color = "grey30") +
  geom_text(aes(label = accidents), position = position_dodge(width = 0.8),
            vjust = -0.35, size = 3.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  scale_fill_manual(values = c("#9ecae1", "#de2d26")) +
  labs(title = "Crashes by day — Holiday vs. Non-holiday (2019–2022)",
       x = "Day of the week", y = "Number of crashes", fill = "") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid.minor = element_blank()
  )
ggsave(OUT2_PNG, p2, width = 9, height = 5.5, dpi = 120)
message("Gráfica 2 guardada en: ", OUT2_PNG)

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









