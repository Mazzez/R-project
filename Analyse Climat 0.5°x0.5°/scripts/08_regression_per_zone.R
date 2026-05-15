# ============================================================
# 08_regression_per_zone.R  (résolution 0.5° × 0.5°)
# Régression multivariée du CO2 résiduel par bande de latitude
# et par hotspot, pour répondre à : où le lien climat → CO2
# est-il le plus fort géographiquement ?
#
# Méthode (analogue phase 4 du 2.5°) :
#   1. Pour chaque zone (bande ou hotspot), désaisonner + détendre
#      linéairement chaque variable climat → résidus.
#   2. Même opération pour le CO2 (NOAA co2_trend).
#   3. Ajuster lm(co2_resid ~ toutes les vars climat résiduelles) ;
#      retenir R² et coefficient T2m comme indicateur.
#
# Sortie : outputs/regression_per_zone.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
CO2_FILE <- "/home/mazzez/Bureau/R project/Final Version/CO2/co2_mm_gl.csv"

# ------------------------------------------------------------
# 1. Chargement des séries
# ------------------------------------------------------------
bands_df <- read.csv(file.path(OUT_DIR, "monthly_band_means_05.csv")) |>
  mutate(date = as.Date(date))
hot_df   <- read.csv(file.path(OUT_DIR, "hotspots_series.csv")) |>
  mutate(date = as.Date(date))

co2 <- read.csv(CO2_FILE, comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  select(date, co2_trend = trend)

# Aligner sur les dates des bandes (= 1979-01 → 2025-12)
dates  <- sort(unique(bands_df$date))
months <- as.integer(format(dates, "%m"))
t_yrs  <- as.numeric(dates - dates[1]) / 365.25

co2_v   <- co2$co2_trend[match(dates, co2$date)]
co2_clim <- tapply(co2_v, months, mean, na.rm = TRUE)
co2_anom <- co2_v - co2_clim[months]
co2_resid <- residuals(lm(co2_anom ~ t_yrs, na.action = na.exclude))

ok_co2 <- !is.na(co2_resid)

# ------------------------------------------------------------
# Helper : résidus (désaisonnés + détendrés) d'une série mensuelle
# ------------------------------------------------------------
to_resid <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  clim <- tapply(x, months, mean, na.rm = TRUE)
  anom <- x - clim[months]
  residuals(lm(anom ~ t_yrs, na.action = na.exclude))
}

# ------------------------------------------------------------
# 2. Régression par bande de latitude
# ------------------------------------------------------------
clim_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO")

bands_list <- unique(bands_df$band)
results_band <- list()

for (b in bands_list) {
  sub <- bands_df |> filter(band == b) |> arrange(date)
  resid_mat <- sapply(clim_vars, function(v) to_resid(sub[[v]]))
  df_reg <- as.data.frame(resid_mat)
  df_reg$co2 <- co2_resid
  df_reg <- df_reg[ok_co2, ]
  fit <- lm(co2 ~ ., data = df_reg)
  r2  <- summary(fit)$r.squared
  r2_adj <- summary(fit)$adj.r.squared
  # Top 3 prédicteurs (par |t|)
  tvals <- summary(fit)$coefficients[, "t value"]
  tvals <- tvals[names(tvals) != "(Intercept)"]
  top3  <- names(sort(abs(tvals), decreasing = TRUE))[1:3]
  results_band[[b]] <- data.frame(
    zone        = b,
    type        = "bande",
    R2          = round(r2, 3),
    R2_adj      = round(r2_adj, 3),
    n_obs       = nrow(df_reg),
    top1        = top3[1],
    top2        = top3[2],
    top3        = top3[3]
  )
}

# ------------------------------------------------------------
# 3. Régression par hotspot
# ------------------------------------------------------------
hot_vars <- c("T2m","PWAT","APCP","TCDC")  # extraites en script 06
regions  <- c("Amazonie","Indonesie","Siberie","Sahel")

results_hot <- list()
for (r in regions) {
  resid_mat <- sapply(hot_vars,
                      function(v) to_resid(hot_df[[paste0(r, "_", v)]]))
  df_reg <- as.data.frame(resid_mat)
  df_reg$co2 <- co2_resid
  df_reg <- df_reg[ok_co2, ]
  fit <- lm(co2 ~ ., data = df_reg)
  r2  <- summary(fit)$r.squared
  r2_adj <- summary(fit)$adj.r.squared
  tvals <- summary(fit)$coefficients[, "t value"]
  tvals <- tvals[names(tvals) != "(Intercept)"]
  top3  <- names(sort(abs(tvals), decreasing = TRUE))[1:3]
  results_hot[[r]] <- data.frame(
    zone        = r,
    type        = "hotspot",
    R2          = round(r2, 3),
    R2_adj      = round(r2_adj, 3),
    n_obs       = nrow(df_reg),
    top1        = top3[1],
    top2        = top3[2],
    top3        = top3[3]
  )
}

# ------------------------------------------------------------
# 4. Tableau final
# ------------------------------------------------------------
final <- bind_rows(c(results_band, results_hot))
print(final, row.names = FALSE)
write.csv(final, file.path(OUT_DIR, "regression_per_zone.csv"),
          row.names = FALSE)

cat("\n=== Régression par zone terminée :",
    file.path(OUT_DIR, "regression_per_zone.csv"), "===\n")
