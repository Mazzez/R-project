# ============================================================
# 05_preparation.R
# Construit les 4 représentations de chaque série pour traiter
# la non-stationnarité et le cycle saisonnier :
#   1. niveau brut             (level)
#   2. anomalie désaisonnée    (anom = level - climatologie mensuelle)
#   3. différence première     (d1 = X_t - X_{t-1})
#   4. différence saisonnière  (d12 = X_t - X_{t-12})
#
# Entrée  : Analyse Climat 2.5°x2.5°/outputs/climate_co2_monthly.csv
# Sortie  : Analyse Climat 2.5°x2.5°/outputs/series_transformed.rds
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
df <- read.csv(file.path(OUT_DIR, "climate_co2_monthly.csv")) |>
  mutate(date = as.Date(date)) |>
  arrange(date)

# Liste des variables à transformer (climat + CO2 + CRE)
target_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
                 "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
                 "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
                 "CRE_SW","CRE_LW","CRE_net",
                 "co2_avg","co2_trend")

cat("Variables à traiter :", length(target_vars), "\n")
cat("Période             :", as.character(min(df$date)), "->",
    as.character(max(df$date)), "\n\n")

# ------------------------------------------------------------
# Calcul des 4 représentations
# ------------------------------------------------------------
make_transformations <- function(df, var) {
  # 1. level
  level <- df[[var]]

  # 2. anomalie désaisonnée = level - climatologie mensuelle
  clim_month <- df |>
    group_by(month) |>
    summarise(clim = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  anom <- level - clim_month$clim[match(df$month, clim_month$month)]

  # 3. anomalie désaisonnée ET détendrée (résidu de la régression
  #    linéaire de anom sur le temps) -> série stationnaire interprétable
  t <- as.numeric(df$date)
  fit  <- lm(anom ~ t)
  resid <- as.numeric(anom - predict(fit))

  # 4. différence première
  d1 <- c(NA, diff(level, lag = 1))

  # 5. différence saisonnière (lag 12)
  d12 <- c(rep(NA, 12), diff(level, lag = 12))

  data.frame(level = level, anom = anom, resid = resid,
             d1 = d1, d12 = d12)
}

trans <- list()
for (v in target_vars) {
  trans[[v]] <- make_transformations(df, v)
}

# Construire 4 data.frames "wide" (un par représentation)
build_wide <- function(repr) {
  cols <- lapply(target_vars, function(v) trans[[v]][[repr]])
  out <- as.data.frame(setNames(cols, target_vars))
  cbind(date = df$date, year = df$year, month = df$month, out)
}

ds <- list(
  level = build_wide("level"),
  anom  = build_wide("anom"),
  resid = build_wide("resid"),
  d1    = build_wide("d1"),
  d12   = build_wide("d12")
)

# Aperçu
for (repr in names(ds)) {
  d <- ds[[repr]]
  cat(sprintf("[%s]  dim = %d x %d   NA(%%) = %.1f\n",
              repr, nrow(d), ncol(d),
              100 * mean(is.na(d[, target_vars]))))
}

round_num <- function(d, k = 3) {
  d[] <- lapply(d, function(x) if (is.numeric(x)) round(x, k) else x)
  d
}
cat("\nAperçu anomalies (3 premières lignes) :\n")
print(round_num(head(ds$anom[, c("date", "T2m", "PWAT", "co2_avg")], 3)))

cat("\nAperçu différences premières (lignes 2-4) :\n")
print(round_num(ds$d1[2:4, c("date", "T2m", "PWAT", "co2_avg")]))

# ------------------------------------------------------------
# Sauvegarde
# ------------------------------------------------------------
saveRDS(ds, file.path(OUT_DIR, "series_transformed.rds"))

# Aussi en CSV pour inspection facile
for (repr in names(ds)) {
  write.csv(ds[[repr]],
            file.path(OUT_DIR, paste0("series_", repr, ".csv")),
            row.names = FALSE)
}

cat("\n=== Fichiers sauvegardés ===\n")
cat(" - series_transformed.rds (liste de 5 dataframes)\n")
cat(" - series_level.csv, series_anom.csv, series_resid.csv, series_d1.csv, series_d12.csv\n")
