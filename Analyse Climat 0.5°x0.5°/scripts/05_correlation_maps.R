# ============================================================
# 05_correlation_maps.R  (résolution 0.5° × 0.5°)
# Cartes des corrélations pixel-par-pixel entre chaque variable
# climatique et le CO2_trend (NOAA GML), sur les RÉSIDUS
# (anomalies désaisonnées + détendrées) — c'est-à-dire le signal
# interannuel propre, comme dans la phase 4 du 2.5°.
#
# Méthode :
#   1. Charge le cube 720x361x564 d'une variable
#   2. Désaisonne (clim mensuelle) puis détendre (régression linéaire
#      vs t) en chaque pixel → résidus
#   3. Charge co2_trend (NOAA), désaisonne et détendre
#   4. Calcule cor(resid_pixel, resid_co2) pour chaque pixel
#
# Sortie : Analyse Climat 0.5°x0.5°/outputs/maps/05_corr_<VAR>.png
#          Analyse Climat 0.5°x0.5°/outputs/correlation_grids.rds
# ============================================================

suppressPackageStartupMessages({
  library(ncdf4); library(dplyr); library(ggplot2); library(scales)
})

NC_BASE  <- "/home/mazzez/Bureau/R project/Final Version/processed/nc_subset_05"
CO2_FILE <- "/home/mazzez/Bureau/R project/Final Version/CO2/co2_mm_gl.csv"
OUT_DIR  <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
MAPS     <- file.path(OUT_DIR, "maps")
dir.create(MAPS, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 11))

VAR_MAP <- c(
  TMP_2maboveground                                = "T2m",
  TMP                                              = "T500",  # plevel=500 dans le 0.5°
  SPFH_2maboveground                               = "SPFH2m",
  PWAT_entireatmosphere_consideredasasinglelayer_  = "PWAT",
  APCP_surface                                     = "APCP",
  TCDC_entireatmosphere_consideredasasinglelayer_  = "TCDC",
  DLWRF_surface                                    = "DLWRF",
  ULWRF_surface                                    = "ULWRF",
  DSWRF_surface                                    = "DSWRF",
  USWRF_surface                                    = "USWRF",
  PRMSL_meansealevel                               = "PRMSL",
  CSDSF_surface                                    = "CSDSF",
  CSUSF_surface                                    = "CSUSF",
  CSDLF_surface                                    = "CSDLF",
  CSULF_surface                                    = "CSULF",
  CDUVB_surface                                    = "CDUVB",
  DUVB_surface                                     = "DUVB",
  ALBDO_surface                                    = "ALBDO"
)

files <- sort(list.files(NC_BASE, pattern = "\\.nc$",
                         recursive = TRUE, full.names = TRUE))
nc0 <- nc_open(files[1])
lat <- nc0$dim$latitude$vals
lon <- nc0$dim$longitude$vals
nc_close(nc0)
nx <- length(lon); ny <- length(lat); nt <- length(files)

dates <- as.Date(sapply(files, function(f) {
  ym <- sub("\\.nc$", "", basename(f))
  sprintf("%s-%s-01", substr(ym, 1, 4), substr(ym, 5, 6))
}))
months <- as.integer(format(dates, "%m"))
t_yrs  <- as.numeric(dates - dates[1]) / 365.25

# ------------------------------------------------------------
# Préparation CO2 : désaisonnage + détendrage linéaire
# ------------------------------------------------------------
co2 <- read.csv(CO2_FILE, comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  select(date, co2_trend = trend)

# Aligner CO2 sur les dates des NetCDF
co2_aligned <- co2[match(dates, co2$date), ]
co2_vec     <- co2_aligned$co2_trend
co2_clim    <- tapply(co2_vec, months, mean, na.rm = TRUE)
co2_anom    <- co2_vec - co2_clim[months]
co2_resid   <- residuals(lm(co2_anom ~ t_yrs, na.action = na.exclude))

cat("CO2 aligné :", sum(!is.na(co2_vec)), "/ ", nt, "valeurs non-NA\n\n")

# ------------------------------------------------------------
# Fonction : charge un cube et calcule les résidus en chaque pixel
# ------------------------------------------------------------
load_resid_cube <- function(nc_var) {
  cube <- array(NA_real_, dim = c(nx, ny, nt))
  for (i in seq_along(files)) {
    nc <- nc_open(files[i])
    if (nc_var %in% names(nc$var)) {
      arr <- ncvar_get(nc, nc_var)   # collapse_degen = TRUE par défaut
      if (length(dim(arr)) == 2 && all(dim(arr) == c(nx, ny))) {
        cube[, , i] <- arr
      }
    }
    nc_close(nc)
  }
  # Désaisonnage
  clim <- array(NA_real_, dim = c(nx, ny, 12))
  for (m in 1:12) {
    idx <- which(months == m)
    clim[, , m] <- apply(cube[, , idx], c(1, 2), mean, na.rm = TRUE)
  }
  anom <- cube
  for (i in seq_len(nt)) {
    anom[, , i] <- cube[, , i] - clim[, , months[i]]
  }
  # Détendrage : retirer la régression linéaire de anom sur t_yrs
  t_centered <- t_yrs - mean(t_yrs)
  var_t <- sum(t_centered^2)
  anom_mean <- apply(anom, c(1, 2), mean, na.rm = TRUE)
  slope <- matrix(0, nx, ny)
  for (i in seq_len(nt)) {
    slope <- slope + (anom[, , i] - anom_mean) * t_centered[i]
  }
  slope <- slope / var_t
  # resid = anom - (anom_mean + slope * t_centered)
  resid <- anom
  for (i in seq_len(nt)) {
    resid[, , i] <- anom[, , i] - anom_mean - slope * t_centered[i]
  }
  resid
}

# ------------------------------------------------------------
# Corrélation vectorisée pixel par pixel
# ------------------------------------------------------------
compute_corr_grid <- function(resid_cube, y_resid) {
  # Restreindre aux dates où CO2 résiduel est dispo (sinon NA propage partout)
  ok   <- !is.na(y_resid)
  y    <- y_resid[ok]
  cube <- resid_cube[, , ok, drop = FALSE]
  n_ok <- length(y)

  y_mean <- mean(y)
  y_sd   <- sd(y)
  yc     <- y - y_mean

  x_mean <- apply(cube, c(1, 2), mean, na.rm = TRUE)
  x_var  <- apply(cube, c(1, 2), var,  na.rm = TRUE)
  cov_xy <- matrix(0, nx, ny)
  for (i in seq_len(n_ok)) {
    cov_xy <- cov_xy + (cube[, , i] - x_mean) * yc[i]
  }
  cov_xy <- cov_xy / (n_ok - 1)
  cov_xy / sqrt(x_var * y_sd^2)
}

# ------------------------------------------------------------
# Carte ggplot
# ------------------------------------------------------------
plot_corr_map <- function(grid, var) {
  df_grid <- expand.grid(lon = lon, lat = lat) |>
    mutate(value = as.vector(grid))
  ggplot(df_grid, aes(lon, lat, fill = value)) +
    geom_raster() +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                         midpoint = 0, limits = c(-1, 1), oob = squish,
                         name = "r") +
    labs(title    = sprintf("Corrélation locale %s ↔ CO2_trend (résidus)", var),
         subtitle = "Anomalies désaisonnées et détendrées 1979-2025 (signal interannuel propre)",
         x = "Longitude (°)", y = "Latitude (°)")
}

# ------------------------------------------------------------
# Boucle principale
# ------------------------------------------------------------
corr_results <- list()

for (nc_var in names(VAR_MAP)) {
  short <- VAR_MAP[[nc_var]]
  cat(sprintf("\n[%s] chargement et résidus...\n", short))
  resid_cube <- load_resid_cube(nc_var)

  cat(sprintf("[%s] corrélation avec CO2 résiduel...\n", short))
  r_grid <- compute_corr_grid(resid_cube, co2_resid)
  corr_results[[short]] <- r_grid

  cat(sprintf("[%s] médiane |r| = %.3f, max |r| = %.3f\n",
              short,
              median(abs(r_grid), na.rm = TRUE),
              max(abs(r_grid), na.rm = TRUE)))

  p <- plot_corr_map(r_grid, short)
  ggsave(file.path(MAPS, sprintf("05_corr_%s.png", short)),
         p, width = 11, height = 5.5, dpi = 130)

  rm(resid_cube); gc()
}

saveRDS(corr_results, file.path(OUT_DIR, "correlation_grids.rds"))
cat("\n=== 18 cartes de corrélation sauvegardées dans :", MAPS, "===\n")
