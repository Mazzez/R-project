# ============================================================
# 04_trend_maps.R  (résolution 0.5° × 0.5°)
# Cartes pixel-par-pixel de la pente de Sen et de la
# significativité Mann-Kendall pour chacune des 18 variables.
#
# Approche : on charge les 564 grilles 720x361, puis pour chaque
# pixel on calcule la tendance temporelle. Coût : 720x361 = 259920
# pixels x 564 valeurs x 18 variables = ~2.6 milliards d'opérations.
# Pour rester rapide, on travaille sur les anomalies désaisonnées
# (= valeur - climatologie mensuelle pixel par pixel) et on calcule
# une régression linéaire vectorisée plutôt que le test de Sen complet.
# Une seconde passe Mann-Kendall sera faite sur un sous-échantillon
# (1 pixel sur 4 = ~65000 pixels) pour la p-value.
#
# Sortie : Analyse Climat 0.5°x0.5°/outputs/maps/04_trend_<VAR>.png
#          Analyse Climat 0.5°x0.5°/outputs/trend_grids.rds
# ============================================================

suppressPackageStartupMessages({
  library(ncdf4); library(dplyr); library(ggplot2); library(scales)
  library(Kendall)
})

NC_BASE <- "/home/mazzez/Bureau/R project/Final Version/processed/nc_subset_05"
OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
MAPS    <- file.path(OUT_DIR, "maps")
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
cat("Fichiers NetCDF :", length(files), "\n")

# Méta-grille
nc0 <- nc_open(files[1])
lat <- nc0$dim$latitude$vals
lon <- nc0$dim$longitude$vals
nc_close(nc0)
nx <- length(lon); ny <- length(lat); nt <- length(files)

# Vecteur des dates et mois
dates <- as.Date(sapply(files, function(f) {
  ym <- sub("\\.nc$", "", basename(f))
  sprintf("%s-%s-01", substr(ym, 1, 4), substr(ym, 5, 6))
}))
months <- as.integer(format(dates, "%m"))
years  <- as.integer(format(dates, "%Y"))
t_yrs  <- as.numeric(dates - dates[1]) / 365.25

# ============================================================
# Fonction : charge un cube 720 x 361 x 564 pour une variable
# ============================================================
load_cube <- function(nc_var) {
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
  cube
}

# ============================================================
# Fonction : pour un cube, calcule la pente (régression linéaire
# vectorisée des anomalies désaisonnées vs t_yrs) en chaque pixel
# ============================================================
compute_trend_grid <- function(cube) {
  # Désaisonnement : retirer la moyenne mensuelle pixel par pixel
  clim <- array(NA_real_, dim = c(nx, ny, 12))
  for (m in 1:12) {
    idx <- which(months == m)
    clim[, , m] <- apply(cube[, , idx], c(1, 2), mean, na.rm = TRUE)
  }
  anom <- cube
  for (i in seq_len(nt)) {
    anom[, , i] <- cube[, , i] - clim[, , months[i]]
  }
  # Régression linéaire vectorisée : pente = cov(t, y) / var(t)
  t_centered <- t_yrs - mean(t_yrs)
  var_t <- sum(t_centered^2)
  # cov pixel par pixel : anom_centered * t_centered, sommé sur t
  anom_mean <- apply(anom, c(1, 2), mean, na.rm = TRUE)
  slope <- matrix(0, nx, ny)
  for (i in seq_len(nt)) {
    slope <- slope + (anom[, , i] - anom_mean) * t_centered[i]
  }
  slope <- slope / var_t   # pente en unité/an
  slope
}

# ============================================================
# Fonction : Mann-Kendall sur sous-échantillon de pixels (1/16)
# pour obtenir une carte de p-value à coût raisonnable
# ============================================================
compute_pval_grid_subsample <- function(cube, step = 4) {
  pval <- matrix(NA_real_, nx, ny)
  ix <- seq(1, nx, by = step)
  iy <- seq(1, ny, by = step)
  cat(sprintf("    MK sur %d pixels (sous-échantillon 1/%d)...\n",
              length(ix) * length(iy), step^2))
  for (i in ix) {
    for (j in iy) {
      v <- cube[i, j, ]
      if (sum(!is.na(v)) > 100) {
        pval[i, j] <- suppressWarnings(MannKendall(v))$sl
      }
    }
  }
  pval
}

# ============================================================
# Fonction : carte ggplot d'un grid
# ============================================================
plot_map <- function(grid, title, subtitle, unit, palette = "div") {
  df_grid <- expand.grid(lon = lon, lat = lat) |>
    mutate(value = as.vector(grid))

  if (palette == "div") {
    lim <- quantile(abs(df_grid$value), 0.99, na.rm = TRUE)
    sc  <- scale_fill_gradient2(low = "steelblue", mid = "white",
                                high = "tomato", midpoint = 0,
                                limits = c(-lim, lim), oob = squish,
                                name = unit)
  } else {
    sc  <- scale_fill_viridis_c(name = unit)
  }
  ggplot(df_grid, aes(lon, lat, fill = value)) +
    geom_raster() +
    coord_fixed(ratio = 1, expand = FALSE) +
    sc +
    labs(title = title, subtitle = subtitle,
         x = "Longitude (°)", y = "Latitude (°)") +
    theme(legend.position = "right")
}

# ============================================================
# Boucle principale sur les 18 variables
# ============================================================
trend_results <- list()

for (nc_var in names(VAR_MAP)) {
  short <- VAR_MAP[[nc_var]]
  cat(sprintf("\n[%s] chargement du cube...\n", short))
  cube <- load_cube(nc_var)

  cat(sprintf("[%s] tendance pixel par pixel...\n", short))
  slope <- compute_trend_grid(cube)

  cat(sprintf("[%s] p-value Mann-Kendall (sous-échantillon)...\n", short))
  pval <- compute_pval_grid_subsample(cube, step = 4)

  trend_results[[short]] <- list(slope = slope, pval = pval)

  # Plot pente
  p <- plot_map(slope,
                title    = sprintf("%s — pente locale (régression sur anomalies)", short),
                subtitle = sprintf("Unité / an, sur 1979-2025 ; Δ pic-vallée à 99 %% : %.4g",
                                   diff(quantile(slope, c(0.005, 0.995),
                                                 na.rm = TRUE))),
                unit = "/an")
  ggsave(file.path(MAPS, sprintf("04_trend_%s.png", short)),
         p, width = 11, height = 5.5, dpi = 130)

  rm(cube); gc()
}

saveRDS(trend_results, file.path(OUT_DIR, "trend_grids.rds"))
cat("\n=== 18 cartes de tendance sauvegardées dans :", MAPS, "===\n")
