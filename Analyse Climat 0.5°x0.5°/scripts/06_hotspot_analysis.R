# ============================================================
# 06_hotspot_analysis.R  (résolution 0.5° × 0.5°)
# Analyse des anomalies climatiques sur 4 régions clés du système
# carbone-climat global :
#   - Amazonie         (5°S-5°N, 70°W-50°W) — puits/source CO2 tropical
#   - Indonésie        (10°S-5°N, 95°E-141°E) — puits ENSO-sensible
#   - Sibérie centrale (55°N-70°N, 70°E-130°E) — permafrost / boréal
#   - Sahel            (10°N-20°N, 20°W-40°E) — semi-aride sensible
#
# Pour chaque région :
#   - extraction des séries mensuelles moyennes (T2m, PWAT, APCP, TCDC)
#   - décomposition STL et tendance
#   - corrélation avec CO2_trend sur résidus
#
# Sortie : Analyse Climat 0.5°x0.5°/outputs/plots/06_hotspots*.png
#          Analyse Climat 0.5°x0.5°/outputs/hotspots_series.csv
# ============================================================

suppressPackageStartupMessages({
  library(ncdf4); library(dplyr); library(tidyr); library(ggplot2)
  library(scales); library(Kendall); library(trend)
})

NC_BASE  <- "/home/mazzez/Bureau/R project/Final Version/processed/nc_subset_05"
CO2_FILE <- "/home/mazzez/Bureau/R project/Final Version/CO2/co2_mm_gl.csv"
OUT_DIR  <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
PLOT     <- file.path(OUT_DIR, "plots")
dir.create(PLOT, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 11))

# ------------------------------------------------------------
# Définition des régions (latmin, latmax, lonmin, lonmax)
# Note : longitudes en convention 0 → 360 (comme dans le NetCDF)
# ------------------------------------------------------------
REGIONS <- list(
  Amazonie  = list(lat = c(-5,  5),  lon = c(290, 310),
                   label = "Amazonie (5°S-5°N, 70-50°W)"),
  Indonesie = list(lat = c(-10, 5),  lon = c(95,  141),
                   label = "Indonésie (10°S-5°N, 95-141°E)"),
  Siberie   = list(lat = c(55,  70), lon = c(70,  130),
                   label = "Sibérie centrale (55-70°N, 70-130°E)"),
  Sahel     = list(lat = c(10,  20), lon = c(340, 400),
                   label = "Sahel (10-20°N, 20°W-40°E)")
)
# Pour Sahel, on devra gérer le wrap-around 340-360 + 0-40 -> on
# stocke ça en deux ranges si besoin

KEY_VARS <- c(TMP_2maboveground = "T2m",
              PWAT_entireatmosphere_consideredasasinglelayer_ = "PWAT",
              APCP_surface = "APCP",
              TCDC_entireatmosphere_consideredasasinglelayer_ = "TCDC")

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
years  <- as.integer(format(dates, "%Y"))
months <- as.integer(format(dates, "%m"))

# Poids cos(lat)
w_lat <- cos(lat * pi / 180)

# ------------------------------------------------------------
# Construire un masque pour une région (gestion du wrap longitude)
# ------------------------------------------------------------
make_region_mask <- function(reg) {
  mask_lat <- lat >= reg$lat[1] & lat <= reg$lat[2]
  if (reg$lon[2] > 360) {
    mask_lon <- lon >= reg$lon[1] | lon <= (reg$lon[2] - 360)
  } else {
    mask_lon <- lon >= reg$lon[1] & lon <= reg$lon[2]
  }
  list(mask_lat = mask_lat, mask_lon = mask_lon)
}

# ------------------------------------------------------------
# Pré-calcul : masques + poids par région (une fois pour toutes)
# ------------------------------------------------------------
region_info <- list()
for (rname in names(REGIONS)) {
  m <- make_region_mask(REGIONS[[rname]])
  region_info[[rname]] <- list(
    mask_lon  = m$mask_lon,
    mask_lat  = m$mask_lat,
    w_sub_lat = w_lat[m$mask_lat]
  )
  cat(sprintf("[%s] %s — %d × %d cellules\n",
              rname, REGIONS[[rname]]$label,
              sum(m$mask_lon), sum(m$mask_lat)))
}

# Moyenne pondérée cos(lat) sur la sous-grille d'une région
band_mean_region <- function(arr, ri) {
  sub    <- arr[ri$mask_lon, ri$mask_lat, drop = FALSE]
  w_grid <- matrix(rep(ri$w_sub_lat, each = nrow(sub)),
                   nrow = nrow(sub), ncol = ncol(sub))
  ok <- !is.na(sub)
  sum(sub[ok] * w_grid[ok]) / sum(w_grid[ok])
}

# ------------------------------------------------------------
# CO2 désaisonné détendre (résidus) pour corrélation
# ------------------------------------------------------------
co2 <- read.csv(CO2_FILE, comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  select(date, co2_trend = trend)
co2_aligned <- co2[match(dates, co2$date), ]
co2_vec     <- co2_aligned$co2_trend
co2_clim    <- tapply(co2_vec, months, mean, na.rm = TRUE)
co2_anom    <- co2_vec - co2_clim[months]
t_yrs       <- as.numeric(dates - dates[1]) / 365.25
co2_resid   <- residuals(lm(co2_anom ~ t_yrs, na.action = na.exclude))

# ------------------------------------------------------------
# Boucle principale : un seul passage sur les NetCDF
# (566 ouvertures au lieu de 4 régions × 4 vars × 566 = 9056)
# ------------------------------------------------------------
all_series <- list()
for (rname in names(REGIONS)) {
  for (short in unname(KEY_VARS)) {
    all_series[[paste0(rname, "_", short)]] <- rep(NA_real_, nt)
  }
}

cat("\nExtraction des séries (1 passage I/O) ...\n")
t0 <- Sys.time()
for (i in seq_along(files)) {
  nc <- nc_open(files[i])
  for (nc_var in names(KEY_VARS)) {
    short <- KEY_VARS[[nc_var]]
    arr   <- ncvar_get(nc, nc_var)
    if (length(dim(arr)) != 2) next
    for (rname in names(REGIONS)) {
      all_series[[paste0(rname, "_", short)]][i] <-
        band_mean_region(arr, region_info[[rname]])
    }
  }
  nc_close(nc)
  if (i %% 60 == 0 || i == nt) {
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("  %3d / %d  (%.0f%%)  elapsed=%.0fs  eta=%.0fs\n",
                i, nt, 100 * i / nt, elapsed,
                elapsed * (nt - i) / i))
  }
}

# ------------------------------------------------------------
# Tableau long
# ------------------------------------------------------------
df <- data.frame(date = dates, year = years, month = months)
for (rname in names(REGIONS)) {
  for (short in unname(KEY_VARS)) {
    col <- paste0(rname, "_", short)
    df[[col]] <- all_series[[col]]
  }
}
write.csv(df, file.path(OUT_DIR, "hotspots_series.csv"), row.names = FALSE)
cat("\nCSV sauvegardé : hotspots_series.csv\n")

# ------------------------------------------------------------
# Plot 1 : T2m anomalie par hotspot
# ------------------------------------------------------------
make_anom <- function(x, m) x - tapply(x, m, mean)[as.character(m)]

df_anom <- df |>
  mutate(across(matches("_T2m$"),  ~ make_anom(.x, month)))

t2m_long <- df_anom |>
  pivot_longer(matches("_T2m$"), names_to = "var", values_to = "anom") |>
  mutate(region = sub("_T2m$", "", var))

p1 <- ggplot(t2m_long, aes(date, anom, color = region)) +
  geom_line(alpha = 0.4, linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_brewer(palette = "Set1") +
  labs(title    = "Anomalies T2m sur 4 hotspots (0.5°)",
       subtitle = "Anomalie = T2m − climatologie mensuelle ; lissage LOESS",
       x = NULL, y = "Anomalie T2m (K)", color = "Région") +
  theme(legend.position = "bottom")
ggsave(file.path(PLOT, "06a_T2m_hotspots.png"), p1,
       width = 12, height = 6, dpi = 130)

# ------------------------------------------------------------
# Plot 2 : 4 variables x 4 hotspots (matrice de séries lissées)
# ------------------------------------------------------------
# Anomalies désaisonnées pour éliminer le bruit saisonnier
df_long <- df |>
  pivot_longer(-c(date, year, month),
               names_to = "var_full", values_to = "value") |>
  separate(var_full, into = c("region", "var"), sep = "_(?=[A-Z])") |>
  group_by(region, var, month) |>
  mutate(anom = value - mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(region = factor(region, levels = names(REGIONS)),
         var    = factor(var, levels = unname(KEY_VARS)))

p2 <- ggplot(df_long, aes(date, anom, color = region)) +
  geom_line(alpha = 0.35, linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE,
              color = "black", linewidth = 1) +
  facet_grid(region ~ var, scales = "free_y", switch = "y") +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(title    = "Anomalies climatiques sur les 4 hotspots (0.5°)",
       subtitle = "Désaisonnées : valeur − climatologie mensuelle 1979-2025  |  ligne noire = lissage LOESS (tendance)",
       x = NULL, y = "Anomalie (unité variable)") +
  theme(strip.placement = "outside",
        strip.text.y    = element_text(angle = 0, face = "bold", hjust = 0),
        strip.text.x    = element_text(face = "bold", size = 11),
        panel.spacing.x = grid::unit(0.8, "lines"),
        panel.spacing.y = grid::unit(0.5, "lines"),
        axis.text       = element_text(size = 9))
ggsave(file.path(PLOT, "06b_4vars_hotspots.png"), p2,
       width = 16, height = 11, dpi = 140)

# ------------------------------------------------------------
# Tableau : tendance Sen et corrélation avec CO2_trend résiduel
# ------------------------------------------------------------
results <- list()
for (rname in names(REGIONS)) {
  for (short in unname(KEY_VARS)) {
    col <- paste0(rname, "_", short)
    x   <- df[[col]]
    sen <- unname(sens.slope(x)$estimates) * 12
    mk  <- MannKendall(x)
    # Résidu de x
    x_clim <- tapply(x, months, mean, na.rm = TRUE)
    x_anom <- x - x_clim[months]
    x_resid <- residuals(lm(x_anom ~ t_yrs, na.action = na.exclude))
    r <- cor(x_resid, co2_resid, use = "complete.obs")
    results[[col]] <- data.frame(
      region = rname, var = short,
      sen_per_year = sen, mk_pvalue = as.numeric(mk$sl),
      r_with_co2_resid = r,
      row.names = NULL
    )
  }
}
res_df <- bind_rows(results)
print(res_df, digits = 3)
write.csv(res_df, file.path(OUT_DIR, "hotspots_summary.csv"), row.names = FALSE)

cat("\n=== Hotspots terminés. Fichiers :\n")
cat(" - outputs/hotspots_series.csv\n")
cat(" - outputs/hotspots_summary.csv\n")
cat(" - outputs/plots/06a_T2m_hotspots.png\n")
cat(" - outputs/plots/06b_4vars_hotspots.png\n")
