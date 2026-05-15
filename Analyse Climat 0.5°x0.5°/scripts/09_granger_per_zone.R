# ============================================================
# 09_granger_per_zone.R  (résolution 0.5° × 0.5°)
# Tests de causalité Granger climat ↔ CO2 par bande et par hotspot.
#
# Méthode (analogue script 08_granger.R du 2.5°) :
#   - Représentation d12 (taux annuel) = X_t − X_{t-12}
#   - lag = 6 mois
#   - Pour chaque zone (bande ou hotspot) et chaque variable :
#       grangertest(co2_d12 ~ X_d12, order = 6)  # X → CO2
#       grangertest(X_d12 ~ co2_d12, order = 6)  # CO2 → X
#
# Sortie : outputs/granger_per_zone.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lmtest)
})

OUT_DIR  <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
CO2_FILE <- "/home/mazzez/Bureau/R project/Final Version/CO2/co2_mm_gl.csv"
LAG      <- 6

bands_df <- read.csv(file.path(OUT_DIR, "monthly_band_means_05.csv")) |>
  mutate(date = as.Date(date)) |> arrange(date)
hot_df   <- read.csv(file.path(OUT_DIR, "hotspots_series.csv")) |>
  mutate(date = as.Date(date)) |> arrange(date)

co2 <- read.csv(CO2_FILE, comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  select(date, co2_trend = trend)

dates <- sort(unique(bands_df$date))
co2_v <- co2$co2_trend[match(dates, co2$date)]

# d12 : x_t - x_{t-12}
d12 <- function(x) c(rep(NA, 12), x[-seq_len(12)] - x[seq_len(length(x) - 12)])

co2_d12 <- d12(co2_v)

# Helper test Granger bidirectionnel
granger_xy <- function(x, y, lag) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 50) return(c(p_x_to_y = NA, p_y_to_x = NA))
  x <- x[ok]; y <- y[ok]
  p_xy <- tryCatch(grangertest(y ~ x, order = lag)$`Pr(>F)`[2],
                   error = function(e) NA)
  p_yx <- tryCatch(grangertest(x ~ y, order = lag)$`Pr(>F)`[2],
                   error = function(e) NA)
  c(p_x_to_y = p_xy, p_y_to_x = p_yx)
}

# ------------------------------------------------------------
# Bandes de latitude
# ------------------------------------------------------------
clim_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO")

bands_list <- unique(bands_df$band)
results <- list()

for (b in bands_list) {
  sub <- bands_df |> filter(band == b) |> arrange(date)
  for (v in clim_vars) {
    x_d12 <- d12(sub[[v]])
    pv <- granger_xy(x_d12, co2_d12, lag = LAG)
    results[[paste0(b, "_", v)]] <- data.frame(
      zone        = b, type = "bande", var = v,
      p_x_to_co2  = pv["p_x_to_y"],
      p_co2_to_x  = pv["p_y_to_x"],
      sens        = case_when(
        pv["p_x_to_y"] < 0.05 & pv["p_y_to_x"] < 0.05 ~ "bidirectional",
        pv["p_x_to_y"] < 0.05                           ~ "X -> CO2",
        pv["p_y_to_x"] < 0.05                           ~ "CO2 -> X",
        TRUE                                            ~ "none"
      ),
      row.names = NULL
    )
  }
}

# ------------------------------------------------------------
# Hotspots
# ------------------------------------------------------------
hot_vars <- c("T2m","PWAT","APCP","TCDC")
regions  <- c("Amazonie","Indonesie","Siberie","Sahel")

for (r in regions) {
  for (v in hot_vars) {
    x_d12 <- d12(hot_df[[paste0(r, "_", v)]])
    pv <- granger_xy(x_d12, co2_d12, lag = LAG)
    results[[paste0(r, "_", v)]] <- data.frame(
      zone        = r, type = "hotspot", var = v,
      p_x_to_co2  = pv["p_x_to_y"],
      p_co2_to_x  = pv["p_y_to_x"],
      sens        = case_when(
        pv["p_x_to_y"] < 0.05 & pv["p_y_to_x"] < 0.05 ~ "bidirectional",
        pv["p_x_to_y"] < 0.05                           ~ "X -> CO2",
        pv["p_y_to_x"] < 0.05                           ~ "CO2 -> X",
        TRUE                                            ~ "none"
      ),
      row.names = NULL
    )
  }
}

gr <- bind_rows(results)
write.csv(gr, file.path(OUT_DIR, "granger_per_zone.csv"),
          row.names = FALSE)

# Synthèse : combien de variables climat → CO2 par zone
synth <- gr |>
  group_by(zone, type) |>
  summarise(
    n_vars         = n(),
    n_x_to_co2_sig = sum(p_x_to_co2 < 0.05, na.rm = TRUE),
    n_co2_to_x_sig = sum(p_co2_to_x < 0.05, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(type, desc(n_x_to_co2_sig))

cat("\n=== Synthèse Granger par zone (d12, lag 6) ===\n")
print(synth, row.names = FALSE)

cat("\n=== Détail sauvegardé :",
    file.path(OUT_DIR, "granger_per_zone.csv"), "===\n")
