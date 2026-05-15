# ============================================================
# 10_per_variable_analysis.R
# Pour chacune des 18 variables (+ 3 CRE), produire une fiche
# d'analyse complète :
#   1. Série temporelle + lissage LOESS
#   2. Décomposition STL (observed / trend / seasonal / remainder)
#   3. Climatologie saisonnière + amplitude par décennie
#   4. Heatmap mois × année des anomalies
#   5. Tests de tendance Mann-Kendall + pente Sen
#   6. Table statistique synthétique
#
# Sortie :
#   outputs/per_variable/<VAR>/01..04*.png
#   outputs/per_variable_stats.csv  (table résumée 21 lignes)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(Kendall); library(trend); library(lubridate)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PV      <- file.path(OUT_DIR, "per_variable")
dir.create(PV, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 11))

df <- read.csv(file.path(OUT_DIR, "climate_co2_monthly.csv")) |>
  mutate(date = as.Date(date))

# Liste des variables à analyser : 18 climat + 3 CRE
all_vars <- c(
  "T2m","T500","SPFH2m","PWAT","APCP","TCDC",
  "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
  "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
  "CRE_SW","CRE_LW","CRE_net"
)

# Métadonnées (unités + nom long) pour les axes/titres
META <- tibble::tribble(
  ~code,     ~unit,        ~long,
  "T2m",     "K",          "Température air 2 m",
  "T500",    "K",          "Température 500 hPa",
  "SPFH2m",  "kg/kg",      "Humidité spécifique 2 m",
  "PWAT",    "kg/m²",      "Eau précipitable colonne",
  "APCP",    "kg/m²",      "Précipitations cumulées",
  "TCDC",    "%",          "Couverture nuageuse totale",
  "DLWRF",   "W/m²",       "LW descendant surface",
  "ULWRF",   "W/m²",       "LW ascendant surface",
  "DSWRF",   "W/m²",       "SW descendant surface",
  "USWRF",   "W/m²",       "SW réfléchi surface",
  "PRMSL",   "Pa",         "Pression réduite mer",
  "CSDSF",   "W/m²",       "SW descendant ciel clair",
  "CSUSF",   "W/m²",       "SW ascendant ciel clair",
  "CSDLF",   "W/m²",       "LW descendant ciel clair",
  "CSULF",   "W/m²",       "LW ascendant ciel clair",
  "CDUVB",   "W/m²",       "UV-B ciel clair",
  "DUVB",    "W/m²",       "UV-B all-sky",
  "ALBDO",   "%",          "Albédo de surface",
  "CRE_SW",  "W/m²",       "Cloud Radiative Effect SW",
  "CRE_LW",  "W/m²",       "Cloud Radiative Effect LW",
  "CRE_net", "W/m²",       "Cloud Radiative Effect net"
)

# ------------------------------------------------------------
# Fonction principale : génère les 4 plots et le ligne de stats
# ------------------------------------------------------------
analyse_variable <- function(v) {
  meta  <- META[META$code == v, ]
  unit  <- meta$unit
  long  <- meta$long

  d <- df |>
    select(date, year, month, value = all_of(v)) |>
    mutate(decade = paste0(floor(year / 10) * 10, "s"))

  vdir <- file.path(PV, v)
  dir.create(vdir, showWarnings = FALSE, recursive = TRUE)

  # --- 1. Série temporelle + LOESS ---
  p1 <- ggplot(d, aes(date, value)) +
    geom_line(color = "steelblue", alpha = 0.6) +
    geom_smooth(method = "loess", span = 0.2, se = FALSE,
                color = "darkred", linewidth = 0.9) +
    labs(title    = sprintf("%s — série temporelle 1979-2025", v),
         subtitle = sprintf("%s (%s) ; lissage LOESS span=0.2 en rouge", long, unit),
         x = NULL, y = sprintf("%s (%s)", v, unit))
  ggsave(file.path(vdir, "01_timeseries_loess.png"),
         p1, width = 10, height = 5, dpi = 130)

  # --- 2. Décomposition STL ---
  ts_v <- ts(d$value, start = c(d$year[1], d$month[1]), frequency = 12)
  stl_fit <- stl(ts_v, s.window = "periodic", robust = TRUE)
  stl_df  <- as.data.frame(stl_fit$time.series) |>
    mutate(date = d$date, observed = d$value)
  amp_seasonal <- diff(range(stl_df$seasonal))

  p2 <- stl_df |>
    pivot_longer(c(observed, trend),
                 names_to = "comp", values_to = "val") |>
    mutate(comp = factor(comp, levels = c("observed","trend"))) |>
    ggplot(aes(date, val)) +
    geom_line(color = "steelblue") +
    facet_wrap(~ comp, scales = "free_y", ncol = 1) +
    labs(title    = sprintf("%s — décomposition STL (observed + trend)", v),
         subtitle = sprintf("Amplitude saisonnière (calculée mais non affichée) = %.3f %s",
                            amp_seasonal, unit),
         x = NULL, y = sprintf("%s (%s)", v, unit))
  ggsave(file.path(vdir, "02_stl_decomposition.png"),
         p2, width = 10, height = 5, dpi = 130)

  # --- 3. Climatologie saisonnière + amplitude par décennie ---
  clim <- d |>
    group_by(month) |>
    summarise(mean = mean(value), sd = sd(value), .groups = "drop")
  amp_dec <- d |>
    group_by(decade) |>
    summarise(amp = max(value) - min(value), .groups = "drop")

  p3a <- ggplot(clim, aes(month, mean)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
                fill = "steelblue", alpha = 0.2) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    labs(title    = sprintf("%s — climatologie mensuelle", v),
         subtitle = "Moyenne mensuelle ± 1 sd (1979-2025)",
         x = "Mois", y = sprintf("%s (%s)", v, unit))

  p3b <- ggplot(amp_dec, aes(decade, amp)) +
    geom_col(fill = "tomato") +
    geom_text(aes(label = signif(amp, 3)), vjust = -0.4) +
    labs(title = sprintf("%s — amplitude (max-min) par décennie", v),
         x = NULL, y = sprintf("%s (%s)", v, unit))

  # Combinaison verticale
  p3 <- patchwork::wrap_plots(p3a, p3b, ncol = 1, heights = c(1, 0.7))
  ggsave(file.path(vdir, "03_seasonal_climato.png"),
         p3, width = 10, height = 8, dpi = 130)

  # --- 4. Heatmap mois × année ---
  # Anomalie = value - moyenne mensuelle (climato)
  d_anom <- d |>
    mutate(anom = value - clim$mean[match(month, clim$month)])

  p4 <- ggplot(d_anom, aes(month, year, fill = anom)) +
    geom_tile() +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                         midpoint = 0,
                         name = sprintf("anom\n(%s)", unit)) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_reverse(breaks = seq(1980, 2025, 5)) +
    labs(title    = sprintf("%s — heatmap mois × année des anomalies", v),
         subtitle = "Anomalie = valeur − climatologie mensuelle",
         x = "Mois", y = "Année")
  ggsave(file.path(vdir, "04_heatmap_anomaly.png"),
         p4, width = 8, height = 9, dpi = 130)

  # --- 5. Tests de tendance ---
  mk  <- MannKendall(d$value)
  sen <- sens.slope(d$value)
  sen_per_year <- sen$estimates * 12   # mensuel -> annuel

  # --- 6. Stats résumées ---
  data.frame(
    var               = v,
    long_name         = long,
    unit              = unit,
    n_obs             = nrow(d),
    min               = min(d$value),
    mean              = mean(d$value),
    max               = max(d$value),
    sd                = sd(d$value),
    range             = max(d$value) - min(d$value),
    sen_per_year      = sen_per_year,
    sen_total         = sen_per_year * (nrow(d) / 12),
    mk_tau            = mk$tau,
    mk_pvalue         = mk$sl,
    seasonal_amp_stl  = amp_seasonal,
    amp_decade_first  = amp_dec$amp[1],
    amp_decade_last   = amp_dec$amp[nrow(amp_dec)],
    stringsAsFactors  = FALSE
  )
}

# ------------------------------------------------------------
# Boucle principale
# ------------------------------------------------------------
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

cat("Génération des fiches par variable :\n")
all_stats <- list()
t0 <- Sys.time()
for (i in seq_along(all_vars)) {
  v <- all_vars[i]
  cat(sprintf("  [%2d/%2d] %s\n", i, length(all_vars), v))
  all_stats[[v]] <- analyse_variable(v)
}
cat("\nDurée totale :",
    round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

stats_df <- bind_rows(all_stats)
write.csv(stats_df, file.path(OUT_DIR, "per_variable_stats.csv"),
          row.names = FALSE)

cat("\n=== Statistiques par variable ===\n")
print(stats_df, digits = 4)

cat(sprintf("\nFiches sauvegardées dans : %s\n", PV))
cat("Stats résumées          :",
    file.path(OUT_DIR, "per_variable_stats.csv"), "\n")
