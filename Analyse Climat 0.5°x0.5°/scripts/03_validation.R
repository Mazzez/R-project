# ============================================================
# 03_validation.R  (résolution 0.5° × 0.5°)
# Sanity checks et plots des séries par bande de latitude.
#
# Sortie : Analyse Climat 0.5°x0.5°/outputs/plots/0[1-3]*.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
dir.create(PLOT, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 11))

df <- read.csv(file.path(OUT_DIR, "monthly_band_means_05.csv")) |>
  mutate(date = as.Date(date),
         band = factor(band,
                       levels = c("austral","temperate_S","tropical",
                                  "temperate_N","boreal","global"),
                       labels = c("Australe (90-60°S)", "Tempérée S (60-30°S)",
                                  "Tropicale (30°S-30°N)", "Tempérée N (30-60°N)",
                                  "Boréale (60-90°N)", "Global")))

cat("Tableau chargé :", paste(dim(df), collapse = " x "), "\n")
cat("Bandes         :", paste(levels(df$band), collapse = " ; "), "\n\n")

vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
          "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
          "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO")

# ============================================================
# 1. Statistiques descriptives par bande
# ============================================================
stats <- df |>
  pivot_longer(all_of(vars), names_to = "var", values_to = "value") |>
  group_by(band, var) |>
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

cat("=== Moyennes par bande (extrait) ===\n")
print(stats |> filter(var %in% c("T2m","PWAT","DSWRF","TCDC")) |>
        pivot_wider(names_from = var, values_from = c(mean, sd)),
      digits = 4)

write.csv(stats, file.path(OUT_DIR, "stats_par_bande.csv"), row.names = FALSE)

# ============================================================
# 2. Anomalies désaisonnées de T2m, PWAT, DSWRF, TCDC par bande
# Grille 5 bandes × 4 variables = 20 panneaux séparés.
# On retire la climatologie mensuelle pour ÉLIMINER LE SIGNAL
# SAISONNIER et laisser apparaître clairement les tendances.
# ============================================================
key_vars <- c("T2m","PWAT","DSWRF","TCDC")
df_anom <- df |>
  filter(band != "Global") |>
  pivot_longer(all_of(key_vars), names_to = "var", values_to = "value") |>
  group_by(band, var, month) |>
  mutate(anom = value - mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(var = factor(var, levels = key_vars))

p1 <- ggplot(df_anom, aes(date, anom, color = band)) +
  geom_line(alpha = 0.35, linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE,
              color = "black", linewidth = 1) +
  facet_grid(band ~ var, scales = "free_y", switch = "y") +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(title    = "Anomalies climatiques par bande de latitude (0.5°)",
       subtitle = "Désaisonnées : valeur − climatologie mensuelle, 1979-2025  |  ligne noire = lissage LOESS (tendance)",
       x = NULL, y = "Anomalie (unité variable)") +
  theme(strip.placement = "outside",
        strip.text.y    = element_text(angle = 0, face = "bold", hjust = 0),
        strip.text.x    = element_text(face = "bold", size = 11),
        panel.spacing.x = grid::unit(0.8, "lines"),
        panel.spacing.y = grid::unit(0.5, "lines"),
        axis.text       = element_text(size = 9))
ggsave(file.path(PLOT, "01_series_par_bande.png"), p1,
       width = 16, height = 12, dpi = 140)

# ============================================================
# 3. Toutes les 18 variables, série globale 0.5°
# ============================================================
df_global <- df |> filter(band == "Global")

p2 <- df_global |>
  pivot_longer(all_of(vars), names_to = "var", values_to = "value") |>
  mutate(var = factor(var, levels = vars)) |>
  ggplot(aes(date, value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ var, scales = "free_y", ncol = 4) +
  labs(title    = "Moyennes globales 0.5° des 18 variables",
       subtitle = "Pour comparaison directe avec le 2.5°",
       x = NULL, y = NULL)
ggsave(file.path(PLOT, "02_series_18_global_05.png"), p2,
       width = 16, height = 10, dpi = 130)

# ============================================================
# 4. T2m par bande — focus sur le réchauffement amplifié à hautes latitudes
# ============================================================
df_t2m <- df |>
  filter(band != "Global") |>
  mutate(year = format(date, "%Y") |> as.integer())

# Anomalie T2m = T2m - climatologie mensuelle
clim <- df_t2m |>
  group_by(band, month) |>
  summarise(clim_T2m = mean(T2m), .groups = "drop")
df_anom <- df_t2m |>
  left_join(clim, by = c("band","month")) |>
  mutate(anom = T2m - clim_T2m)

p3 <- ggplot(df_anom, aes(date, anom, color = band)) +
  geom_line(alpha = 0.4, linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_brewer(palette = "Set1") +
  labs(title    = "Anomalie de T2m par bande de latitude (lissage LOESS)",
       subtitle = "Réchauffement amplifié attendu en bande boréale (Arctic amplification)",
       x = NULL, y = "Anomalie T2m (K)", color = "Bande") +
  theme(legend.position = "bottom")
ggsave(file.path(PLOT, "03_T2m_anomaly_par_bande.png"), p3,
       width = 12, height = 7, dpi = 130)

# ============================================================
# 4 bis. Signature effet de serre : DLWRF + CSDLF par bande
# (anomalies désaisonnées + lissage LOESS)
# CSDLF (ciel-clair) = forçage GES PUR, non pollué par les nuages
# DLWRF (all-sky)    = forçage GES + variations nuageuses
# ============================================================
ges_vars <- c("DLWRF", "CSDLF")
df_ges <- df |>
  filter(band != "Global") |>
  pivot_longer(all_of(ges_vars), names_to = "var", values_to = "value") |>
  group_by(band, var, month) |>
  mutate(anom = value - mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(var = factor(var, levels = ges_vars,
                      labels = c("DLWRF (LW all-sky)",
                                 "CSDLF (LW ciel-clair — signature GES pure)")))

p_ges <- ggplot(df_ges, aes(date, anom, color = band)) +
  geom_line(alpha = 0.35, linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE,
              color = "black", linewidth = 1) +
  facet_grid(band ~ var, scales = "free_y", switch = "y") +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(title    = "Signature de l'effet de serre par bande de latitude",
       subtitle = "Anomalies désaisonnées de DLWRF et CSDLF (IR descendant en surface) — ligne noire = LOESS",
       x = NULL, y = "Anomalie (W/m²)") +
  theme(strip.placement = "outside",
        strip.text.y    = element_text(angle = 0, face = "bold", hjust = 0),
        strip.text.x    = element_text(face = "bold", size = 10),
        panel.spacing.x = grid::unit(0.8, "lines"),
        panel.spacing.y = grid::unit(0.5, "lines"),
        axis.text       = element_text(size = 9))
ggsave(file.path(PLOT, "04_GES_signature.png"), p_ges,
       width = 13, height = 11, dpi = 140)

# ============================================================
# 5. Synthèse écart de tendance par bande
# ============================================================
trend_per_band <- df |>
  filter(band != "Global") |>
  group_by(band) |>
  summarise(
    sen_T2m   = unname(trend::sens.slope(T2m)$estimates)   * 12,
    sen_PWAT  = unname(trend::sens.slope(PWAT)$estimates)  * 12,
    sen_DSWRF = unname(trend::sens.slope(DSWRF)$estimates) * 12,
    sen_TCDC  = unname(trend::sens.slope(TCDC)$estimates)  * 12,
    sen_DLWRF = unname(trend::sens.slope(DLWRF)$estimates) * 12,
    sen_CSDLF = unname(trend::sens.slope(CSDLF)$estimates) * 12,
    .groups = "drop"
  )
cat("\n=== Tendances Sen annualisées par bande ===\n")
print(trend_per_band)
write.csv(trend_per_band, file.path(OUT_DIR, "trends_par_bande.csv"),
          row.names = FALSE)

cat("\n=== Plots sauvegardés dans :", PLOT, "===\n")
