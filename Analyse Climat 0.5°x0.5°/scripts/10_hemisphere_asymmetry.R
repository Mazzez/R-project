# ============================================================
# 10_hemisphere_asymmetry.R  (résolution 0.5° × 0.5°)
# Asymétrie hémisphérique du cycle saisonnier — lien avec
# le ratio CO2 MLO/SPO = 5.6× de la phase 1.
#
# Idée : le cycle saisonnier T2m est mécaniquement plus fort
# au nord qu'au sud (plus de continents au N → moins d'inertie
# océanique). On quantifie ce ratio par bande de latitude.
#
# Comparaison phase 1 :
#   - CO2 saisonnier MLO (Mauna Loa, Hawaii)  / SPO (Pôle Sud) ≈ 5.6
#   - Climat saisonnier Boréale (60-90°N)    / Australe (90-60°S) ?
#
# Sortie : outputs/hemisphere_asymmetry.csv
#          outputs/plots/08_hemisphere_asymmetry.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
theme_set(theme_minimal(base_size = 11))

df <- read.csv(file.path(OUT_DIR, "monthly_band_means_05.csv")) |>
  mutate(date = as.Date(date),
         band = factor(band,
                       levels = c("austral","temperate_S","tropical",
                                  "temperate_N","boreal","global"),
                       labels = c("Australe (90-60°S)", "Tempérée S (60-30°S)",
                                  "Tropicale (30°S-30°N)", "Tempérée N (30-60°N)",
                                  "Boréale (60-90°N)", "Global")))

vars <- c("T2m","PWAT","DSWRF","TCDC")

# ------------------------------------------------------------
# 1. Climatologie mensuelle par bande × variable
# ------------------------------------------------------------
clim_long <- df |>
  filter(band != "Global") |>
  pivot_longer(all_of(vars), names_to = "var", values_to = "value") |>
  group_by(band, var, month) |>
  summarise(mean_clim = mean(value, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------
# 2. Amplitude saisonnière = max(clim) - min(clim) par bande × var
# ------------------------------------------------------------
amp <- clim_long |>
  group_by(band, var) |>
  summarise(amplitude = max(mean_clim) - min(mean_clim), .groups = "drop")

amp_wide <- amp |> pivot_wider(names_from = band, values_from = amplitude)
print(amp_wide, digits = 4)

# Ratio Boréale / Australe (équivalent climat de MLO/SPO)
ratio <- amp |>
  filter(band %in% c("Boréale (60-90°N)", "Australe (90-60°S)")) |>
  pivot_wider(names_from = band, values_from = amplitude) |>
  mutate(ratio_N_over_S = `Boréale (60-90°N)` / `Australe (90-60°S)`)

cat("\n=== Ratio amplitude saisonnière Boréale / Australe ===\n")
print(ratio, row.names = FALSE)

# Référence phase 1
ratio_co2_mlo_spo <- 5.6
cat(sprintf("\nRéférence phase 1 : MLO/SPO CO2 saisonnier = %.1f×\n",
            ratio_co2_mlo_spo))

# ------------------------------------------------------------
# 3. Sauvegarde CSV
# ------------------------------------------------------------
write.csv(amp,   file.path(OUT_DIR, "hemisphere_asymmetry.csv"),
          row.names = FALSE)
write.csv(ratio, file.path(OUT_DIR, "hemisphere_asymmetry_ratio.csv"),
          row.names = FALSE)

# ------------------------------------------------------------
# 4. Plot : climatologies mensuelles superposées par bande
# (pour visualiser l'asymétrie du cycle saisonnier T2m)
# ------------------------------------------------------------
clim_t2m <- clim_long |> filter(var == "T2m") |>
  group_by(band) |>
  mutate(anom = mean_clim - mean(mean_clim)) |>
  ungroup()

p1 <- ggplot(clim_t2m, aes(month, anom, color = band)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_color_brewer(palette = "Set1") +
  labs(title    = "Cycle saisonnier de T2m par bande de latitude",
       subtitle = "Anomalie par rapport à la moyenne annuelle de chaque bande — 1979-2025",
       x = "Mois", y = "Anomalie T2m (K)", color = "Bande") +
  theme(legend.position = "bottom")

# Plot 2 : barres comparatives amplitudes
amp_t2m <- amp |> filter(var == "T2m") |>
  mutate(band = factor(band, levels = c("Boréale (60-90°N)", "Tempérée N (30-60°N)",
                                         "Tropicale (30°S-30°N)", "Tempérée S (60-30°S)",
                                         "Australe (90-60°S)")))
p2 <- ggplot(amp_t2m, aes(band, amplitude, fill = band)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f K", amplitude)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(title = sprintf("Amplitude saisonnière T2m par bande — ratio N/S = %.2f×",
                       ratio$ratio_N_over_S),
       subtitle = sprintf("À comparer au ratio CO2 saisonnier MLO/SPO de la phase 1 = %.1f×",
                          ratio_co2_mlo_spo),
       x = NULL, y = "Amplitude saisonnière (K)") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, face = "bold"))

# Assembler en deux panneaux
library(patchwork, quietly = TRUE)
combined <- p1 / p2 +
  plot_layout(heights = c(1, 1))
ggsave(file.path(PLOT, "08_hemisphere_asymmetry.png"), combined,
       width = 12, height = 10, dpi = 140)

cat("\n=== Plot sauvegardé :",
    file.path(PLOT, "08_hemisphere_asymmetry.png"), "===\n")
