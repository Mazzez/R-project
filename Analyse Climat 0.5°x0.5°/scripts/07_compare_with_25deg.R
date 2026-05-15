# ============================================================
# 07_compare_with_25deg.R
# Validation croisée : comparer les moyennes globales du 0.5°
# (calculées dans 02_band_means.R, ligne band == "global") avec
# celles du 2.5° (calculées par Analyse Climat 2.5°x2.5°/scripts/02).
#
# Si tout est correct, les écarts doivent être de l'ordre de 0.01-0.05 %
# pour les variables thermo et < 1 % pour les autres.
#
# Sortie : Analyse Climat 0.5°x0.5°/outputs/comparison_05_vs_25.csv
#          Analyse Climat 0.5°x0.5°/outputs/plots/07_comparison.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

DIR_05 <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
DIR_25 <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT   <- file.path(DIR_05, "plots")
theme_set(theme_minimal(base_size = 11))

# Charger 0.5° (band == "global")
d05 <- read.csv(file.path(DIR_05, "monthly_band_means_05.csv")) |>
  filter(band == "global") |>
  mutate(date = as.Date(date)) |>
  select(-band)

d25 <- read.csv(file.path(DIR_25, "monthly_global_means_25.csv")) |>
  mutate(date = as.Date(date))

cat("0.5° global  :", nrow(d05), "lignes,", ncol(d05), "colonnes\n")
cat("2.5° global  :", nrow(d25), "lignes,", ncol(d25), "colonnes\n")

vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
          "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
          "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO")

# Jointure
both <- inner_join(d05, d25, by = c("date","year","month"),
                   suffix = c("_05","_25"))
cat("\nPériode commune :", as.character(min(both$date)), "->",
    as.character(max(both$date)), "(", nrow(both), "mois)\n\n")

# Statistiques d'écart par variable
gap_stats <- bind_rows(lapply(vars, function(v) {
  v05 <- both[[paste0(v, "_05")]]
  v25 <- both[[paste0(v, "_25")]]
  diff <- v05 - v25
  rel  <- diff / v25 * 100
  data.frame(
    var               = v,
    mean_05           = mean(v05, na.rm = TRUE),
    mean_25           = mean(v25, na.rm = TRUE),
    abs_diff_mean     = mean(abs(diff), na.rm = TRUE),
    abs_diff_max      = max(abs(diff),  na.rm = TRUE),
    rel_diff_mean_pct = mean(abs(rel),  na.rm = TRUE),
    correlation       = cor(v05, v25,   use = "complete.obs")
  )
}))

cat("=== Comparaison 0.5° vs 2.5° (moyennes globales) ===\n")
print(gap_stats, digits = 4)
write.csv(gap_stats, file.path(DIR_05, "comparison_05_vs_25.csv"),
          row.names = FALSE)

# Plot : superposition pour 4 variables clés + écart relatif
key <- c("T2m","PWAT","DSWRF","TCDC")
df_long <- both |>
  select(date, all_of(c(paste0(key, "_05"), paste0(key, "_25")))) |>
  pivot_longer(-date, names_to = "key", values_to = "value") |>
  separate(key, into = c("var","resol"), sep = "_") |>
  mutate(resol = recode(resol, "05" = "0.5°", "25" = "2.5°"))

p <- ggplot(df_long, aes(date, value, color = resol)) +
  geom_line(alpha = 0.7, linewidth = 0.4) +
  facet_wrap(~ var, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("0.5°" = "tomato", "2.5°" = "steelblue")) +
  labs(title    = "Moyennes globales : 0.5° vs 2.5°",
       subtitle = "Validation croisée des deux pipelines",
       x = NULL, y = NULL, color = "Résolution") +
  theme(legend.position = "bottom")
ggsave(file.path(PLOT, "07_comparison.png"), p,
       width = 14, height = 8, dpi = 130)

cat("\n=== Plot : 07_comparison.png ===\n")
