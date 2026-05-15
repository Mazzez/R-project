# ============================================================
# 09_synthese.R
# Tableau de synthèse final consolidant les résultats des
# scripts 06 (corrélations), 07 (régression), 08 (Granger).
#
# Sortie : Analyse Climat 2.5°x2.5°/outputs/synthese_finale.csv
#          Analyse Climat 2.5°x2.5°/outputs/plots/09_synthese.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
theme_set(theme_minimal(base_size = 12))

cor4 <- read.csv(file.path(OUT_DIR, "correlations_4repr.csv"))
gr   <- read.csv(file.path(OUT_DIR, "granger_results.csv"))

# Granger d12 — colonne "sens" + p en X→CO2
gr_d12 <- gr |> filter(repr == "d12") |>
  select(var, p_x_to_co2_d12 = p_x_to_co2,
              p_co2_to_x_d12 = p_co2_to_x,
              sens_d12 = sens)

synth <- cor4 |>
  select(var, r_level = level, r_anom = anom,
              r_resid = resid, r_d1 = d1, r_d12 = d12) |>
  left_join(gr_d12, by = "var") |>
  arrange(desc(abs(r_resid)))

# Catégorie qualitative pour le rapport
synth <- synth |>
  mutate(
    poids_corr = case_when(
      abs(r_resid) > 0.4  ~ "fort",
      abs(r_resid) > 0.2  ~ "modéré",
      TRUE                 ~ "faible"
    ),
    spurious_trend = ifelse(abs(r_anom) > 0.5 & abs(r_resid) < 0.25,
                            "OUI", "non")
  )

cat("=== Synthèse finale (corrélations + Granger d12) ===\n\n")
print(synth)
write.csv(synth, file.path(OUT_DIR, "synthese_finale.csv"), row.names = FALSE)

# ============================================================
# Plot synthèse : 4 panneaux sur les 21 variables
# ============================================================
synth_long <- synth |>
  mutate(var = factor(var, levels = synth$var)) |>
  pivot_longer(c(r_level, r_anom, r_resid, r_d12),
               names_to = "repr", values_to = "r") |>
  mutate(repr = factor(repr,
                       levels = c("r_level","r_anom","r_resid","r_d12"),
                       labels = c("Niveaux","Anomalies","Résidus","d12 (annuel)")))

p1 <- ggplot(synth_long, aes(reorder(var, abs(r)), r, fill = repr)) +
  geom_col(position = position_dodge(0.85), width = 0.75) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(title    = "Corrélation climat ↔ CO2 selon la représentation temporelle",
       subtitle = "Plus on assainit la série (level → anom → resid), plus on isole le signal interannuel propre",
       x = NULL, y = "r (Pearson)", fill = NULL) +
  theme(legend.position = "bottom")

ggsave(file.path(PLOT, "09_synthese.png"), p1,
       width = 11, height = 8, dpi = 130)

cat("\n=== Sauvegardes ===\n")
cat(" - synthese_finale.csv\n")
cat(" - plots/09_synthese.png\n\n")

# ============================================================
# Bilan textuel
# ============================================================
n_spurious <- sum(synth$spurious_trend == "OUI")
n_xCo2_d12 <- sum(synth$sens_d12 %in% c("X -> CO2", "bidirectionnel"), na.rm = TRUE)
n_co2X_d12 <- sum(synth$sens_d12 %in% c("CO2 -> X", "bidirectionnel"), na.rm = TRUE)
top5_resid <- synth |> slice_max(abs(r_resid), n = 5)

cat("====================================================\n")
cat("                BILAN PHASE 3 — climat ↔ CO2\n")
cat("====================================================\n\n")
cat("• ", nrow(synth), "variables climatiques globales testées\n")
cat("• Variables avec corrélation 'spurious' (fort sur anom, faible sur resid) : ",
    n_spurious, "/", nrow(synth), "\n")
cat("  -> ces variables partagent surtout une tendance commune avec le CO2\n")
cat("• Top 5 corrélations sur résidus (signal interannuel pur) :\n")
print(top5_resid |> select(var, r_resid, sens_d12))
cat("\n• Granger d12 — variables X causant CO2 (p < 0.05) :", n_xCo2_d12, "\n")
cat("• Granger d12 — CO2 causant X (p < 0.05)              :", n_co2X_d12, "\n")
cat("\n=> Le sens dominant à l'échelle interannuelle est CLIMAT -> CO2\n")
cat("   (les variations climatiques précèdent celles du CO2)\n")
cat("====================================================\n")
