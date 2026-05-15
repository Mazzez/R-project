# ============================================================
# 06_correlations.R
# Calcule les corrélations climat ↔ CO2 sur 4 représentations
# (level, anom, d1, d12) et fait une analyse en lag pour
# les variables les plus pertinentes.
#
# Entrée : Analyse Climat 2.5°x2.5°/outputs/series_transformed.rds
# Sortie : Analyse Climat 2.5°x2.5°/outputs/correlations_4repr.csv
#          Analyse Climat 2.5°x2.5°/outputs/lag_correlations.csv
#          Analyse Climat 2.5°x2.5°/outputs/plots/06a_corr_heatmap.png
#          Analyse Climat 2.5°x2.5°/outputs/plots/06b_corr_climat_co2.png
#          Analyse Climat 2.5°x2.5°/outputs/plots/06c_lag_correlations.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(corrplot)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
ds      <- readRDS(file.path(OUT_DIR, "series_transformed.rds"))
theme_set(theme_minimal(base_size = 12))

clim_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
               "CRE_SW","CRE_LW","CRE_net")
co2_var   <- "co2_trend"   # cible désaisonnée NOAA pour les niveaux/anom
co2_var2  <- "co2_avg"     # série brute pour les diff (plus naturel)

# ============================================================
# 1. Corrélations climat ↔ CO2 sur les 5 représentations
# ============================================================
get_co2_target <- function(repr) if (repr %in% c("d1","d12")) co2_var2 else co2_var

cor_repr <- lapply(names(ds), function(repr) {
  d <- ds[[repr]]
  y <- d[[get_co2_target(repr)]]
  data.frame(
    repr = repr,
    var  = clim_vars,
    r    = sapply(clim_vars, function(v)
              suppressWarnings(cor(d[[v]], y, use = "complete.obs")))
  )
}) |> bind_rows()

cor_wide <- cor_repr |>
  pivot_wider(names_from = repr, values_from = r) |>
  arrange(desc(abs(resid)))

cat("=== Corrélations climat ↔ CO2 sur les 5 représentations ===\n")
cat(" level : niveaux bruts (dominé par tendance commune)\n")
cat(" anom  : anomalies désaisonnées (cycle saisonnier retiré, tendance conservée)\n")
cat(" resid : anomalies désaisonnées ET détendrées (signal interannuel pur)  <-- KEY\n")
cat(" d1    : différences mensuelles (capture surtout la saisonnalité)\n")
cat(" d12   : différences annuelles (taux d'évolution interannuel)\n\n")
print(cor_wide, digits = 3, n = 30)
write.csv(cor_wide, file.path(OUT_DIR, "correlations_4repr.csv"),
          row.names = FALSE)

# Plot comparatif des 5 représentations
cor_long <- cor_repr |>
  mutate(repr = factor(repr, levels = c("level","anom","resid","d1","d12")),
         var  = factor(var,
                       levels = cor_wide$var))   # ordonné par |r(resid)| décroissant

p_cmp <- ggplot(cor_long, aes(var, r, fill = repr)) +
  geom_col(position = position_dodge(0.8), width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(title    = "Corrélations climat ↔ CO2 — comparaison des 5 représentations",
       subtitle = "level/anom/resid : vs co2_trend ;  d1/d12 : vs Δco2",
       x = NULL, y = "r (Pearson)", fill = "Représentation") +
  theme(legend.position = "bottom")
ggsave(file.path(PLOT, "06b_corr_climat_co2.png"),
       p_cmp, width = 11, height = 8, dpi = 130)

# ============================================================
# 2. Heatmap de la matrice de corrélation complète sur les RÉSIDUS
#    (anomalies désaisonnées et détendrées : le seul signal "propre"
#    pour étudier les liens interannuels)
# ============================================================
all_vars <- c("co2_trend", clim_vars)
M <- cor(ds$resid[, all_vars], use = "complete.obs")
png(file.path(PLOT, "06a_corr_heatmap.png"),
    width = 1600, height = 1500, res = 150)
corrplot(M, method = "color", type = "lower",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.55,
         col = colorRampPalette(c("steelblue", "white", "tomato"))(200),
         title = "Matrice de corrélation sur résidus (anomalies désaisonnées + détendrées)",
         mar = c(0,0,2,0))
dev.off()

# ============================================================
# 3. Analyse en lag : pour les 6 variables les plus corrélées sur
#    les RÉSIDUS, tester des lags de -12 à +12 mois.
#    Lag positif : la variable précède le CO2 (climat → CO2)
#    Lag négatif : le CO2 précède la variable (CO2 → climat)
# ============================================================
top6 <- cor_wide |> slice_max(abs(resid), n = 6) |> pull(var)
cat("\n=== Top 6 variables (résidus) pour lag analysis ===\n")
print(top6)

lag_corr <- function(x, y, max_lag = 12) {
  data.frame(lag = -max_lag:max_lag,
             r = sapply(-max_lag:max_lag, function(L) {
               if (L > 0) {
                 # x avance de L mois : x_{t-L} vs y_t  → x précède y
                 cor(x[seq_len(length(x) - L)],
                     y[(L + 1):length(y)], use = "complete.obs")
               } else if (L < 0) {
                 # y avance de |L| : x_t vs y_{t-|L|}  → y précède x
                 L2 <- -L
                 cor(x[(L2 + 1):length(x)],
                     y[seq_len(length(y) - L2)], use = "complete.obs")
               } else {
                 cor(x, y, use = "complete.obs")
               }
             }))
}

lag_results <- lapply(top6, function(v) {
  r <- lag_corr(ds$resid[[v]], ds$resid[[co2_var]])
  r$var <- v
  r
}) |> bind_rows() |>
  mutate(var = factor(var, levels = top6))

write.csv(lag_results, file.path(OUT_DIR, "lag_correlations.csv"),
          row.names = FALSE)

cat("\n=== Pic absolu de corrélation par variable (sur résidus) ===\n")
print(lag_results |>
        group_by(var) |>
        slice_max(abs(r), n = 1) |>
        ungroup())

p_lag <- ggplot(lag_results, aes(lag, r, color = var)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ var, scales = "free_y") +
  labs(title    = "Corrélations climat ↔ CO2 en fonction du lag (sur résidus)",
       subtitle = "Lag > 0 : la variable précède le CO2  |  Lag < 0 : le CO2 précède la variable",
       x = "Lag (mois)", y = "r (Pearson)") +
  theme(legend.position = "none")
ggsave(file.path(PLOT, "06c_lag_correlations.png"),
       p_lag, width = 12, height = 8, dpi = 130)

cat("\n=== Plots et CSV sauvegardés dans :", OUT_DIR, "===\n")
