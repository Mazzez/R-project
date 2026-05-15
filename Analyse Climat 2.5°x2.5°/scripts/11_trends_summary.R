# ============================================================
# 11_trends_summary.R
# Bilan synthétique des tendances pour les 21 variables :
#   - pente de Sen annualisée
#   - intervalle de confiance bootstrap (R = 500)
#   - test de Mann-Kendall (tau, p-value)
#   - amplitude saisonnière par décennie (premier vs dernier)
#   - plot comparatif des "vitesses" climatiques
#   - heatmap mois × année grille (4 × 6) pour les 21 variables
#   - grille STL trend pour les 21 variables
#
# Sortie :
#   outputs/trends_summary.csv
#   outputs/plots/11a_trends_sen.png
#   outputs/plots/11b_trends_significance.png
#   outputs/plots/11c_grid_heatmaps.png
#   outputs/plots/11d_grid_stl_trends.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(Kendall); library(trend); library(boot); library(patchwork)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
theme_set(theme_minimal(base_size = 11))

df <- read.csv(file.path(OUT_DIR, "climate_co2_monthly.csv")) |>
  mutate(date = as.Date(date))

all_vars <- c(
  "T2m","T500","SPFH2m","PWAT","APCP","TCDC",
  "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
  "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
  "CRE_SW","CRE_LW","CRE_net"
)

# ------------------------------------------------------------
# 1. Calcul Sen + bootstrap + Mann-Kendall pour chaque variable
# ------------------------------------------------------------
set.seed(42)
boot_sen <- function(x, idx) {
  s <- sort(idx)   # garde l'ordre temporel sur sous-échantillon
  trend::sens.slope(x[s])$estimates
}

cat("Calcul des tendances + bootstrap Sen (R=500) pour 21 variables...\n")
results <- list()
t0 <- Sys.time()

for (v in all_vars) {
  x   <- df[[v]]
  mk  <- MannKendall(x)
  sen <- sens.slope(x)
  b   <- boot::boot(data.frame(x = x), function(d, idx) boot_sen(d$x, idx),
                    R = 500)
  ci  <- boot::boot.ci(b, type = "perc")
  per_year_factor <- 12

  results[[v]] <- data.frame(
    var               = v,
    sen_per_year      = sen$estimates * per_year_factor,
    sen_lo95          = if (!is.null(ci)) ci$percent[4] * per_year_factor else NA,
    sen_hi95          = if (!is.null(ci)) ci$percent[5] * per_year_factor else NA,
    mk_tau            = mk$tau,
    mk_pvalue         = mk$sl,
    mean              = mean(x),
    sd                = sd(x),
    pct_change_47y    = (sen$estimates * per_year_factor * 47) / mean(x) * 100
  )
}
cat("Durée :", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n\n")

trends <- bind_rows(results) |>
  mutate(significant = mk_pvalue < 0.05)

cat("=== Tendances Sen annualisées (avec IC bootstrap 95%) ===\n")
print(trends, digits = 4)
write.csv(trends, file.path(OUT_DIR, "trends_summary.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 2. Plot comparatif : vitesses normalisées (% du moyen / an)
# ------------------------------------------------------------
trends_pct <- trends |>
  mutate(pct_per_year = sen_per_year / mean * 100) |>
  arrange(pct_per_year)

p_pct <- ggplot(trends_pct,
                aes(reorder(var, pct_per_year), pct_per_year,
                    fill = pct_per_year > 0)) +
  geom_col() +
  geom_text(aes(label = sprintf("%+.3f", pct_per_year)),
            hjust = ifelse(trends_pct$pct_per_year > 0, -0.1, 1.1),
            size = 3.2) +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
                    guide = "none") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(title    = "Vitesses d'évolution normalisées des 21 variables (1979-2025)",
       subtitle = "Pente de Sen / valeur moyenne, en % par an",
       x = NULL, y = "% / an") +
  theme(plot.margin = margin(5, 30, 5, 5))
ggsave(file.path(PLOT, "11a_trends_sen.png"), p_pct,
       width = 11, height = 8, dpi = 130)

# ------------------------------------------------------------
# 3. Plot des p-values Mann-Kendall
# ------------------------------------------------------------
trends_p <- trends |>
  mutate(nlogp = -log10(pmax(mk_pvalue, 1e-300))) |>
  arrange(nlogp)

p_pv <- ggplot(trends_p, aes(reorder(var, nlogp), nlogp,
                             fill = nlogp > -log10(0.05))) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "grey60"),
                    guide = "none") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed",
             color = "darkred") +
  coord_flip() +
  labs(title    = "Significativité des tendances (Mann-Kendall)",
       subtitle = "Ligne rouge = seuil p = 0.05 ; vert = tendance significative",
       x = NULL, y = "−log10(p)")
ggsave(file.path(PLOT, "11b_trends_significance.png"), p_pv,
       width = 11, height = 8, dpi = 130)

# ------------------------------------------------------------
# 4. Heatmap grille mois × année des anomalies (21 variables)
# ------------------------------------------------------------
df_long <- df |>
  pivot_longer(all_of(all_vars), names_to = "var", values_to = "value")

clim <- df_long |>
  group_by(var, month) |>
  summarise(clim_mean = mean(value), .groups = "drop")
df_anom <- df_long |>
  left_join(clim, by = c("var","month")) |>
  mutate(anom_z = (value - clim_mean) / ave(value - clim_mean, var, FUN = sd))

p_grid_heat <- ggplot(df_anom, aes(month, year, fill = anom_z)) +
  geom_tile() +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                       midpoint = 0, limits = c(-3, 3),
                       oob = squish, name = "anom\n(z-score)") +
  scale_x_continuous(breaks = c(1, 6, 12), labels = c("J","J","D")) +
  scale_y_reverse(breaks = c(1980, 2000, 2020)) +
  facet_wrap(~ var, ncol = 6) +
  labs(title    = "Heatmaps mois × année des anomalies — 21 variables",
       subtitle = "Anomalies z-score (= (val - climato) / sd), même échelle pour comparaison",
       x = "Mois", y = "Année") +
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        strip.text  = element_text(size = 9, face = "bold"))
ggsave(file.path(PLOT, "11c_grid_heatmaps.png"), p_grid_heat,
       width = 14, height = 10, dpi = 130)

# ------------------------------------------------------------
# 5. Grille des STL trends (21 variables)
# ------------------------------------------------------------
extract_stl_trend <- function(d, v) {
  ts_v <- ts(d[[v]], start = c(d$year[1], d$month[1]), frequency = 12)
  stl_fit <- stl(ts_v, s.window = "periodic", robust = TRUE)
  data.frame(date = d$date, var = v,
             trend = as.numeric(stl_fit$time.series[, "trend"]),
             observed = d[[v]])
}

stl_all <- lapply(all_vars, function(v) extract_stl_trend(df, v)) |>
  bind_rows() |>
  mutate(var = factor(var, levels = all_vars))

p_grid_stl <- ggplot(stl_all, aes(date)) +
  geom_line(aes(y = observed), color = "grey70", alpha = 0.6, linewidth = 0.3) +
  geom_line(aes(y = trend), color = "darkred", linewidth = 0.7) +
  facet_wrap(~ var, scales = "free_y", ncol = 6) +
  labs(title    = "Trend STL extraite — 21 variables",
       subtitle = "Gris : série observée ; rouge : composante trend STL",
       x = NULL, y = NULL) +
  theme(strip.text = element_text(size = 9, face = "bold"))
ggsave(file.path(PLOT, "11d_grid_stl_trends.png"), p_grid_stl,
       width = 14, height = 9, dpi = 130)

# ------------------------------------------------------------
# Bilan textuel
# ------------------------------------------------------------
cat("\n=== Synthèse ===\n")
cat("Variables avec tendance significative (p < 0.05) :",
    sum(trends$significant), "/ ", nrow(trends), "\n")
cat("Top 5 plus rapides en %/an :\n")
print(trends_pct |> slice_max(abs(pct_per_year), n = 5) |>
        select(var, sen_per_year, pct_per_year, mk_pvalue))

cat("\n=== Sauvegardes ===\n")
cat(" - trends_summary.csv\n")
cat(" - plots/11a_trends_sen.png\n")
cat(" - plots/11b_trends_significance.png\n")
cat(" - plots/11c_grid_heatmaps.png\n")
cat(" - plots/11d_grid_stl_trends.png\n")
