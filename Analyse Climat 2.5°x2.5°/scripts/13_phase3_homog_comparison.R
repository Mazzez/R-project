# ============================================================
# 13_phase3_homog_comparison.R
# Compare les conclusions de la phase 3 sur la version brute
# vs la version homogénéisée (saut CFSR→CFSv2 retiré).
#
# Sortie :
#   outputs/comparison_homog_correlations.csv
#   outputs/plots/13a_correlations_homog_vs_brut.png
#   outputs/plots/13b_R2_homog_vs_brut.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
theme_set(theme_minimal(base_size = 11))

co2 <- read.csv("/home/mazzez/Bureau/R project/Final Version/CO2/co2_mm_gl.csv",
                comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  select(date, co2_avg = average, co2_trend = trend)

clim_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
               "CRE_SW","CRE_LW","CRE_net")

# ------------------------------------------------------------
# Pour les deux versions : transforme et calcule corrélations resid
# ------------------------------------------------------------
analyse_version <- function(file_path, label) {
  d <- read.csv(file_path) |>
    mutate(date = as.Date(date)) |>
    inner_join(co2, by = "date")

  if (!"CRE_SW" %in% names(d)) {
    d <- d |> mutate(
      CRE_SW  = (DSWRF - USWRF) - (CSDSF - CSUSF),
      CRE_LW  = (DLWRF - ULWRF) - (CSDLF - CSULF),
      CRE_net = CRE_SW + CRE_LW
    )
  }

  build_resid <- function(x, dates) {
    months <- as.integer(format(dates, "%m"))
    clim   <- tapply(x, months, mean)
    anom   <- x - clim[as.character(months)]
    t      <- as.numeric(dates)
    fit    <- lm(anom ~ t)
    as.numeric(anom - predict(fit))
  }

  resid_co2 <- build_resid(d$co2_trend, d$date)
  cor_resid <- sapply(c(clim_vars, "co2_trend"), function(v) {
    if (v == "co2_trend") return(1)
    r <- build_resid(d[[v]], d$date)
    cor(r, resid_co2, use = "complete.obs")
  })

  # Régression multivariée (R²)
  X <- as.data.frame(sapply(clim_vars, function(v) build_resid(d[[v]], d$date)))
  colnames(X) <- clim_vars
  fit_full <- lm(resid_co2 ~ ., data = X)
  fit_step <- step(fit_full, direction = "backward", trace = 0)
  r2_full <- summary(fit_full)$r.squared
  r2_step <- summary(fit_step)$r.squared

  list(
    cor   = data.frame(version = label, var = c(clim_vars, "co2_trend"),
                       r = cor_resid),
    r2    = data.frame(version = label, r2_full = r2_full, r2_step = r2_step,
                       n_step  = length(coef(fit_step)) - 1)
  )
}

cat("=== Analyse version BRUTE ===\n")
res_brut  <- analyse_version(
  file.path(OUT_DIR, "monthly_global_means_25.csv"), "brute")
cat("R² complet :", round(res_brut$r2$r2_full, 3),
    "  R² stepwise :", round(res_brut$r2$r2_step, 3),
    "  vars retenues :", res_brut$r2$n_step, "\n\n")

cat("=== Analyse version HOMOGÉNÉISÉE ===\n")
res_homog <- analyse_version(
  file.path(OUT_DIR, "monthly_global_means_25_homog.csv"), "homog")
cat("R² complet :", round(res_homog$r2$r2_full, 3),
    "  R² stepwise :", round(res_homog$r2$r2_step, 3),
    "  vars retenues :", res_homog$r2$n_step, "\n\n")

# ------------------------------------------------------------
# Comparaison côte à côte
# ------------------------------------------------------------
cmp <- bind_rows(res_brut$cor, res_homog$cor) |>
  filter(var != "co2_trend") |>
  pivot_wider(names_from = version, values_from = r) |>
  mutate(diff = homog - brute) |>
  arrange(desc(abs(diff)))

cat("=== Variations des corrélations (sur résidus) ===\n")
print(cmp, digits = 3, n = 21)
write.csv(cmp, file.path(OUT_DIR, "comparison_homog_correlations.csv"),
          row.names = FALSE)

# ------------------------------------------------------------
# Plot scatter : r_brut vs r_homog
# ------------------------------------------------------------
p_scatter <- ggplot(cmp, aes(brute, homog, label = var)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
  geom_point(aes(color = abs(diff) > 0.1), size = 3) +
  ggrepel::geom_text_repel(size = 3.5, max.overlaps = 21) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "tomato"),
                     name = "|Δr| > 0.1") +
  labs(title    = "Corrélations climat ↔ CO2 sur résidus",
       subtitle = "Comparaison : version brute vs version homogénéisée\nDiagonale = pas de changement",
       x = "r (version brute)", y = "r (version homogénéisée)") +
  theme(legend.position = "bottom")

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel", repos = "https://cloud.r-project.org")
}
library(ggrepel)
ggsave(file.path(PLOT, "13a_correlations_homog_vs_brut.png"), p_scatter,
       width = 10, height = 9, dpi = 130)

# ------------------------------------------------------------
# Plot R² comparatif
# ------------------------------------------------------------
r2_df <- bind_rows(
  data.frame(version = "Brute",         model = "Complet",  r2 = res_brut$r2$r2_full),
  data.frame(version = "Brute",         model = "Stepwise", r2 = res_brut$r2$r2_step),
  data.frame(version = "Homogénéisée",  model = "Complet",  r2 = res_homog$r2$r2_full),
  data.frame(version = "Homogénéisée",  model = "Stepwise", r2 = res_homog$r2$r2_step)
)

p_r2 <- ggplot(r2_df, aes(model, r2, fill = version)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", r2)),
            position = position_dodge(0.7), vjust = -0.4) +
  scale_fill_brewer(palette = "Set1") +
  ylim(0, 1) +
  labs(title    = "R² des modèles de régression CO2_trend ~ climat",
       subtitle = "Variables sur résidus (anom désaisonnée + détendrée)",
       x = NULL, y = "R²", fill = NULL) +
  theme(legend.position = "bottom")
ggsave(file.path(PLOT, "13b_R2_homog_vs_brut.png"), p_r2,
       width = 8, height = 6, dpi = 130)

cat("\n=== Sauvegardes ===\n")
cat(" - comparison_homog_correlations.csv\n")
cat(" - plots/13a_correlations_homog_vs_brut.png\n")
cat(" - plots/13b_R2_homog_vs_brut.png\n")
