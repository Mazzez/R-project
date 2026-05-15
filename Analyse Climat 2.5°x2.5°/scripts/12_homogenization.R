# ============================================================
# 12_homogenization.R
# Détection et correction du saut CFSR (.grb2, 1979-2010) -> CFSv2
# (.grib2, 2011-2025) sur les 21 variables.
#
# Méthode :
#   On modélise la série désaisonnée par un modèle additif :
#     X_t = a + b * t + c * 1{t >= 2011-01} + season(t) + eps_t
#   Le coefficient c (step) estime la discontinuité de niveau
#   imputable au changement de réanalyse, indépendamment de la
#   tendance climatique sous-jacente (b).
#
# Sortie :
#   outputs/cfsr_to_cfsv2_jumps.csv            (saut + IC + p-value)
#   outputs/monthly_global_means_25_homog.csv  (série homogénéisée)
#   outputs/plots/12a_jumps_bar.png            (saut estimé par variable)
#   outputs/plots/12b_before_after.png         (4 vars témoins, avant/après)
#   outputs/plots/12c_significance_grid.png    (saut absolu + p-value × var)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(patchwork)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
theme_set(theme_minimal(base_size = 11))

clim <- read.csv(file.path(OUT_DIR, "monthly_global_means_25.csv")) |>
  mutate(date = as.Date(date),
         t    = as.numeric((date - as.Date("1979-01-01")) / 365.25),
         step = as.integer(date >= as.Date("2011-01-01")),
         m    = factor(month, levels = 1:12))

vars_clim <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO")

# Recalculer les CRE depuis les colonnes brutes
clim <- clim |>
  mutate(CRE_SW  = (DSWRF - USWRF) - (CSDSF - CSUSF),
         CRE_LW  = (DLWRF - ULWRF) - (CSDLF - CSULF),
         CRE_net = CRE_SW + CRE_LW)

all_vars <- c(vars_clim, "CRE_SW", "CRE_LW", "CRE_net")

# ------------------------------------------------------------
# 1. Estimation du saut par variable
# ------------------------------------------------------------
jumps <- list()
for (v in all_vars) {
  d <- clim
  d$y <- d[[v]]

  # Modèle : tendance linéaire + step + saisonnalité (mois en facteur)
  fit <- lm(y ~ t + step + m, data = d)
  s   <- summary(fit)

  step_coef <- coef(s)["step", ]
  ci <- confint(fit, "step", level = 0.95)
  trend_coef <- coef(s)["t", ]   # ppm/K/W/m² par an

  jumps[[v]] <- data.frame(
    var       = v,
    jump      = step_coef["Estimate"],
    se        = step_coef["Std. Error"],
    p_value   = step_coef["Pr(>|t|)"],
    ci_lo     = ci[1],
    ci_hi     = ci[2],
    trend_per_yr = trend_coef["Estimate"],
    mean_var  = mean(d$y),
    sd_var    = sd(d$y),
    jump_pct  = step_coef["Estimate"] / mean(d$y) * 100
  )
}
J <- bind_rows(jumps) |>
  mutate(significant = p_value < 0.05,
         jump_in_sd  = jump / sd_var)

cat("=== Saut estimé CFSR -> CFSv2 (jan 2011) par variable ===\n")
print(J |>
        arrange(desc(abs(jump_in_sd))) |>
        select(var, jump, jump_pct, jump_in_sd, p_value, significant),
      digits = 3)
write.csv(J, file.path(OUT_DIR, "cfsr_to_cfsv2_jumps.csv"), row.names = FALSE)

cat("\n=== Synthèse ===\n")
cat("Variables avec saut significatif (p < 0.05) :",
    sum(J$significant), "/ ", nrow(J), "\n")
cat("Top 5 plus gros sauts (en sd) :\n")
print(J |>
        slice_max(abs(jump_in_sd), n = 5) |>
        select(var, jump, jump_pct, jump_in_sd, p_value))

# ------------------------------------------------------------
# 2. Construction d'une version homogénéisée
#    On retire le saut estimé aux observations CFSv2 (>= 2011-01).
# ------------------------------------------------------------
homog <- clim
for (v in all_vars) {
  jump <- J$jump[J$var == v]
  homog[[v]] <- ifelse(homog$step == 1, homog[[v]] - jump, homog[[v]])
}
homog <- homog |> select(-t, -step, -m)
write.csv(homog, file.path(OUT_DIR, "monthly_global_means_25_homog.csv"),
          row.names = FALSE)

# ------------------------------------------------------------
# 3. Plot bar des sauts en sd (signé)
# ------------------------------------------------------------
p_jumps <- ggplot(J |> arrange(jump_in_sd),
                  aes(reorder(var, jump_in_sd), jump_in_sd,
                      fill = significant)) +
  geom_col() +
  geom_text(aes(label = sprintf("%+.2f", jump_in_sd)),
            hjust = ifelse(J$jump_in_sd > 0, -0.15, 1.15),
            size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("TRUE"  = "tomato",
                               "FALSE" = "grey60"),
                    labels = c("FALSE" = "non sig.", "TRUE" = "p < 0.05")) +
  coord_flip() +
  labs(title    = "Saut CFSR → CFSv2 (jan 2011) par variable",
       subtitle = "Estimé par lm(y ~ t + step + month) ; valeur en écarts-type",
       x = NULL, y = "Saut / écart-type", fill = NULL) +
  theme(legend.position = "bottom",
        plot.margin     = margin(5, 30, 5, 5))
ggsave(file.path(PLOT, "12a_jumps_bar.png"), p_jumps,
       width = 10, height = 8, dpi = 130)

# ------------------------------------------------------------
# 4. Plot before/after pour 4 variables (les plus impactées)
# ------------------------------------------------------------
top4 <- J |>
  slice_max(abs(jump_in_sd), n = 4) |>
  pull(var)
cat("\nVariables visualisées avant/après :\n")
print(top4)

ba_long <- list()
for (v in top4) {
  ba_long[[v]] <- bind_rows(
    data.frame(date = clim$date,
               value = clim[[v]],
               version = "Brute (avec saut)",
               var = v),
    data.frame(date = homog$date,
               value = homog[[v]],
               version = "Homogénéisée",
               var = v)
  )
}
ba_long <- bind_rows(ba_long)

p_ba <- ggplot(ba_long, aes(date, value, color = version)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  geom_vline(xintercept = as.Date("2011-01-01"),
             linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("Brute (avec saut)" = "steelblue",
                                "Homogénéisée"      = "tomato")) +
  facet_wrap(~ var, scales = "free_y") +
  labs(title    = "Avant / après homogénéisation — top 4 variables",
       subtitle = "Ligne pointillée rouge : transition CFSR → CFSv2 (jan 2011)",
       x = NULL, y = NULL, color = NULL) +
  theme(legend.position = "bottom")
ggsave(file.path(PLOT, "12b_before_after.png"), p_ba,
       width = 12, height = 8, dpi = 130)

# ------------------------------------------------------------
# 5. Plot synthèse : magnitude × significativité
# ------------------------------------------------------------
J_plot <- J |>
  mutate(nlogp = -log10(pmax(p_value, 1e-300)))

p_sig <- ggplot(J_plot, aes(reorder(var, abs(jump_in_sd)),
                            abs(jump_in_sd),
                            fill = nlogp)) +
  geom_col() +
  scale_fill_viridis_c(name = "−log10(p)",
                       option = "magma", direction = -1) +
  coord_flip() +
  labs(title    = "Magnitude absolue du saut + significativité",
       subtitle = "Couleur = -log10(p) ; un saut peut être grand mais non significatif si la variance est élevée",
       x = NULL, y = "|Saut| / écart-type")
ggsave(file.path(PLOT, "12c_significance_grid.png"), p_sig,
       width = 10, height = 8, dpi = 130)

cat("\n=== Sauvegardes ===\n")
cat(" - cfsr_to_cfsv2_jumps.csv\n")
cat(" - monthly_global_means_25_homog.csv\n")
cat(" - plots/12a_jumps_bar.png\n")
cat(" - plots/12b_before_after.png\n")
cat(" - plots/12c_significance_grid.png\n")
