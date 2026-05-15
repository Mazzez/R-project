# ============================================================
# 08_granger.R
# Tests de causalité de Granger entre les variables climatiques
# et le CO2 (sur RÉSIDUS — anomalies désaisonnées et détendrées).
#
# Pour chaque variable X :
#   H0 (X ↛ CO2) : X ne cause pas CO2 au sens de Granger
#   H0 (CO2 ↛ X) : CO2 ne cause pas X au sens de Granger
#
# On teste avec un retard de 6 mois (motivé par la phase 1 :
# corrélation taux annuel ↔ ENSO peak à lag = 6 mois).
#
# Entrée : Analyse Climat 2.5°x2.5°/outputs/series_transformed.rds
# Sortie : Analyse Climat 2.5°x2.5°/outputs/granger_results.csv
#          Analyse Climat 2.5°x2.5°/outputs/plots/08_granger.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(lmtest)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
ds      <- readRDS(file.path(OUT_DIR, "series_transformed.rds"))
theme_set(theme_minimal(base_size = 12))

LAG <- 6  # cohérent avec le résultat ENSO de la phase 1

clim_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
               "CRE_SW","CRE_LW","CRE_net")

# Test Granger sur deux représentations en parallèle
run_granger_table <- function(d_x, d_co2, label) {
  out <- list()
  for (v in clim_vars) {
    x <- d_x[[v]]
    ok <- complete.cases(x, d_co2)
    if (sum(ok) < 50) next

    t1 <- tryCatch(grangertest(d_co2[ok] ~ x[ok], order = LAG),
                   error = function(e) NULL)
    t2 <- tryCatch(grangertest(x[ok] ~ d_co2[ok], order = LAG),
                   error = function(e) NULL)

    out[[v]] <- data.frame(
      repr = label,
      var  = v,
      F_x_to_co2  = if (!is.null(t1)) t1$F[2]        else NA,
      p_x_to_co2  = if (!is.null(t1)) t1$`Pr(>F)`[2]  else NA,
      F_co2_to_x  = if (!is.null(t2)) t2$F[2]        else NA,
      p_co2_to_x  = if (!is.null(t2)) t2$`Pr(>F)`[2]  else NA
    )
  }
  bind_rows(out)
}

gr_resid <- run_granger_table(ds$resid, ds$resid$co2_trend, "resid")
gr_d12   <- run_granger_table(ds$d12,   ds$d12$co2_avg,    "d12")

gr <- bind_rows(gr_resid, gr_d12) |>
  mutate(
    sig_x_to_co2 = p_x_to_co2 < 0.05,
    sig_co2_to_x = p_co2_to_x < 0.05,
    sens = case_when(
      sig_x_to_co2 &  sig_co2_to_x ~ "bidirectionnel",
      sig_x_to_co2 & !sig_co2_to_x ~ "X -> CO2",
      !sig_x_to_co2 &  sig_co2_to_x ~ "CO2 -> X",
      TRUE ~ "aucun"
    )
  ) |>
  arrange(repr, p_x_to_co2)

cat("=== Granger causality (lag =", LAG, "mois) — RÉSIDUS ===\n\n")
print(gr |> filter(repr == "resid") |>
        select(var, p_x_to_co2, p_co2_to_x, sens))
cat("\n=== Granger causality (lag =", LAG, "mois) — d12 (taux annuel) ===\n\n")
print(gr |> filter(repr == "d12") |>
        select(var, p_x_to_co2, p_co2_to_x, sens))

write.csv(gr, file.path(OUT_DIR, "granger_results.csv"), row.names = FALSE)

# Comptage par sens
cat("\n=== Synthèse sens du lien (p < 0.05) par représentation ===\n")
print(gr |> count(repr, sens))

# ============================================================
# Visualisation : -log10(p) dans les deux sens
# ============================================================
gr_long <- gr |>
  select(repr, var, p_x_to_co2, p_co2_to_x) |>
  pivot_longer(c(p_x_to_co2, p_co2_to_x),
               names_to = "direction", values_to = "p") |>
  mutate(
    direction = recode(direction,
                       p_x_to_co2 = "X -> CO2",
                       p_co2_to_x = "CO2 -> X"),
    nlogp = -log10(p)
  )

p_gr <- ggplot(gr_long, aes(reorder(var, nlogp), nlogp, fill = direction)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed", color = "darkred") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ repr, scales = "free_x") +
  coord_flip() +
  labs(title    = sprintf("Tests de causalité de Granger (lag = %d mois)", LAG),
       subtitle = "Comparaison résidus (anomalies détendrées) vs d12 (taux annuel)\nLigne rouge = seuil p = 0.05",
       x = NULL, y = "−log10(p)", fill = "Sens") +
  theme(legend.position = "bottom")
ggsave(file.path(PLOT, "08_granger.png"), p_gr,
       width = 11, height = 8, dpi = 130)

cat("\n=== Sauvegardes ===\n")
cat(" - granger_results.csv\n")
cat(" - plots/08_granger.png\n")
