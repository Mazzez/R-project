# ============================================================
# 03_validation.R
# Vérifie le réalisme des moyennes globales calculées et trace
# les séries temporelles + Cloud Radiative Effects.
#
# Entrée : Analyse Climat 2.5°x2.5°/outputs/monthly_global_means_25.csv
# Sortie : Analyse Climat 2.5°x2.5°/outputs/plots/*.png
#          Analyse Climat 2.5°x2.5°/outputs/cre_monthly_25.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
dir.create(PLOT, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 12))

df <- read.csv(file.path(OUT_DIR, "monthly_global_means_25.csv")) |>
  mutate(date = as.Date(date))

cat("Dimensions :", paste(dim(df), collapse = " x "), "\n")
cat("Plage      :", as.character(min(df$date)), "->",
    as.character(max(df$date)), "\n\n")

# ============================================================
# 1. Statistiques descriptives par variable
# ============================================================
vars <- setdiff(names(df), c("year", "month", "date"))
stats <- df |>
  pivot_longer(all_of(vars), names_to = "var", values_to = "value") |>
  group_by(var) |>
  summarise(min = min(value), mean = mean(value),
            max = max(value), sd = sd(value), .groups = "drop")
print(stats)

# Plages "raisonnables" attendues pour la moyenne globale annuelle
expected <- tribble(
  ~var,     ~lo,       ~hi,
  "T2m",    284,       290,
  "T500",   254,       262,
  "SPFH2m", 0.008,     0.011,
  "PWAT",   22,        28,
  "APCP",   0.3,       0.7,
  "TCDC",   55,        70,
  "DLWRF",  325,       350,
  "ULWRF",  380,       405,
  "DSWRF",  185,       215,
  "USWRF",  25,        45,
  "PRMSL",  101000,    101300,
  "CSDSF",  240,       275,
  "CSUSF",  30,        45,
  "CSDLF",  300,       325,
  "CSULF",  380,       405,
  "CDUVB",  3.5,       4.5,
  "DUVB",   2.8,       3.8,
  "ALBDO",  9,         15
)
sanity <- left_join(stats, expected, by = "var") |>
  mutate(below = mean < lo, above = mean > hi,
         status = ifelse(below | above, "ALERTE", "OK"))
cat("\n=== Sanity check (moyennes globales) ===\n")
print(sanity |> select(var, mean, lo, hi, status))

# ============================================================
# 2. Plot 18 séries temporelles
# ============================================================
df_long <- df |>
  pivot_longer(all_of(vars), names_to = "var", values_to = "value") |>
  mutate(var = factor(var, levels = vars))

p_all <- ggplot(df_long, aes(date, value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ var, scales = "free_y", ncol = 4) +
  labs(title    = "Moyennes globales mensuelles pondérées cos(lat) — 18 variables",
       subtitle = "1979-01 → 2025-12 (résolution 2.5° × 2.5°)",
       x = NULL, y = NULL)
ggsave(file.path(PLOT, "01_series_18_variables.png"),
       p_all, width = 16, height = 10, dpi = 130)

# ============================================================
# 3. Cloud Radiative Effects
#    CRE = (all-sky net flux) − (clear-sky net flux)
#    CRE_SW = (DSWRF − USWRF)  − (CSDSF − CSUSF)
#    CRE_LW = (DLWRF − ULWRF)  − (CSDLF − CSULF)
#    CRE_net = CRE_SW + CRE_LW
# ============================================================
cre <- df |>
  mutate(
    NET_SW_all   = DSWRF - USWRF,
    NET_SW_clear = CSDSF - CSUSF,
    NET_LW_all   = DLWRF - ULWRF,
    NET_LW_clear = CSDLF - CSULF,
    CRE_SW       = NET_SW_all - NET_SW_clear,
    CRE_LW       = NET_LW_all - NET_LW_clear,
    CRE_net      = CRE_SW + CRE_LW
  ) |>
  select(date, year, month, CRE_SW, CRE_LW, CRE_net)

cat("\n=== Statistiques Cloud Radiative Effects (W/m²) ===\n")
print(cre |>
        summarise(across(c(CRE_SW, CRE_LW, CRE_net),
                         list(mean = mean, sd = sd))))

write.csv(cre, file.path(OUT_DIR, "cre_monthly_25.csv"), row.names = FALSE)

p_cre <- cre |>
  pivot_longer(c(CRE_SW, CRE_LW, CRE_net),
               names_to = "component", values_to = "value") |>
  mutate(component = factor(component,
                            levels = c("CRE_SW", "CRE_LW", "CRE_net"))) |>
  ggplot(aes(date, value, color = component)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ component, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("CRE_SW" = "steelblue",
                                "CRE_LW" = "tomato",
                                "CRE_net" = "purple")) +
  labs(title    = "Cloud Radiative Effects à la surface",
       subtitle = "CRE_SW (refroidissement par les nuages), CRE_LW (réchauffement par les nuages), net",
       x = NULL, y = "W/m²", color = NULL)
ggsave(file.path(PLOT, "02_cloud_radiative_effects.png"),
       p_cre, width = 12, height = 10, dpi = 130)

# ============================================================
# 4. Vérification continuité CFSR → CFSv2 (transition fin 2010)
# ============================================================
df_zoom <- df |> filter(year >= 2008 & year <= 2013)

p_zoom <- df_zoom |>
  pivot_longer(c(T2m, PWAT, DSWRF, TCDC),
               names_to = "var", values_to = "value") |>
  ggplot(aes(date, value)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = as.Date("2011-01-01"),
             linetype = "dashed", color = "darkred", linewidth = 0.7) +
  facet_wrap(~ var, scales = "free_y") +
  labs(title    = "Continuité CFSR (≤ 2010) → CFSv2 (≥ 2011)",
       subtitle = "Pas de saut visible attendu — 4 variables témoins",
       x = NULL, y = NULL)
ggsave(file.path(PLOT, "03_transition_cfsr_cfsv2.png"),
       p_zoom, width = 12, height = 7, dpi = 130)

# ============================================================
# 5. Climatologie saisonnière (T2m, PWAT, DSWRF, APCP)
# ============================================================
clim <- df |>
  pivot_longer(c(T2m, PWAT, DSWRF, APCP, TCDC, ALBDO),
               names_to = "var", values_to = "value") |>
  group_by(var, month) |>
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

p_clim <- ggplot(clim, aes(month, mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~ var, scales = "free_y") +
  labs(title    = "Climatologie saisonnière mondiale",
       subtitle = "Moyenne mensuelle 1979-2025 ± 1 sd",
       x = "Mois", y = NULL)
ggsave(file.path(PLOT, "04_climatologie_saisonniere.png"),
       p_clim, width = 12, height = 7, dpi = 130)

cat("\n=== Plots sauvegardés dans :", PLOT, "===\n")
