# ============================================================
# 04_merge_with_co2.R
# Fusionne les 18 moyennes globales climatiques avec la série CO2
# NOAA GML, calcule les CRE, et fait un premier aperçu des
# corrélations climat ↔ CO2 (sans pré-traitement, juste pour repérer
# les liens de premier ordre).
#
# Sortie : Analyse Climat 2.5°x2.5°/outputs/climate_co2_monthly.csv
#          Analyse Climat 2.5°x2.5°/outputs/plots/05_correlations_brut.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

CLIMAT_FILE <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs/monthly_global_means_25.csv"
CO2_FILE    <- "/home/mazzez/Bureau/R project/Final Version/CO2/co2_mm_gl.csv"
OUT_DIR     <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT        <- file.path(OUT_DIR, "plots")
theme_set(theme_minimal(base_size = 12))

clim <- read.csv(CLIMAT_FILE) |> mutate(date = as.Date(date))
co2  <- read.csv(CO2_FILE, comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  select(date, co2_avg = average, co2_trend = trend)

cat("Climat   :", nrow(clim), "lignes,",
    as.character(min(clim$date)), "->", as.character(max(clim$date)), "\n")
cat("CO2 NOAA :", nrow(co2), "lignes,",
    as.character(min(co2$date)),  "->", as.character(max(co2$date)),  "\n")

# Fusion + ajout des CRE
df <- inner_join(clim, co2, by = "date") |>
  mutate(
    CRE_SW  = (DSWRF - USWRF) - (CSDSF - CSUSF),
    CRE_LW  = (DLWRF - ULWRF) - (CSDLF - CSULF),
    CRE_net = CRE_SW + CRE_LW
  )

cat("\nTableau fusionné :", paste(dim(df), collapse = " x "), "\n")
cat("Plage commune    :", as.character(min(df$date)), "->",
    as.character(max(df$date)), "\n")

write.csv(df, file.path(OUT_DIR, "climate_co2_monthly.csv"), row.names = FALSE)
cat("\n=== Sauvegardé : climate_co2_monthly.csv ===\n")

# ============================================================
# Aperçu des corrélations brutes (Pearson, sur niveaux)
# ATTENTION : ces corrélations contiennent la trend commune ;
# elles servent uniquement à repérer les liens de premier ordre
# avant la phase 3 où on désaisonnera et différenciera proprement.
# ============================================================
clim_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
               "CRE_SW","CRE_LW","CRE_net")

cor_brut <- sapply(clim_vars, function(v)
  cor(df[[v]], df$co2_trend, use = "complete.obs"))
cor_df <- data.frame(var = clim_vars, r_pearson = cor_brut) |>
  arrange(desc(abs(r_pearson)))

cat("\n=== Corrélations brutes Pearson( var, CO2_trend ) ===\n")
print(cor_df)
write.csv(cor_df, file.path(OUT_DIR, "correlations_brut.csv"), row.names = FALSE)

p_cor <- ggplot(cor_df, aes(reorder(var, r_pearson), r_pearson,
                            fill = r_pearson > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
                    guide = "none") +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  coord_flip() +
  labs(title    = "Corrélations brutes (Pearson) climat ↔ CO2 trend",
       subtitle = "Niveaux non désaisonnés ni dé-tendance — vue de premier ordre seulement",
       x = NULL, y = "r")
ggsave(file.path(PLOT, "05_correlations_brut.png"),
       p_cor, width = 10, height = 8, dpi = 130)

cat("\n=== Plot : 05_correlations_brut.png ===\n")
