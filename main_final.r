# =============================================================================
#                       ANALYSE COMPLÈTE DES DONNÉES 
# =============================================================================


# =============================================================================
#           ANALYSE COMPLÈTE DES DONNÉES CO₂ GLOBALES NOAA (1979-2025)
# =============================================================================

# -----------------------------------------------------------------------------
# 1. INSTALLATION DES PACKAGES
# -----------------------------------------------------------------------------
cat("=== INSTALLATION ET CHARGEMENT DES PACKAGES ===\n")

# Liste des packages nécessaires
packages <- c("tidyverse", "lubridate", "ggplot2", "ggpubr", "zoo", "gridExtra",
              "viridis", "scales", "grid")

# Installer les packages manquants
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("Installation des packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages)
}

# Charger les packages
library(tidyverse)    # Manipulation de données
library(lubridate)    # Dates
library(ggplot2)      # Graphiques
library(ggpubr)       # Graphiques avancés
library(zoo)          # Séries temporelles
library(gridExtra)    # Multiples graphiques
library(viridis)      # Couleurs
library(scales)       # Échelles
library(grid)         # Fonctions graphiques de bas niveau

cat("✓ Tous les packages sont chargés\n")

# -----------------------------------------------------------------------------
# 2. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------
cat("\n=== CONFIGURATION ===\n")


# Créer les dossiers nécessaires
dirs_to_create <- c("processed", "plots", "reports")
for(dir in dirs_to_create) {
  if(!dir.exists(dir)) {
    dir.create(dir)
    cat("✓ Dossier créé:", dir, "\n")
  }
}

# Paramètres graphiques
theme_set(theme_minimal(base_size = 12))
options(scipen = 999)  # Désactiver la notation scientifique

# ------------------------------------------------------------------------------
# 3. LECTURE DES DONNÉES BRUTES
# ------------------------------------------------------------------------------
cat("\n=== LECTURE DES DONNÉES BRUTES ===\n")

# Vérifier l'existence du fichier
if(!file.exists("co2_mm_gl.csv")) {
  stop("ERREUR: Fichier co2_mm_gl.csv non trouvé!")
}

# Lire les premières lignes pour inspection
cat("\n1. Inspection des premières lignes:\n")
raw_lines <- readLines("co2_mm_gl.csv", n = 20)
for(i in 1:length(raw_lines)) {
  cat(sprintf("Ligne %2d: %s\n", i, substr(raw_lines[i], 1, 80)))
}

# Lire les données (ignorer les lignes de commentaire)
co2_raw <- read.table("co2_mm_gl.csv", 
                      header = TRUE, 
                      sep = ",", 
                      comment.char = "#",
                      stringsAsFactors = FALSE,
                      na.strings = c("", "NA", "-99.99"))

cat("\n2. Données brutes chargées:\n")
cat("   Dimensions:", dim(co2_raw), "lignes x colonnes\n")
cat("   Période:", min(co2_raw$year), "à", max(co2_raw$year), "\n")
cat("   Colonnes:", paste(names(co2_raw), collapse = ", "), "\n")

# ------------------------------------------------------------------------------
# 4. NETTOYAGE ET PRÉPARATION
# ------------------------------------------------------------------------------
cat("\n=== NETTOYAGE ET PRÉPARATION DES DONNÉES ===\n")

co2_clean <- co2_raw %>%
  # Créer une date complète
  mutate(
    Date = make_date(year, month, 15),  # 15 du mois comme convention
    Date_decimal = decimal,
    
    # Variables principales
    CO2_ppm = average,                   # Concentration CO₂
    CO2_uncertainty = average_unc,       # Incertitude
    CO2_trend = trend,                   # Tendance désaisonnalisée
    Trend_uncertainty = trend_unc,
    
    # Identifiants et métadonnées
    ID = row_number(),
    
    # Variables temporelles
    Year = year,
    Month = month,
    Quarter = ceiling(month / 3),
    
    # Saisons (hémisphère nord)
    Season = case_when(
      month %in% c(12, 1, 2) ~ "Hiver",
      month %in% c(3, 4, 5) ~ "Printemps",
      month %in% c(6, 7, 8) ~ "Été",
      month %in% c(9, 10, 11) ~ "Automne"
    ),
    
    # Décennies
    Decade = floor(Year / 10) * 10,
    
    # Périodes d'analyse
    Period = case_when(
      Year < 1990 ~ "1979-1989",
      Year >= 1990 & Year < 2000 ~ "1990-1999",
      Year >= 2000 & Year < 2010 ~ "2000-2009",
      Year >= 2010 & Year < 2020 ~ "2010-2019",
      Year >= 2020 ~ "2020-présent"
    ),
    
    # Variables dérivées
    CO2_anomaly = CO2_ppm - mean(CO2_ppm, na.rm = TRUE),
    Year_fraction = Year + (Month - 0.5) / 12
  ) %>%
  
  # Gérer les valeurs manquantes
  group_by(Year) %>%
  mutate(
    CO2_ppm = ifelse(is.na(CO2_ppm), mean(CO2_ppm, na.rm = TRUE), CO2_ppm)
  ) %>%
  ungroup() %>%
  
  # Sélectionner et ordonner les colonnes
  select(ID, Date, Year, Month, Quarter, Season, Decade, Period,
         CO2_ppm, CO2_uncertainty, CO2_trend, Trend_uncertainty,
         CO2_anomaly, Date_decimal, Year_fraction) %>%
  
  # Trier par date
  arrange(Date)

cat("✓ Données nettoyées\n")
cat("   Observations:", nrow(co2_clean), "\n")
cat("   Période finale:", min(co2_clean$Year), "-", max(co2_clean$Year), "\n")
cat("   Valeurs manquantes CO2:", sum(is.na(co2_clean$CO2_ppm)), "\n")

# -----------------------------------------------------------------------------
# 5. STATISTIQUES DESCRIPTIVES
# -----------------------------------------------------------------------------
cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")

# 5.1 Statistiques générales
cat("\n1. STATISTIQUES GÉNÉRALES DU CO₂:\n")
general_stats <- summary(co2_clean$CO2_ppm)
print(general_stats)

# 5.2 Statistiques par décennie
cat("\n2. STATISTIQUES PAR DÉCENNIE:\n")
decade_stats <- co2_clean %>%
  group_by(Decade) %>%
  summarise(
    Début = min(Year),
    Fin = max(Year),
    N_mois = n(),
    CO2_moyen = mean(CO2_ppm, na.rm = TRUE),
    CO2_min = min(CO2_ppm, na.rm = TRUE),
    CO2_max = max(CO2_ppm, na.rm = TRUE),
    CO2_sd = sd(CO2_ppm, na.rm = TRUE),
    Augmentation = CO2_max - CO2_min,
    .groups = 'drop'
  )
print(decade_stats)

# 5.3 Statistiques saisonnières
cat("\n3. STATISTIQUES SAISONNIÈRES:\n")
season_stats <- co2_clean %>%
  group_by(Season) %>%
  summarise(
    CO2_moyen = mean(CO2_ppm, na.rm = TRUE),
    CO2_sd = sd(CO2_ppm, na.rm = TRUE),
    Amplitude = max(CO2_ppm, na.rm = TRUE) - min(CO2_ppm, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(CO2_moyen)
print(season_stats)

# 5.4 Statistiques annuelles
cat("\n4. STATISTIQUES ANNUELLES (TOP 5):\n")
annual_stats <- co2_clean %>%
  group_by(Year) %>%
  summarise(
    CO2_moyen = mean(CO2_ppm, na.rm = TRUE),
    CO2_min = min(CO2_ppm, na.rm = TRUE),
    CO2_max = max(CO2_ppm, na.rm = TRUE),
    Amplitude = CO2_max - CO2_min,
    .groups = 'drop'
  ) %>%
  arrange(desc(CO2_moyen))

print(head(annual_stats, 5))
cat("\nAnnée avec CO₂ minimum:", annual_stats$Year[which.min(annual_stats$CO2_moyen)], "\n")
cat("Année avec CO₂ maximum:", annual_stats$Year[which.max(annual_stats$CO2_moyen)], "\n")

# -----------------------------------------------------------------------------
# 6. ANALYSES AVANCÉES
# -----------------------------------------------------------------------------
cat("\n=== ANALYSES AVANCÉES ===\n")

# 6.1 Tendance linéaire
cat("\n1. RÉGRESSION LINÉAIRE (CO₂ ~ Temps):\n")
lm_model <- lm(CO2_ppm ~ Year_fraction, data = co2_clean)
lm_summary <- summary(lm_model)

cat("   Pente:", round(coef(lm_model)[2], 4), "ppm/année\n")
cat("   Ordonnée à l'origine:", round(coef(lm_model)[1], 2), "ppm\n")
cat("   R²:", round(lm_summary$r.squared, 4), "\n")
cat("   p-value:", format.pval(lm_summary$coefficients[2, 4], digits = 3), "\n")

# 6.2 Augmentation totale
co2_1979 <- mean(co2_clean$CO2_ppm[co2_clean$Year == 1979], na.rm = TRUE)
co2_2025 <- mean(co2_clean$CO2_ppm[co2_clean$Year == 2025], na.rm = TRUE)

cat("\n2. AUGMENTATION TOTALE (1979-2025):\n")
cat("   CO₂ en 1979:", round(co2_1979, 1), "ppm\n")
cat("   CO₂ en 2025:", round(co2_2025, 1), "ppm\n")
cat("   Augmentation absolue:", round(co2_2025 - co2_1979, 1), "ppm\n")
cat("   Augmentation relative:", round((co2_2025 - co2_1979) / co2_1979 * 100, 1), "%\n")
cat("   Taux annuel moyen:", round((co2_2025 - co2_1979) / (2025 - 1979), 2), "ppm/an\n")

# 6.3 Analyse de la saisonnalité
cat("\n3. ANALYSE DE SAISONNALITÉ:\n")
seasonal_amplitude <- max(season_stats$CO2_moyen) - min(season_stats$CO2_moyen)
cat("   Amplitude saisonnière moyenne:", round(seasonal_amplitude, 2), "ppm\n")
cat("   Saison la plus élevée:", season_stats$Season[which.max(season_stats$CO2_moyen)], "\n")
cat("   Saison la plus basse:", season_stats$Season[which.min(season_stats$CO2_moyen)], "\n")

# -----------------------------------------------------------------------------
# 7. VISUALISATIONS
# -----------------------------------------------------------------------------
cat("\n=== CRÉATION DES VISUALISATIONS ===\n")

# 7.1 Évolution temporelle complète
p1 <- ggplot(co2_clean, aes(x = Date, y = CO2_ppm)) +
  geom_line(color = "#E41A1C", linewidth = 1, alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.1, color = "#377EB8", 
              linetype = "dashed", se = FALSE, linewidth = 1) +
  geom_smooth(method = "lm", color = "#4DAF4A", se = FALSE, linewidth = 0.8) +
  labs(
    title = "Évolution des concentrations CO₂ globales (1979-2025)",
    subtitle = "Source: NOAA Global Monitoring Laboratory",
    x = "Année",
    y = "CO₂ (ppm)",
    caption = paste("Pente:", round(coef(lm_model)[2], 3), "ppm/an | R²:", round(lm_summary$r.squared, 3))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    plot.caption = element_text(hjust = 1, color = "gray50"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(limits = c(330, 440)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")

# 7.2 Cycle saisonnier
p2 <- ggplot(co2_clean, aes(x = Month, y = CO2_ppm)) +
  geom_jitter(alpha = 0.1, color = "gray70", width = 0.2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), 
               color = "#984EA3", linewidth = 1.5) +
  stat_summary(fun = mean, geom = "point", aes(group = 1), 
               color = "#984EA3", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", 
               aes(group = 1), alpha = 0.2, fill = "#984EA3") +
  labs(
    title = "Cycle saisonnier du CO₂ global",
    subtitle = "Moyenne mensuelle avec intervalle de confiance",
    x = "Mois",
    y = "CO₂ (ppm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb)

# 7.3 Évolution du cycle saisonnier par décennie
cat("\n Évolution du cycle saisonnier par décennie\n")

# Calculer la moyenne mensuelle par décennie
seasonal_by_decade <- co2_clean %>%
  group_by(Decade, Month) %>%
  summarise(
    CO2_mean = mean(CO2_ppm, na.rm = TRUE),
    CO2_sd = sd(CO2_ppm, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Month_name = factor(month.abb[Month], levels = month.abb),
    Decade_label = paste0(Decade, "s")
  )

# Créer le graphique
p3 <- ggplot(seasonal_by_decade, aes(x = Month_name, y = CO2_mean, 
                                     color = Decade_label, group = Decade_label)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  labs(
    title = "Évolution du cycle saisonnier du CO₂ par décennie",
    subtitle = "Moyenne mensuelle pour chaque décennie (1979-2025)",
    x = "Mois",
    y = "CO₂ (ppm)",
    color = "Décennie"
  ) +
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.9) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(limits = month.abb)

# Calculer l'amplitude saisonnière par décennie
seasonal_amplitude_by_decade <- seasonal_by_decade %>%
  group_by(Decade, Decade_label) %>%
  summarise(
    Amplitude = max(CO2_mean) - min(CO2_mean),
    .groups = 'drop'
  )

cat("   Amplitude saisonnière par décennie:\n")
print(seasonal_amplitude_by_decade)

# 7.4 Boxplot par décennie
p4 <- ggplot(co2_clean, aes(x = factor(Decade), y = CO2_ppm, fill = factor(Decade))) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Distribution du CO₂ par décennie",
    subtitle = "Boîtes: distribution interquartile | Losanges: moyenne",
    x = "Décennie",
    y = "CO₂ (ppm)",
    fill = "Décennie"
  ) +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.9) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(limits = c(330, 440))

# 7.5 Anomalies par rapport à la moyenne
co2_monthly_avg <- co2_clean %>%
  group_by(Month) %>%
  summarise(monthly_avg = mean(CO2_ppm, na.rm = TRUE))

co2_anomalies <- co2_clean %>%
  left_join(co2_monthly_avg, by = "Month") %>%
  mutate(anomaly = CO2_ppm - monthly_avg)

p5 <- ggplot(co2_anomalies, aes(x = Date, y = anomaly)) +
  geom_line(color = "#FF7F00", linewidth = 0.7, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "loess", span = 0.2, color = "#A65628", se = FALSE) +
  labs(
    title = "Anomalies du CO₂ par rapport à la moyenne mensuelle",
    subtitle = "Écart à la moyenne climatologique mensuelle",
    x = "Année",
    y = "Anomalie CO₂ (ppm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")

# 7.6 Taux de croissance annuel
annual_growth <- co2_clean %>%
  group_by(Year) %>%
  summarise(mean_co2 = mean(CO2_ppm, na.rm = TRUE)) %>%
  mutate(
    growth = mean_co2 - lag(mean_co2),
    growth_rate = (mean_co2 - lag(mean_co2)) / lag(mean_co2) * 100
  ) %>%
  filter(!is.na(growth))

p6 <- ggplot(annual_growth, aes(x = Year, y = growth)) +
  geom_col(fill = "#F781BF", alpha = 0.7) +
  geom_hline(yintercept = mean(annual_growth$growth, na.rm = TRUE), 
             linetype = "dashed", color = "#E41A1C", linewidth = 1) +
  geom_smooth(method = "loess", color = "#377EB8", se = FALSE, linewidth = 0.8) +
  labs(
    title = "Augmentation annuelle du CO₂",
    subtitle = paste("Moyenne:", round(mean(annual_growth$growth, na.rm = TRUE), 2), "ppm/an"),
    x = "Année",
    y = "Augmentation (ppm/an)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5))

# -----------------------------------------------------------------------------
# 8. SAUVEGARDE DES GRAPHIQUES
# -----------------------------------------------------------------------------
cat("\n=== SAUVEGARDE DES GRAPHIQUES ===\n")

# Dimensions standard pour publications
width_large <- 12
height_large <- 8
width_small <- 10
height_small <- 6
dpi <- 300

# Sauvegarder chaque graphique individuellement 
ggsave("plots/CO2_evolution_1979_2025.png", p1, 
       width = width_large, height = height_large, dpi = dpi)
cat("✓ Graphique 1: plots/CO2_evolution_1979_2025.png\n")

ggsave("plots/CO2_seasonal_cycle.png", p2, 
       width = width_small, height = height_small, dpi = dpi)
cat("✓ Graphique 2: plots/CO2_seasonal_cycle.png\n")

ggsave("plots/CO2_seasonal_evolution_by_decade.png", p3, 
       width = width_large, height = height_small, dpi = dpi)
cat("✓ Graphique 3: plots/CO2_seasonal_evolution_by_decade.png\n")

ggsave("plots/CO2_by_decade.png", p4, 
       width = width_small, height = height_small, dpi = dpi)
cat("✓ Graphique 4: plots/CO2_by_decade.png\n")

ggsave("plots/CO2_anomalies.png", p5, 
       width = width_large, height = height_small, dpi = dpi)
cat("✓ Graphique 5: plots/CO2_anomalies.png\n")

ggsave("plots/CO2_annual_growth.png", p6, 
       width = width_large, height = height_small, dpi = dpi)
cat("✓ Graphique 6: plots/CO2_annual_growth.png\n")

# Mettre à jour le graphique combiné (ajouter p6)
cat("\nCréation du graphique combiné (6 graphiques)...\n")
p_combined <- grid.arrange(p1, p2, p3, p4, p5, p6,
                           ncol = 2, 
                           layout_matrix = rbind(c(1, 2), c(3, 4), c(5, 6)),
                           top = textGrob("Analyse complète des données CO₂ globales NOAA (1979-2025)", 
                                          gp = gpar(fontsize = 18, fontface = "bold")))

ggsave("plots/CO2_analysis_complete.png", p_combined, 
       width = 16, height = 24, dpi = dpi)  # Augmenter la hauteur pour 6 graphiques
cat("✓ Graphique combiné mis à jour: plots/CO2_analysis_complete.png\n")

# -----------------------------------------------------------------------------
# 9. SAUVEGARDE DES DONNÉES
# -----------------------------------------------------------------------------
cat("\n=== SAUVEGARDE DES DONNÉES ===\n")

# 1. Dans la section 9.4 Statistiques des tendances, AJOUTER :
cat("\n9.5 Statistiques saisonnières par décennie:\n")
write_csv(seasonal_amplitude_by_decade, "processed/co2_seasonal_amplitude_by_decade.csv")
cat("✓ Amplitude saisonnière par décennie: processed/co2_seasonal_amplitude_by_decade.csv\n")

cat("\n=== EXPORT DES FICHIERS CO₂ ===\n")

# 2. FICHIER NETTOYÉ COMPLET (561 observations)

cat("\n1. Exporting cleaned CO₂ dataset...\n")

write_csv(co2_clean, "processed/co2_global_noaa_clean_1979_2025.csv")

cat("✓ Fichier sauvegardé: processed/co2_global_noaa_clean_1979_2025.csv\n")
cat("   Dimensions: ", nrow(co2_clean), " lignes x ", ncol(co2_clean), " colonnes\n")

# 3. STATISTIQUES ANNUELLES

cat("\n2. Exporting annual statistics...\n")

annual_summary <- co2_clean %>%
  group_by(Year) %>%
  summarise(
    CO2_mean = mean(CO2_ppm, na.rm = TRUE),
    CO2_sd = sd(CO2_ppm, na.rm = TRUE),
    CO2_min = min(CO2_ppm, na.rm = TRUE),
    CO2_max = max(CO2_ppm, na.rm = TRUE),
    CO2_amplitude = CO2_max - CO2_min,
    Trend_mean = mean(CO2_trend, na.rm = TRUE),
    Anomaly_mean = mean(CO2_anomaly, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Year)

write_csv(annual_summary, "processed/co2_global_noaa_annual_1979_2025.csv")

cat("✓ Fichier sauvegardé: processed/co2_global_noaa_annual_1979_2025.csv\n")
cat("   Dimensions: ", nrow(annual_summary), " années\n")

# 4. CLIMATOLOGIE MENSUELLE (moyenne et écart-type par mois)

cat("\n3. Exporting monthly climatology...\n")

monthly_climatology <- co2_clean %>%
  group_by(Month) %>%
  summarise(
    Month_name = first(month.abb[Month]),
    CO2_mean = mean(CO2_ppm, na.rm = TRUE),
    CO2_sd = sd(CO2_ppm, na.rm = TRUE),
    CO2_min = min(CO2_ppm, na.rm = TRUE),
    CO2_max = max(CO2_ppm, na.rm = TRUE),
    N_observations = n(),
    .groups = 'drop'
  ) %>%
  arrange(Month) %>%
  select(Month, Month_name, CO2_mean, CO2_sd, CO2_min, CO2_max, N_observations)

write_csv(monthly_climatology, "processed/co2_global_noaa_monthly_climatology.csv")

cat("✓ Fichier sauvegardé: processed/co2_global_noaa_monthly_climatology.csv\n")
cat("   Dimensions: 12 mois x ", ncol(monthly_climatology), " colonnes\n")

# 5. STATISTIQUES DE TENDANCE PAR DÉCENNIE ET PÉRIODE

cat("\n4. Exporting trend statistics...\n")

# Statistiques par décennie
decade_summary <- co2_clean %>%
  group_by(Decade) %>%
  summarise(
    Decade_label = paste0(Decade, "s"),
    Year_start = min(Year),
    Year_end = max(Year),
    N_months = n(),
    CO2_mean = mean(CO2_ppm, na.rm = TRUE),
    CO2_sd = sd(CO2_ppm, na.rm = TRUE),
    CO2_min = min(CO2_ppm, na.rm = TRUE),
    CO2_max = max(CO2_ppm, na.rm = TRUE),
    CO2_increase = CO2_max - CO2_min,
    Trend_mean = mean(CO2_trend, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Decade)

# Ajouter la pente linéaire par décennie
decade_slopes <- co2_clean %>%
  group_by(Decade) %>%
  do(
    data.frame(
      Decade = unique(.$Decade),
      Slope_ppm_per_year = tryCatch(
        coef(lm(CO2_ppm ~ Year_fraction, data = .))[2],
        error = function(e) NA
      ),
      R_squared = tryCatch(
        summary(lm(CO2_ppm ~ Year_fraction, data = .))$r.squared,
        error = function(e) NA
      )
    )
  )

decade_full <- decade_summary %>%
  left_join(decade_slopes, by = "Decade") %>%
  select(Decade, Decade_label, Year_start, Year_end, N_months, 
         CO2_mean, CO2_sd, CO2_min, CO2_max, CO2_increase, 
         Slope_ppm_per_year, R_squared, Trend_mean)

write_csv(decade_full, "processed/co2_global_noaa_trend_statistics.csv")

cat("✓ Fichier sauvegardé: processed/co2_global_noaa_trend_statistics.csv\n")
cat("   Dimensions: ", nrow(decade_full), " décennies x ", ncol(decade_full), " colonnes\n")

# 6. RÉSUMÉ DES EXPORTS

cat("\n", strrep("=", 70), "\n", sep = "")
cat("RÉSUMÉ DES EXPORTS CO₂\n")
cat(strrep("=", 70), "\n\n")

cat("✓ Fichier 1: co2_global_noaa_clean_1979_2025.csv\n")
cat("   Contenu: Dataset complet nettoyé (561 observations, 15 colonnes)\n")
cat("   Variables: Year, Month, Date, CO2_ppm, CO2_trend, CO2_anomaly, etc.\n\n")

cat("✓ Fichier 2: co2_global_noaa_annual_1979_2025.csv\n")
cat("   Contenu: Statistiques annuelles (", nrow(annual_summary), " années)\n")
cat("   Variables: Mean, SD, Min, Max, Amplitude, Trend, Anomaly\n\n")

cat("✓ Fichier 3: co2_global_noaa_monthly_climatology.csv\n")
cat("   Contenu: Climatologie mensuelle (12 mois)\n")
cat("   Variables: Month, Name, Mean, SD, Min, Max, N_observations\n\n")

cat("✓ Fichier 4: co2_global_noaa_trend_statistics.csv\n")
cat("   Contenu: Tendances par décennie (6 décennies)\n")
cat("   Variables: Decade, Year_range, CO2_stats, Slope, R², Trend_mean\n\n")

cat("Tous les fichiers sont dans le dossier: processed/\n")
cat(strrep("=", 70), "\n")

# -----------------------------------------------------------------------------
# 10. RAPPORT SYNTHÉTIQUE
# -----------------------------------------------------------------------------
cat("\n=== RAPPORT SYNTHÉTIQUE ===\n")

cat("\n" , strrep("=", 70), "\n", sep = "")
cat("ANALYSE COMPLÈTE DES DONNÉES CO₂ GLOBALES NOAA\n")
cat(strrep("=", 70), "\n\n")

# AJOUTER dans les résultats clés :
cat("RÉSULTATS CLÉS:\n")
cat("  1. Concentration moyenne:", round(mean(co2_clean$CO2_ppm), 1), "ppm\n")
cat("  2. Tendance 1979-2025:", round(coef(lm_model)[2], 2), "ppm/an\n")
cat("  3. Augmentation totale:", round(co2_2025 - co2_1979, 1), "ppm\n")
cat("  4. Amplitude saisonnière moyenne:", round(seasonal_amplitude, 2), "ppm\n")
cat("  5. Décennie de plus forte augmentation:", 
    decade_stats$Decade[which.max(decade_stats$Augmentation)], "\n")
# AJOUTER la décennie avec la plus grande amplitude saisonnière
cat("  6. Décennie avec plus grande amplitude saisonnière:", 
    seasonal_amplitude_by_decade$Decade_label[which.max(seasonal_amplitude_by_decade$Amplitude)], 
    "(", round(max(seasonal_amplitude_by_decade$Amplitude), 2), "ppm)\n\n")

# Mettre à jour les produits générés :
cat("PRODUITS GÉNÉRÉS:\n")
cat("  1. Dataset nettoyé (", nrow(co2_clean), " observations)\n", sep = "")
cat("  2. 6 graphiques haute résolution (au lieu de 5)\n")  # CHANGER 5 → 6
cat("  3. 5 fichiers de synthèse CSV (au lieu de 4)\n")    # CHANGER 4 → 5
cat("  4. Tous les produits dans dossiers 'processed/' et 'plots/'\n")

# -----------------------------------------------------------------------------
# 11. GÉNÉRATION DE RAPPORT
# -----------------------------------------------------------------------------
cat("\n=== GÉNÉRATION DE RAPPORT ===\n")

# Créer un rapport texte synthétique
report_content <- paste0(
  "=================================\n",
  "RAPPORT D'ANALYSE CO₂ GLOBAL NOAA\n",
  "=================================\n\n",
  "Date de génération: ", Sys.Date(), "\n",
  "Période analysée: 1979-", max(co2_clean$Year), "\n",
  "Nombre d'observations: ", nrow(co2_clean), " mois\n\n",
  
  "RÉSULTATS CLÉS:\n",
  "1. Concentration moyenne: ", round(mean(co2_clean$CO2_ppm), 1), " ppm\n",
  "2. Tendance linéaire: ", round(coef(lm_model)[2], 2), " ppm/an\n",
  "3. Augmentation totale (1979-2025): ", round(co2_2025 - co2_1979, 1), " ppm\n",
  "4. R² de la tendance: ", round(lm_summary$r.squared, 3), "\n",
  "5. Amplitude saisonnière moyenne: ", round(seasonal_amplitude, 2), " ppm\n\n",
  
  "DÉCENNIES ANALYSÉES:\n"
)

# Ajouter les statistiques par décennie
for(i in 1:nrow(decade_stats)) {
  report_content <- paste0(report_content,
                           "  - ", decade_stats$Début[i], "-", decade_stats$Fin[i], 
                           ": ", round(decade_stats$CO2_moyen[i], 1), " ppm (Δ", 
                           round(decade_stats$Augmentation[i], 1), " ppm)\n")
}

report_content <- paste0(report_content, "\n",
                         "SAISONS (ordre croissant):\n")

# Ajouter les statistiques saisonnières
for(i in 1:nrow(season_stats)) {
  report_content <- paste0(report_content,
                           "  - ", season_stats$Season[i], 
                           ": ", round(season_stats$CO2_moyen[i], 1), " ppm\n")
}

report_content <- paste0(report_content, "\n",
                         "PRODUITS GÉNÉRÉS:\n",
                         "1. Données nettoyées: processed/co2_global_noaa_clean_1979_2025.csv\n",
                         "2. Résumé annuel: processed/co2_global_noaa_annual_1979_2025.csv\n",
                         "3. Climatologie mensuelle: processed/co2_global_noaa_monthly_climatology.csv\n",
                         "4. Statistiques de tendance: processed/co2_global_noaa_trend_statistics.csv\n",
                         "5. Amplitude saisonnière par décennie: processed/co2_seasonal_amplitude_by_decade.csv\n",
                         "6. 6 graphiques haute résolution dans dossier plots/\n\n",
                         
                         "ANALYSE RÉALISÉE AVEC R version ", R.version$major, ".", R.version$minor, "\n",
                         "Script: analyse_co2_global_noaa.R\n"
)

# Écrire le rapport dans un fichier
report_file <- paste0("reports/rapport_co2_global_", 
                      format(Sys.Date(), "%Y%m%d"), ".txt")
writeLines(report_content, report_file)
cat("✓ Rapport généré: ", report_file, "\n")


cat("\n", strrep("=", 70), "\n", sep = "")
cat("ANALYSE TERMINÉE AVEC SUCCÈS\n")
cat(strrep("=", 70), "\n")