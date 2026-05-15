# ============================================================
# Analyse de la concentration atmosphérique mondiale de CO2
# Source : NOAA GML  -  co2_mm_gl.csv  (moyennes mensuelles globales, ppm)
# Projet R - Climat & CO2
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(Kendall)
  library(trend)
  library(forecast)
  library(strucchange)
  library(scales)
})

data_path <- "/home/mazzez/Bureau/R project/Final Version/CO2/co2_mm_gl.csv"
out_dir   <- "/home/mazzez/Bureau/R project/Final Version/Analyse CO2/outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(out_dir, paste0(name, ".png")),
         p, width = w, height = h, dpi = 150)
}

theme_set(theme_minimal(base_size = 12))

# ============================================================
# 1. Chargement & nettoyage
# ============================================================
cat("\n========== 1. Chargement & nettoyage ==========\n")

co2 <- read.csv(data_path, comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  arrange(date) |>
  mutate(decade = paste0(floor(year / 10) * 10, "s"))

cat("Dimensions       :", paste(dim(co2), collapse = " x "), "\n")
cat("Plage temporelle :", as.character(min(co2$date)), "->",
    as.character(max(co2$date)), "\n")
cat("Plage average    :", round(min(co2$average), 2), "->",
    round(max(co2$average), 2), "ppm\n")
cat("NA dans average  :", sum(is.na(co2$average)), "\n")
cat("Pas de temps régulier ? ",
    all(diff(co2$date) %in% c(28, 29, 30, 31)), "\n")

# ============================================================
# 2. Statistiques descriptives
# ============================================================
cat("\n========== 2. Statistiques descriptives ==========\n")

stats_global <- co2 |>
  summarise(
    n        = n(),
    min      = min(average),
    max      = max(average),
    mean     = mean(average),
    median   = median(average),
    sd       = sd(average),
    range    = max(average) - min(average)
  )
print(stats_global)

stats_decade <- co2 |>
  group_by(decade) |>
  summarise(
    n      = n(),
    mean   = mean(average),
    min    = min(average),
    max    = max(average),
    sd     = sd(average),
    .groups = "drop"
  )
print(stats_decade)
write.csv(stats_decade, file.path(out_dir, "stats_decennie.csv"),
          row.names = FALSE)

total_rise <- last(co2$average) - first(co2$average)
years_span <- as.numeric(diff(range(co2$decimal)))
avg_rate   <- total_rise / years_span
cat(sprintf("\nHausse totale: %.2f ppm sur %.2f ans  (~%.3f ppm/an)\n",
            total_rise, years_span, avg_rate))

# ============================================================
# 3. Décomposition saisonnière (STL)
# ============================================================
cat("\n========== 3. Décomposition saisonnière (STL) ==========\n")

co2_ts <- ts(co2$average,
             start     = c(co2$year[1], co2$month[1]),
             frequency = 12)

stl_fit <- stl(co2_ts, s.window = "periodic", robust = TRUE)
stl_df <- as.data.frame(stl_fit$time.series) |>
  mutate(date = co2$date, observed = co2$average)

amp_seasonal <- diff(range(stl_df$seasonal))
cat("Amplitude moyenne du cycle saisonnier (STL) :",
    round(amp_seasonal, 3), "ppm\n")

p_stl <- stl_df |>
  pivot_longer(cols = c(observed, trend),
               names_to = "component", values_to = "value") |>
  mutate(component = factor(component, levels = c("observed", "trend"))) |>
  ggplot(aes(date, value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ component, scales = "free_y", ncol = 1) +
  labs(title    = "Décomposition STL du CO2 mondial (observed + trend)",
       subtitle = "Source : NOAA GML, moyennes mensuelles globales",
       x = NULL, y = "ppm")
save_plot(p_stl, "03_stl_decomposition", h = 5)

# ============================================================
# 4. Cycle saisonnier détaillé
# ============================================================
cat("\n========== 4. Cycle saisonnier détaillé ==========\n")

co2_anom <- co2 |>
  mutate(anomaly = average - trend)

clim_month <- co2_anom |>
  group_by(month) |>
  summarise(mean_anom = mean(anomaly),
            sd_anom   = sd(anomaly),
            .groups = "drop")
print(clim_month)

p_clim <- ggplot(clim_month, aes(factor(month), mean_anom)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_anom - sd_anom,
                    ymax = mean_anom + sd_anom), width = 0.2) +
  labs(title    = "Cycle saisonnier moyen du CO2 mondial (1979-2025)",
       subtitle = "Anomalie = average - trend désaisonnalisé NOAA",
       x = "Mois", y = "Anomalie (ppm)")
save_plot(p_clim, "04a_cycle_saisonnier_moyen")

amp_dec <- co2_anom |>
  group_by(decade) |>
  summarise(amplitude = max(anomaly) - min(anomaly),
            .groups = "drop")
print(amp_dec)
write.csv(amp_dec, file.path(out_dir, "amplitude_saisonniere_decennie.csv"),
          row.names = FALSE)

p_amp <- ggplot(amp_dec, aes(decade, amplitude)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = round(amplitude, 2)), vjust = -0.4) +
  labs(title = "Amplitude saisonnière du CO2 par décennie",
       x = NULL, y = "Amplitude max-min (ppm)")
save_plot(p_amp, "04b_amplitude_par_decennie")

# Vue empilée des cycles annuels
p_overlay <- ggplot(co2_anom, aes(month, anomaly,
                                  group = year, color = year)) +
  geom_line(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Cycles saisonniers superposés (une ligne par année)",
       x = "Mois", y = "Anomalie (ppm)", color = "Année")
save_plot(p_overlay, "04c_cycles_superposes")

# ============================================================
# 5. Tendance long-terme
# ============================================================
cat("\n========== 5. Tendance long-terme ==========\n")

lm1 <- lm(average ~ decimal, data = co2)
lm2 <- lm(average ~ poly(decimal, 2, raw = TRUE), data = co2)
lm3 <- lm(average ~ poly(decimal, 3, raw = TRUE), data = co2)

cat("\n--- Régression linéaire ---\n");     print(summary(lm1)$coefficients)
cat("\n--- Régression quadratique ---\n");  print(summary(lm2)$coefficients)
cat("\n--- Régression cubique ---\n");      print(summary(lm3)$coefficients)
cat("\nComparaison AIC (plus petit = meilleur) :\n")
print(AIC(lm1, lm2, lm3))

cat("\n--- Mann-Kendall (non-paramétrique) ---\n")
mk <- MannKendall(co2$average); print(mk)

cat("\n--- Pente de Sen ---\n")
sen <- sens.slope(co2$average)
cat("Sen (mensuel)   :", round(sen$estimates, 4), "ppm/mois\n")
cat("Sen (annualisé) :", round(sen$estimates * 12, 4), "ppm/an\n")

co2 <- co2 |>
  mutate(lm1_fit = predict(lm1),
         lm2_fit = predict(lm2),
         lm3_fit = predict(lm3))

p_trend <- ggplot(co2, aes(date, average)) +
  geom_point(alpha = 0.3, size = 0.5, color = "grey40") +
  geom_line(aes(y = lm1_fit, color = "Linéaire"),    linewidth = 0.7) +
  geom_line(aes(y = lm2_fit, color = "Quadratique"), linewidth = 0.7) +
  geom_line(aes(y = lm3_fit, color = "Cubique"),     linewidth = 0.7) +
  scale_color_manual(values = c("Linéaire"   = "blue",
                                "Quadratique"= "red",
                                "Cubique"    = "darkgreen")) +
  labs(title = "Tendance long-terme du CO2 mondial",
       x = NULL, y = "ppm", color = "Modèle")
save_plot(p_trend, "05_tendance_polynomes")

# ============================================================
# 6. Analyse de l'accélération
# ============================================================
cat("\n========== 6. Analyse de l'accélération ==========\n")

annual <- co2 |>
  group_by(year) |>
  summarise(annual_mean = mean(average),
            n_months    = n(),
            .groups = "drop") |>
  filter(n_months >= 6) |>
  mutate(annual_rate = annual_mean - lag(annual_mean))
print(tail(annual, 15))
write.csv(annual, file.path(out_dir, "taux_annuel.csv"), row.names = FALSE)

p_rate <- ggplot(annual |> filter(!is.na(annual_rate)),
                 aes(year, annual_rate)) +
  geom_col(fill = "darkorange") +
  geom_smooth(method = "loess", se = FALSE,
              color = "black", linewidth = 0.7) +
  labs(title    = "Taux annuel de croissance du CO2 mondial",
       subtitle = "Différence des moyennes annuelles successives",
       x = NULL, y = "ppm/an")
save_plot(p_rate, "06a_taux_annuel")

cat("\n--- Détection de ruptures (strucchange) ---\n")
trend_ts <- ts(co2$trend,
               start = c(co2$year[1], co2$month[1]),
               frequency = 12)
bp <- breakpoints(trend_ts ~ time(trend_ts), h = 0.1)
print(summary(bp))

if (!is.null(bp$breakpoints) && !any(is.na(bp$breakpoints))) {
  brk_dates <- co2$date[bp$breakpoints]
  cat("Dates de rupture détectées :\n"); print(brk_dates)

  p_break <- ggplot(co2, aes(date, trend)) +
    geom_line(color = "darkred") +
    geom_vline(xintercept = brk_dates, linetype = "dashed") +
    labs(title    = "Trend désaisonnalisé NOAA et ruptures détectées",
         subtitle = "Lignes pointillées = changements de régime",
         x = NULL, y = "ppm")
  save_plot(p_break, "06b_ruptures")
}

# ============================================================
# 7. Prévision ARIMA (24 mois)
# ============================================================
cat("\n========== 7. Prévision ARIMA ==========\n")

fit_arima <- auto.arima(co2_ts, seasonal = TRUE, stepwise = FALSE,
                        approximation = FALSE)
print(summary(fit_arima))

fcst <- forecast(fit_arima, h = 24)

p_fcst <- autoplot(fcst) +
  labs(title    = "Prévision CO2 mondial (24 mois)",
       subtitle = paste("Modèle :", paste(fit_arima$arma, collapse = "-")),
       x = NULL, y = "ppm")
save_plot(p_fcst, "07_prevision_arima")

# ============================================================
# 8. Visualisations de synthèse
# ============================================================
cat("\n========== 8. Synthèse ==========\n")

p_serie <- ggplot(co2, aes(date)) +
  geom_line(aes(y = average), color = "grey40", alpha = 0.7) +
  geom_line(aes(y = trend),   color = "darkred", linewidth = 0.8) +
  labs(title    = "CO2 atmosphérique mondial 1979-2025",
       subtitle = "Série brute (gris) et tendance désaisonnalisée NOAA (rouge)",
       x = NULL, y = "ppm")
save_plot(p_serie, "08a_serie_complete")

p_zoom <- ggplot(co2, aes(date, average)) +
  geom_line(color = "steelblue") +
  scale_x_date(breaks = "5 years", labels = date_format("%Y")) +
  labs(title = "CO2 mondial - série mensuelle brute",
       x = NULL, y = "ppm")
save_plot(p_zoom, "08b_serie_brute_zoom")

cat("\n=== Tous les outputs sauvegardés dans :",
    out_dir, "===\n")
