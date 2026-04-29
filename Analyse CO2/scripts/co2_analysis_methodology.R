# ============================================================
# Sections 12-23 : raffinements méthodologiques (A1-A5)
#                  + enrichissements analytiques (B1-B7)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(lubridate)
  library(scales)
  library(tseries)        # adf, kpss, pp
  library(seasonal)       # X-13 ARIMA
  library(mFilter)        # hpfilter
  library(boot)           # bootstrap
  library(forecast)       # auto.arima, tsCV
  library(KFAS)           # state-space
  library(strucchange)
  library(Kendall); library(trend)
})

Sys.setenv(X13_PATH = x13binary::x13path())

data_dir <- "/home/mazzez/Bureau/R project/Data/CO2"
out_dir  <- "/home/mazzez/Bureau/R project/Final Version/Analyse CO2/outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(out_dir, paste0(name, ".png")),
         p, width = w, height = h, dpi = 150)
}
theme_set(theme_minimal(base_size = 12))

co2 <- read.csv(file.path(data_dir, "co2_mm_gl.csv"), comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  arrange(date)

co2_ts   <- ts(co2$average, start = c(co2$year[1], co2$month[1]), frequency = 12)
trend_ts <- ts(co2$trend,   start = c(co2$year[1], co2$month[1]), frequency = 12)

# ============================================================
# 12. A1 — Tests de stationnarité (ADF, KPSS, Phillips-Perron)
# ============================================================
cat("\n========== 12. Tests de stationnarité ==========\n")

run_stationarity <- function(x, name) {
  cat("\n--", name, "--\n")
  cat("ADF      p =",
      round(suppressWarnings(adf.test(x))$p.value, 4),  "\n")
  cat("KPSS     p =",
      round(suppressWarnings(kpss.test(x))$p.value, 4), "\n")
  cat("PP       p =",
      round(suppressWarnings(pp.test(x))$p.value, 4),   "\n")
}
run_stationarity(co2$average,                "Série brute")
run_stationarity(co2$trend,                  "Trend NOAA désaisonnalisé")
run_stationarity(diff(co2$average),          "Δ première (différence ordre 1)")
run_stationarity(diff(co2$average, 12),      "Δ saisonnière (lag 12)")
run_stationarity(diff(diff(co2$average, 12)),"Δ + Δ12")
cat("\nInterprétation :\n",
    "  ADF p > 0.05  =>  non stationnaire\n",
    "  KPSS p < 0.05 =>  non stationnaire\n",
    "  PP  p > 0.05  =>  non stationnaire\n", sep = "")

# ============================================================
# 13. A2 — X-13 ARIMA-SEATS vs STL vs NOAA
# ============================================================
cat("\n========== 13. X-13 ARIMA-SEATS ==========\n")

x13 <- tryCatch(seasonal::seas(co2_ts),
                error = function(e) { message("X-13 error : ", e$message); NULL })

if (!is.null(x13)) {
  trnd_x13 <- as.numeric(trendcycle(x13))
  stl_fit  <- stl(co2_ts, s.window = "periodic")
  trend_stl <- as.numeric(stl_fit$time.series[, "trend"])

  comp_df <- data.frame(date = co2$date,
                        NOAA = co2$trend,
                        STL  = trend_stl,
                        X13  = trnd_x13)

  p13 <- comp_df |>
    pivot_longer(c(NOAA, STL, X13),
                 names_to = "method", values_to = "trend") |>
    ggplot(aes(date, trend, color = method)) +
    geom_line(linewidth = 0.7) +
    labs(title = "Trend désaisonnalisé : 3 méthodes",
         x = NULL, y = "ppm", color = NULL)
  save_plot(p13, "13_x13_vs_stl_vs_noaa")

  cat("Écarts max entre méthodes (ppm) :\n")
  cat("  NOAA - STL :", round(max(abs(comp_df$NOAA - comp_df$STL)), 3), "\n")
  cat("  NOAA - X13 :", round(max(abs(comp_df$NOAA - comp_df$X13)), 3), "\n")
  cat("  STL  - X13 :", round(max(abs(comp_df$STL  - comp_df$X13)), 3), "\n")
  print(summary(x13))
}

# ============================================================
# 14. A3 — Bootstrap de la pente de Sen
# ============================================================
cat("\n========== 14. Bootstrap pente de Sen ==========\n")

set.seed(123)
sen_boot <- function(data, idx) {
  sub <- data[sort(idx), ]
  trend::sens.slope(sub$average)$estimates
}
b  <- boot::boot(co2, sen_boot, R = 500)
ci <- boot::boot.ci(b, type = c("perc", "bca"))

cat("Sen original         :", round(b$t0 * 12, 4), "ppm/an\n")
cat("IC 95 % (percentile) :",
    round(ci$percent[4] * 12, 4), "-",
    round(ci$percent[5] * 12, 4), "ppm/an\n")
cat("IC 95 % (BCa)        :",
    round(ci$bca[4] * 12, 4), "-",
    round(ci$bca[5] * 12, 4), "ppm/an\n")

p14 <- ggplot(data.frame(annual = b$t * 12), aes(annual)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = b$t0 * 12, color = "red", linewidth = 1) +
  labs(title = sprintf("Bootstrap pente de Sen (R = 500) : %.3f ppm/an",
                       b$t0 * 12),
       subtitle = sprintf("IC 95 %% percentile : [%.3f ; %.3f]",
                          ci$percent[4]*12, ci$percent[5]*12),
       x = "Pente Sen (ppm/an)", y = "Fréquence")
save_plot(p14, "14_bootstrap_sen")

# ============================================================
# 15. A4 — Validation croisée ARIMA (rolling origin)
# ============================================================
cat("\n========== 15. CV rolling-origin ARIMA ==========\n")

arima_fixed <- auto.arima(co2_ts, seasonal = TRUE)
cat("Modèle :", paste(arima_fixed$arma, collapse = "-"), "\n")

f_fcst <- function(x, h) forecast(Arima(x, model = arima_fixed), h = h)
e <- tsCV(co2_ts, f_fcst, h = 12, initial = 120)
mae  <- colMeans(abs(e), na.rm = TRUE)
rmse <- sqrt(colMeans(e^2,  na.rm = TRUE))
cv_df <- data.frame(horizon = 1:12, MAE = mae, RMSE = rmse)
print(cv_df)
write.csv(cv_df, file.path(out_dir, "cv_arima.csv"), row.names = FALSE)

p15 <- cv_df |>
  pivot_longer(c(MAE, RMSE), names_to = "metric", values_to = "ppm") |>
  ggplot(aes(horizon, ppm, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Validation croisée rolling-origin ARIMA",
       subtitle = "Erreur en fonction de l'horizon de prévision",
       x = "Horizon (mois)", y = "Erreur (ppm)", color = NULL)
save_plot(p15, "15_cv_arima")

# ============================================================
# 16. A5 — NOAA / STL / Hodrick-Prescott
# ============================================================
cat("\n========== 16. NOAA / STL / HP ==========\n")

stl_fit   <- stl(co2_ts, s.window = "periodic")
trend_stl <- as.numeric(stl_fit$time.series[, "trend"])
hp        <- mFilter::hpfilter(co2_ts, freq = 14400, type = "lambda")
trend_hp  <- as.numeric(hp$trend)

cmp <- data.frame(date = co2$date,
                  NOAA = co2$trend,
                  STL  = trend_stl,
                  HP   = trend_hp)

p16 <- cmp |>
  pivot_longer(-date, names_to = "method", values_to = "ppm") |>
  ggplot(aes(date, ppm, color = method)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Trend désaisonnalisé : NOAA / STL / Hodrick-Prescott",
       x = NULL, y = "ppm", color = NULL)
save_plot(p16, "16_three_trends")

cat("Diffs max (ppm) :\n")
cat("  NOAA - STL :", round(max(abs(cmp$NOAA - cmp$STL)), 3), "\n")
cat("  NOAA - HP  :", round(max(abs(cmp$NOAA - cmp$HP)),  3), "\n")
cat("  STL  - HP  :", round(max(abs(cmp$STL  - cmp$HP)),  3), "\n")

# ============================================================
# 17. B1 — Modèle exponentiel C(t) = C0 * exp(r*t)
# ============================================================
cat("\n========== 17. Modèle exponentiel ==========\n")

co2 <- co2 |> mutate(t_yrs = decimal - decimal[1])
fit_exp <- nls(average ~ C0 * exp(r * t_yrs),
               data  = co2,
               start = list(C0 = 336, r = 0.005))
print(summary(fit_exp))

co2$exp_fit <- predict(fit_exp)
co2$lm_fit  <- predict(lm(average ~ decimal, data = co2))

r_hat <- coef(fit_exp)["r"]
double_yrs <- log(2) / r_hat

p17 <- ggplot(co2, aes(date, average)) +
  geom_point(alpha = 0.3, color = "grey40", size = 0.4) +
  geom_line(aes(y = exp_fit, color = "Exponentiel"), linewidth = 0.8) +
  geom_line(aes(y = lm_fit,  color = "Linéaire"),    linewidth = 0.8) +
  scale_color_manual(values = c("Exponentiel" = "red",
                                "Linéaire"    = "blue")) +
  labs(title    = "Hausse exponentielle vs linéaire",
       subtitle = sprintf("Taux exp r = %.4f /an, doublement = %.1f ans",
                          r_hat, double_yrs),
       x = NULL, y = "ppm", color = NULL)
save_plot(p17, "17_modele_exponentiel")

cat("AIC linéaire vs exponentiel :\n")
print(AIC(lm(average ~ decimal, data = co2), fit_exp))

# ============================================================
# 18. B2 — ACF / PACF des résidus STL
# ============================================================
cat("\n========== 18. ACF / PACF résidus STL ==========\n")

resid_stl <- stl_fit$time.series[, "remainder"]
ljb <- Box.test(resid_stl, lag = 24, type = "Ljung-Box")
cat("Ljung-Box (lag 24) p =", round(ljb$p.value, 4), "\n")

png(file.path(out_dir, "18_acf_pacf_residus_stl.png"),
    width = 1200, height = 800, res = 150)
par(mfrow = c(2, 1))
acf( resid_stl, lag.max = 60, main = "ACF des résidus STL")
pacf(resid_stl, lag.max = 60, main = "PACF des résidus STL")
dev.off()

# ============================================================
# 19. B3 — Périodogramme spectral
# ============================================================
cat("\n========== 19. Spectre ==========\n")

spec_input <- ts(co2$average - co2$trend, frequency = 12)
sp <- spectrum(spec_input, plot = FALSE)
# Pour ts(freq=12), sp$freq est en cycles/an  ->  période = 12 / freq  (en mois)
spec_df <- data.frame(period_months = 12 / sp$freq,
                      density       = sp$spec)

top5 <- spec_df |> arrange(desc(density)) |> head(5)
cat("Top 5 périodes (mois) :\n"); print(top5)

p19 <- ggplot(spec_df |> filter(period_months <= 60),
              aes(period_months, density)) +
  geom_line(color = "darkblue") +
  geom_vline(xintercept = 12, color = "red", linetype = "dashed") +
  annotate("text", x = 13, y = max(spec_df$density) * 0.9,
           label = "12 mois", color = "red", hjust = 0) +
  labs(title = "Périodogramme de l'anomalie (average − trend)",
       x = "Période (mois)", y = "Densité spectrale")
save_plot(p19, "19_periodogramme")

# ============================================================
# 20. B4 — Régression taux annuel ~ ENSO (Niño 3.4)
# ============================================================
cat("\n========== 20. Taux annuel ~ ENSO ==========\n")

oni <- read.table(file.path(data_dir, "oni.ascii.txt"),
                  header = TRUE, stringsAsFactors = FALSE)

seas_to_month <- c(DJF=1,JFM=2,FMA=3,MAM=4,AMJ=5,MJJ=6,
                   JJA=7,JAS=8,ASO=9,SON=10,OND=11,NDJ=12)
oni <- oni |>
  mutate(month = seas_to_month[SEAS]) |>
  rename(year = YR, oni = ANOM) |>
  select(year, month, oni)

annual <- co2 |>
  group_by(year) |>
  summarise(annual_mean = mean(average), n = n(), .groups = "drop") |>
  filter(n >= 6) |>
  mutate(annual_rate = annual_mean - lag(annual_mean))

oni_annual <- oni |>
  group_by(year) |>
  summarise(oni_annual = mean(oni, na.rm = TRUE), .groups = "drop")

ar_oni <- inner_join(annual, oni_annual, by = "year") |>
  filter(!is.na(annual_rate))

lm_oni <- lm(annual_rate ~ oni_annual, data = ar_oni)
print(summary(lm_oni))
r_pearson <- cor(ar_oni$annual_rate, ar_oni$oni_annual)
cat("Corrélation Pearson taux ↔ ONI :", round(r_pearson, 3), "\n")

p20 <- ggplot(ar_oni, aes(oni_annual, annual_rate)) +
  geom_point(aes(color = year), size = 2.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_viridis_c() +
  labs(title    = "Taux annuel CO2 vs indice ENSO (Niño 3.4)",
       subtitle = sprintf("r = %.2f, pente = %.2f ppm/an par unité ONI (p = %.3g)",
                          r_pearson, coef(lm_oni)[2],
                          summary(lm_oni)$coefficients[2, 4]),
       x = "ONI annuel (anomalie SST °C)",
       y = "Taux CO2 (ppm/an)", color = "Année")
save_plot(p20, "20_taux_vs_enso")

# Test avec lag de 6 mois (réponse retardée du puits terrestre)
oni_lag6 <- oni |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  arrange(date) |>
  mutate(oni_lag6 = lag(oni, 6))
oni_lag6_annual <- oni_lag6 |>
  group_by(year) |>
  summarise(oni_lag6 = mean(oni_lag6, na.rm = TRUE), .groups = "drop")
ar_oni_lag <- inner_join(annual, oni_lag6_annual, by = "year") |>
  filter(!is.na(annual_rate), !is.na(oni_lag6))
cat("Corrélation taux ↔ ONI(lag 6 mois) :",
    round(cor(ar_oni_lag$annual_rate, ar_oni_lag$oni_lag6), 3), "\n")

# ============================================================
# 21. B5 — Effet COVID-19
# ============================================================
cat("\n========== 21. Effet COVID-19 ==========\n")

co2_pre <- window(co2_ts, end = c(2019, 12))
fit_pre <- auto.arima(co2_pre, seasonal = TRUE)
fcst_covid <- forecast(fit_pre, h = 24)

obs_covid <- as.numeric(window(co2_ts, start = c(2020, 1), end = c(2021, 12)))
covid_df <- data.frame(
  date      = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
  observed  = obs_covid,
  predicted = as.numeric(fcst_covid$mean),
  lo80      = as.numeric(fcst_covid$lower[, "80%"]),
  hi80      = as.numeric(fcst_covid$upper[, "80%"])
) |> mutate(anomaly = observed - predicted)

cat("Anomalie cumulée 2020-2021 :",
    round(sum(covid_df$anomaly), 2), "ppm\n")
cat("Anomalie moyenne mensuelle  :",
    round(mean(covid_df$anomaly), 3), "ppm\n")
write.csv(covid_df, file.path(out_dir, "covid_anomalie.csv"), row.names = FALSE)

p21 <- ggplot(covid_df, aes(date)) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "grey80") +
  geom_line(aes(y = predicted, color = "Prédit (ARIMA pré-2020)"),
            linewidth = 0.8) +
  geom_line(aes(y = observed,  color = "Observé"), linewidth = 0.8) +
  scale_color_manual(values = c("Observé" = "red",
                                "Prédit (ARIMA pré-2020)" = "blue")) +
  labs(title    = "Effet COVID-19 sur le CO2 atmosphérique global",
       subtitle = "Modèle ARIMA fitté sur 1979-2019 puis projeté",
       x = NULL, y = "ppm", color = NULL)
save_plot(p21, "21_covid_effect")

# ============================================================
# 22. B6 — Cycle saisonnier hémisphérique : NH vs SH
# ============================================================
cat("\n========== 22. Cycle saisonnier hémisphérique ==========\n")

mlo <- read.csv(file.path(data_dir, "co2_mm_mlo.csv"), comment.char = "#",
                check.names = FALSE)
names(mlo) <- c("year","month","decimal","mlo_avg","mlo_deseason",
                "ndays","sdev","unc")
mlo <- mlo |>
  mutate(date    = as.Date(paste(year, month, "01", sep = "-")),
         mlo_avg = ifelse(mlo_avg < 0, NA, mlo_avg),
         mlo_deseason = ifelse(mlo_deseason < 0, NA, mlo_deseason))

spo <- read.table(file.path(data_dir,
                            "co2_spo_surface-flask_1_ccgg_month.txt"),
                  comment.char = "#",
                  col.names = c("site", "year", "month", "spo_avg"),
                  fill = TRUE, stringsAsFactors = FALSE) |>
  mutate(year    = suppressWarnings(as.integer(year)),
         month   = suppressWarnings(as.integer(month)),
         spo_avg = suppressWarnings(as.numeric(spo_avg))) |>
  filter(!is.na(year), !is.na(month), !is.na(spo_avg)) |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

cat("MLO : période =", as.character(min(mlo$date, na.rm = TRUE)),
    "->", as.character(max(mlo$date, na.rm = TRUE)),
    "(", sum(!is.na(mlo$mlo_avg)), "mois)\n")
cat("SPO : période =", as.character(min(spo$date)),
    "->", as.character(max(spo$date)),
    "(", nrow(spo), "mois)\n")

# anomalie = avg - moyenne mobile centrée 12 mois
moving_anom <- function(x) {
  as.numeric(x - stats::filter(x, rep(1/12, 12), sides = 2))
}
mlo$anom <- moving_anom(mlo$mlo_avg)
spo$anom <- moving_anom(spo$spo_avg)

clim_hem <- bind_rows(
  mlo |> filter(!is.na(anom)) |>
        select(month, anom) |> mutate(site = "Mauna Loa (NH)"),
  spo |> filter(!is.na(anom)) |>
        select(month, anom) |> mutate(site = "South Pole (SH)")
) |>
  group_by(site, month) |>
  summarise(mean_anom = mean(anom),
            sd_anom   = sd(anom), .groups = "drop")

p22 <- ggplot(clim_hem, aes(factor(month), mean_anom, fill = site)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_anom - sd_anom,
                    ymax = mean_anom + sd_anom),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = c("Mauna Loa (NH)"  = "tomato",
                               "South Pole (SH)" = "steelblue")) +
  labs(title    = "Cycle saisonnier moyen : NH vs SH",
       subtitle = "Cycles déphasés et amplitude NH >> SH",
       x = "Mois", y = "Anomalie (ppm)", fill = NULL)
save_plot(p22, "22_cycle_hemispherique")

amp_mlo_h <- diff(range(filter(clim_hem, site == "Mauna Loa (NH)")$mean_anom))
amp_spo_h <- diff(range(filter(clim_hem, site == "South Pole (SH)")$mean_anom))
cat("Amplitude saisonnière MLO :", round(amp_mlo_h, 2), "ppm\n")
cat("Amplitude saisonnière SPO :", round(amp_spo_h, 2), "ppm\n")
cat("Ratio NH / SH             :", round(amp_mlo_h / amp_spo_h, 1), "\n")

# ============================================================
# 23. B7 — Modèle state-space (KFAS)
# ============================================================
cat("\n========== 23. State-space (KFAS) ==========\n")

ssm <- SSModel(co2_ts ~ SSMtrend(degree = 2, Q = list(NA, NA)) +
                       SSMseasonal(period = 12, Q = NA),
               H = NA)
fit_ssm <- fitSSM(ssm, inits = c(-5, -5, -5, -5))
out_ssm <- KFS(fit_ssm$model)

trend_ssm  <- as.numeric(out_ssm$alphahat[, "level"])
slope_ssm  <- as.numeric(out_ssm$alphahat[, "slope"])

ssm_df <- data.frame(date = co2$date,
                     observed   = co2$average,
                     trend_ssm  = trend_ssm,
                     slope_ssm  = slope_ssm)
write.csv(ssm_df, file.path(out_dir, "ssm_components.csv"), row.names = FALSE)

p23a <- ggplot(ssm_df, aes(date)) +
  geom_line(aes(y = observed),  color = "grey60", alpha = 0.6) +
  geom_line(aes(y = trend_ssm), color = "red", linewidth = 0.8) +
  labs(title    = "State-space (KFAS) : niveau lissé",
       subtitle = "Niveau + tendance (degré 2) + saison période 12",
       x = NULL, y = "ppm")
save_plot(p23a, "23a_ssm_level")

p23b <- ggplot(ssm_df, aes(date, slope_ssm * 12)) +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  labs(title    = "Pente instantanée (slope) du modèle state-space",
       subtitle = "Convertie en ppm/an, équivalent du taux annuel",
       x = NULL, y = "ppm/an")
save_plot(p23b, "23b_ssm_slope")

cat("Variances finales du modèle :\n")
cat("  H (obs)   :", round(fit_ssm$model$H[1, 1, 1], 6), "\n")
print(fit_ssm$model$Q)

cat("\n=== Sections 12-23 sauvegardées dans", out_dir, "===\n")
