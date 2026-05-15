# ============================================================
# Analyse CO2 étendue : Mauna Loa + Global Carbon Budget + Vostok
# Sections 9, 10, 11 (suite de co2_analysis.R)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
})

data_dir <- "/home/mazzez/Bureau/R project/Final Version/CO2"
out_dir  <- "/home/mazzez/Bureau/R project/Final Version/Analyse CO2/outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(out_dir, paste0(name, ".png")),
         p, width = w, height = h, dpi = 150)
}
theme_set(theme_minimal(base_size = 12))

# Constantes physiques utilisées dans toutes les conversions
PPM_TO_GTC   <- 2.124          # 1 ppm CO2 atm <-> 2.124 GtC
GTC_TO_GTCO2 <- 44 / 12        # masse molaire CO2 / C
MTCO2_TO_GTC <- 1 / (1000 * GTC_TO_GTCO2)

# ============================================================
# 9. Comparaison Global (NOAA GML) vs Mauna Loa
# ============================================================
cat("\n========== 9. Global vs Mauna Loa ==========\n")

co2_gl <- read.csv(file.path(data_dir, "co2_mm_gl.csv"),
                   comment.char = "#") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
  select(date, year, month, gl_avg = average, gl_trend = trend)

co2_mlo <- read.csv(file.path(data_dir, "co2_mm_mlo.csv"),
                    comment.char = "#",
                    check.names = FALSE)
names(co2_mlo) <- c("year", "month", "decimal",
                    "mlo_avg", "mlo_deseason",
                    "ndays", "sdev", "unc")
co2_mlo <- co2_mlo |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         mlo_avg      = ifelse(mlo_avg      < 0, NA, mlo_avg),
         mlo_deseason = ifelse(mlo_deseason < 0, NA, mlo_deseason)) |>
  select(date, mlo_avg, mlo_deseason)

both <- inner_join(co2_gl, co2_mlo, by = "date") |>
  mutate(diff       = mlo_avg - gl_avg,
         diff_trend = mlo_deseason - gl_trend)

cat("Période commune :", as.character(min(both$date)), "->",
    as.character(max(both$date)), "(", nrow(both), "mois)\n")
cat("Diff brut    (MLO - Global) : moyenne =",
    round(mean(both$diff,       na.rm = TRUE), 2),
    "ppm  ; sd =", round(sd(both$diff,       na.rm = TRUE), 2), "ppm\n")
cat("Diff trend   (MLO - Global) : moyenne =",
    round(mean(both$diff_trend, na.rm = TRUE), 2),
    "ppm  ; sd =", round(sd(both$diff_trend, na.rm = TRUE), 2), "ppm\n")

p9a <- both |>
  pivot_longer(c(gl_avg, mlo_avg),
               names_to = "site", values_to = "ppm") |>
  mutate(site = recode(site,
                       gl_avg  = "Global (NOAA GML)",
                       mlo_avg = "Mauna Loa")) |>
  ggplot(aes(date, ppm, color = site)) +
  geom_line() +
  scale_color_manual(values = c("Global (NOAA GML)" = "steelblue",
                                "Mauna Loa"         = "tomato")) +
  labs(title = "CO2 mensuel : Global vs Mauna Loa",
       x = NULL, y = "ppm", color = NULL)
save_plot(p9a, "09a_global_vs_mlo")

p9b <- ggplot(both, aes(date, diff)) +
  geom_line(color = "purple", alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE,
              color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title    = "Écart mensuel Mauna Loa - Global",
       subtitle = "Surplus systématique de l'hémisphère nord",
       x = NULL, y = "MLO - Global (ppm)")
save_plot(p9b, "09b_diff_mlo_global")

clim_cmp <- bind_rows(
  both |> mutate(anomaly = gl_avg  - gl_trend,    source = "Global"),
  both |> mutate(anomaly = mlo_avg - mlo_deseason, source = "Mauna Loa")
) |>
  mutate(month_n = month(date)) |>
  group_by(source, month_n) |>
  summarise(mean_anom = mean(anomaly, na.rm = TRUE),
            sd_anom   = sd(anomaly,   na.rm = TRUE),
            .groups = "drop")

p9c <- ggplot(clim_cmp,
              aes(factor(month_n), mean_anom, fill = source)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_anom - sd_anom,
                    ymax = mean_anom + sd_anom),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = c("Global"    = "steelblue",
                               "Mauna Loa" = "tomato")) +
  labs(title    = "Cycle saisonnier moyen : Global vs Mauna Loa",
       subtitle = "Mauna Loa amplifie le signal hémisphérique nord (~3x)",
       x = "Mois", y = "Anomalie (ppm)", fill = NULL)
save_plot(p9c, "09c_cycle_compare")

amp_gl  <- diff(range(filter(clim_cmp, source == "Global")$mean_anom))
amp_mlo <- diff(range(filter(clim_cmp, source == "Mauna Loa")$mean_anom))
cat("Amplitude saisonnière moyenne :  Global =", round(amp_gl, 2),
    "ppm   |  Mauna Loa =", round(amp_mlo, 2),
    "ppm   |  ratio =", round(amp_mlo/amp_gl, 2), "\n")

# ============================================================
# 10. Émissions mondiales (GCB) et fraction airborne
# ============================================================
cat("\n========== 10. Émissions et fraction airborne ==========\n")

gcb <- read.csv(file.path(data_dir, "GCB2025v15_MtCO2_flat.csv")) |>
  filter(Country == "Global", !is.na(Total)) |>
  select(year         = Year,
         total_MtCO2  = Total,
         coal         = Coal,
         oil          = Oil,
         gas          = Gas,
         cement       = Cement,
         flaring      = Flaring,
         other        = Other) |>
  mutate(total_GtC = total_MtCO2 * MTCO2_TO_GTC)

cat("GCB Global : période =", min(gcb$year), "->", max(gcb$year), "\n")
cat("Émissions 2024 : ",
    round(tail(gcb$total_MtCO2, 1) / 1000, 2), "GtCO2  (",
    round(tail(gcb$total_GtC, 1), 2),         "GtC)\n")

co2_annual <- co2_gl |>
  group_by(year) |>
  summarise(annual_mean = mean(gl_avg),
            n = n(), .groups = "drop") |>
  filter(n >= 6) |>
  mutate(d_ppm = annual_mean - lag(annual_mean),
         d_GtC = d_ppm * PPM_TO_GTC)

af <- inner_join(co2_annual |> filter(!is.na(d_GtC)),
                 gcb |> select(year, total_GtC), by = "year") |>
  mutate(airborne_fraction = d_GtC / total_GtC)

cat("Fraction airborne moyenne :",
    round(mean(af$airborne_fraction, na.rm = TRUE), 3),
    "  (médiane :",
    round(median(af$airborne_fraction, na.rm = TRUE), 3), ")\n")
cat("Plage des fractions       :",
    paste(round(range(af$airborne_fraction, na.rm = TRUE), 3),
          collapse = " - "), "\n")

write.csv(af, file.path(out_dir, "fraction_airborne.csv"), row.names = FALSE)

p10a <- gcb |>
  filter(year >= 1900) |>
  pivot_longer(c(coal, oil, gas, cement, flaring, other),
               names_to = "source", values_to = "MtCO2") |>
  ggplot(aes(year, MtCO2 / 1000, fill = source)) +
  geom_area() +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "Émissions mondiales de CO2 par source (1900-2024)",
       subtitle = "Source : Global Carbon Budget 2025v15",
       x = NULL, y = "GtCO2 / an", fill = "Source")
save_plot(p10a, "10a_emissions_par_source")

p10b <- af |>
  pivot_longer(c(total_GtC, d_GtC),
               names_to = "var", values_to = "GtC") |>
  mutate(var = recode(var,
                      total_GtC = "Émissions fossiles",
                      d_GtC     = "Accroissement atmosphérique")) |>
  ggplot(aes(year, GtC, color = var)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Émissions fossiles"          = "darkred",
                                "Accroissement atmosphérique" = "steelblue")) +
  labs(title    = "Émissions fossiles vs accroissement atmosphérique",
       subtitle = "Période 1980-2024, en GtC/an",
       x = NULL, y = "GtC / an", color = NULL)
save_plot(p10b, "10b_emissions_vs_accroissement")

p10c <- ggplot(af, aes(year, airborne_fraction)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = mean(af$airborne_fraction, na.rm = TRUE),
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE,
              color = "black", linewidth = 0.7) +
  scale_y_continuous(labels = percent) +
  labs(title    = "Fraction airborne du CO2 fossile (1980-2024)",
       subtitle = sprintf("Moyenne = %.1f %% (ligne rouge)",
                          100 * mean(af$airborne_fraction,
                                     na.rm = TRUE)),
       x = NULL, y = expression(Delta * CO[2]^atm / Emissions))
save_plot(p10c, "10c_fraction_airborne")

cat("\n--- Régression d_GtC ~ total_GtC ---\n")
lm_af <- lm(d_GtC ~ total_GtC, data = af)
print(summary(lm_af))
cat("Pente (~ fraction airborne implicite) :",
    round(coef(lm_af)["total_GtC"], 3), "\n")

# ============================================================
# 11. Perspective paléoclimatique (Vostok)
# ============================================================
cat("\n========== 11. Vostok (paléo) ==========\n")

vostok <- read.table(file.path(data_dir, "co2nat-noaa.txt"),
                     header = FALSE, comment.char = "#",
                     col.names = c("gas_ageBP", "CO2"),
                     fill = TRUE,
                     stringsAsFactors = FALSE) |>
  mutate(gas_ageBP = suppressWarnings(as.numeric(gas_ageBP)),
         CO2       = suppressWarnings(as.numeric(CO2))) |>
  filter(!is.na(gas_ageBP), !is.na(CO2)) |>
  mutate(year_AD = 1950 - gas_ageBP) |>
  arrange(gas_ageBP)

cat("Vostok : âge max =", max(vostok$gas_ageBP), "ans BP\n")
cat("CO2 plage paléo :",
    round(min(vostok$CO2), 1), "->",
    round(max(vostok$CO2), 1), "ppm  (n =", nrow(vostok), ")\n")

co2_now <- tail(co2_gl$gl_avg, 1)

p11a <- ggplot(vostok, aes(gas_ageBP / 1000, CO2)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = co2_now,
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  annotate("text",
           x = max(vostok$gas_ageBP) / 1000 * 0.6,
           y = co2_now + 8,
           label = sprintf("Niveau actuel : %.0f ppm (2025)", co2_now),
           color = "darkred") +
  scale_x_reverse() +
  labs(title    = "CO2 atmosphérique sur 414 000 ans (Vostok)",
       subtitle = "4 cycles glaciaires-interglaciaires ; le niveau actuel dépasse tous les pics paléo",
       x = "Milliers d'années avant 1950 (BP)",
       y = "CO2 (ppm)")
save_plot(p11a, "11a_vostok_full")

vostok_rates <- vostok |>
  mutate(d_age = gas_ageBP - lag(gas_ageBP),
         d_co2 = CO2       - lag(CO2),
         rate_ppm_per_century = (-d_co2 / d_age) * 100)

max_paleo_rate <- max(vostok_rates$rate_ppm_per_century, na.rm = TRUE)
modern_rate    <- 1.85 * 100   # 1.85 ppm/an = 185 ppm/siècle
cat("Taux paléo maximal       :",
    round(max_paleo_rate, 2), "ppm/siècle\n")
cat("Taux moderne (1979-2025) :",
    round(modern_rate, 2),    "ppm/siècle\n")
cat("Ratio moderne / paléo    :",
    round(modern_rate / max_paleo_rate, 1), "x plus rapide\n")

recent_paleo <- vostok |> filter(gas_ageBP <= 20000)
modern_annual <- co2_gl |>
  group_by(year) |>
  summarise(CO2 = mean(gl_avg), .groups = "drop") |>
  mutate(gas_ageBP = 1950 - year)

p11b <- ggplot() +
  geom_line(data = recent_paleo, aes(gas_ageBP, CO2),
            color = "steelblue", linewidth = 0.8) +
  geom_line(data = modern_annual, aes(gas_ageBP, CO2),
            color = "darkred", linewidth = 1.2) +
  scale_x_reverse(breaks = seq(0, 20000, 2000)) +
  labs(title    = "CO2 sur 20 000 ans : paléo (Vostok, bleu) + moderne (NOAA, rouge)",
       subtitle = "La hausse moderne est sans précédent dans cette fenêtre",
       x = "Années avant 1950 (BP)", y = "CO2 (ppm)")
save_plot(p11b, "11b_vostok_20k_zoom")

cat("\n=== Sections 9-11 sauvegardées dans :", out_dir, "===\n")
