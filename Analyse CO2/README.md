# Analyse CO2 — Projet R Climat & CO2

Première phase du projet : analyse statistique complète de la concentration atmosphérique mondiale de CO2 sur la période 1979-2025, mise en perspective avec Mauna Loa, le Global Carbon Budget, l'enregistrement paléo de Vostok, et l'indice ENSO.

## Structure du dossier

```
Analyse CO2/
├── scripts/                          Code R exécutable, à lancer dans cet ordre
│   ├── 00_install_packages.R         installation des 14 packages CRAN
│   ├── co2_analysis.R                sections 1-8   (analyse de base)
│   ├── co2_analysis_extended.R       sections 9-11  (Mauna Loa + GCB + Vostok)
│   └── co2_analysis_methodology.R    sections 12-23 (raffinements + enrichissements)
│
├── rapport/                          Rapport reproductible
│   ├── CO2_report.Rmd                source RMarkdown unique (23 sections)
│   ├── CO2_report.html               version HTML rendue (à ouvrir dans un navigateur)
│   ├── CO2_report.md                 version markdown brute
│   ├── figure/                       figures du knit Markdown brut
│   ├── CO2_report_files/             figures du knit HTML
│   ├── cache/, CO2_report_cache/     caches knitr (regeneration rapide)
│
├── outputs/                          Sorties des scripts (37 fichiers : 30 PNG + 7 CSV)
│   ├── 03_stl_decomposition.png      ... 23a_ssm_level.png, 23b_ssm_slope.png
│   ├── stats_decennie.csv
│   ├── amplitude_saisonniere_decennie.csv
│   ├── taux_annuel.csv
│   ├── fraction_airborne.csv
│   ├── cv_arima.csv
│   ├── covid_anomalie.csv
│   └── ssm_components.csv
│
└── notes/                            Documentation manuelle / récap
    ├── Récapitulatif détaillé — Analyse CO2.txt
    ├── 1ere lecture.txt
    └── general view/
```

## Sources de données

Toutes situées dans `/home/mazzez/Bureau/R project/Data/CO2/` :

| Dataset | Fichier | Période |
|---|---|---|
| CO2 mondial moyen mensuel (NOAA GML) | `co2_mm_gl.csv` | 1979-01 → 2025-09 |
| CO2 Mauna Loa (NOAA / SIO) | `co2_mm_mlo.csv` | 1958-03 → 2026-02 |
| CO2 South Pole flask (NOAA) | `co2_spo_surface-flask_1_ccgg_month.txt` | 1975-07 → 2024-12 |
| Émissions CO2 fossiles (Global Carbon Budget 2025v15) | `GCB2025v15_MtCO2_flat.csv` | 1750 → 2024 |
| CO2 paléo Vostok (NOAA Paleoclimatology) | `co2nat-noaa.txt` | 414 085 → 2 342 BP |
| Indice ENSO Niño 3.4 (NOAA CPC) | `oni.ascii.txt` | 1950 → présent |

## Reproduire l'analyse

```bash
cd "/home/mazzez/Bureau/R project/Final Version/Analyse CO2/scripts"
Rscript 00_install_packages.R
Rscript co2_analysis.R
Rscript co2_analysis_extended.R
Rscript co2_analysis_methodology.R
```

Pour régénérer le rapport HTML (nécessite `pandoc`) :

```bash
cd "../rapport"
Rscript -e 'rmarkdown::render("CO2_report.Rmd")'
```

## Conclusions principales

1. Hausse confirmée et accélérée : **+86 ppm en 47 ans**, taux de Sen 1.881 ppm/an [IC 95 % 1.774-1.965].
2. AIC privilégie un modèle exponentiel (r = 0.5 %/an, doublement 137 ans) au modèle linéaire.
3. Cycle saisonnier en élargissement : **+18 %** d'amplitude entre 1980s et 2020s. Ratio NH/SH = 5.6×.
4. **53 %** des émissions fossiles 1980-2024 restent dans l'atmosphère (fraction airborne).
5. Hausse moderne **30× plus rapide** que toute hausse paléo des 414 000 dernières années.
6. ENSO module le taux annuel avec un lag de 6 mois (r = +0.28).
7. Effet COVID-19 mesurable mais faible : −1.86 ppm cumulés en 2020-2021.
8. Sept ruptures de régime détectées (1987, 1992, 1998, 2002, 2009, 2015, 2021).

## Suite

La phase 2 du projet (à venir) traitera l'**extraction et la corrélation des 18 variables climatiques globales** issues des réanalyses NCAR (CFSR 1979-2010 + CFSv2 2011-2025) à la résolution 2.5° × 2.5°.
