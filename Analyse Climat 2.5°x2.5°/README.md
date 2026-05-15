# Analyse Climat 2.5°x2.5° — Phases 2 & 3 du projet

Seconde moitié du projet R Climat & CO2. Cette partie extrait 18 variables climatiques globales depuis les réanalyses NCAR (CFSR + CFSv2, résolution 2.5° × 2.5°) sur la période 1979-01 → 2025-12, puis étudie leur lien statistique avec la concentration atmosphérique mondiale de CO2 (NOAA GML).

## Structure du dossier

```
Analyse Climat 2.5°x2.5°/
├── scripts/                          Pipeline en 12 étapes, à lancer dans cet ordre
│   ├── 15_extract_grib_subset.sh     **(ÉTAPE 0)** extrait 18 records GRIB de chaque
│   │                                 fichier source vers Final Version/2.5° x 2.5° subset/
│   │                                 (3.5 Go → 172 Mo, 1 min, à faire UNE fois)
│   ├── 01_extract_subset.sh          subset GRIB → NetCDF (16 sec, lit le subset)
│   ├── 02_global_means.R             moyennes globales pondérées cos(lat)
│   ├── 03_validation.R               sanity checks, plots, Cloud Radiative Effects
│   ├── 04_merge_with_co2.R           fusion avec co2_mm_gl.csv + corrélations brutes
│   ├── 05_preparation.R              5 représentations temporelles (level/anom/resid/d1/d12)
│   ├── 06_correlations.R             corrélations + heatmap + analyse en lag
│   ├── 07_regressions.R              régression complète, stepwise AIC, lasso CV
│   ├── 08_granger.R                  tests de causalité Granger (resid + d12)
│   ├── 09_synthese.R                 tableau de synthèse final + bilan phase 3
│   ├── 10_per_variable_analysis.R    fiches détaillées (4 plots/var) + stats
│   ├── 11_trends_summary.R           tendances Sen+bootstrap+Mann-Kendall, grilles comparatives
│   ├── 12_homogenization.R           détection + correction du saut CFSR→CFSv2 (jan 2011)
│   ├── 13_phase3_homog_comparison.R  refait la phase 3 sur version brute vs homogénéisée
│   └── 14_verify_grib_codes.sh       vérifie l'identité des codes GRIB2 entre CFSR et CFSv2
│
├── outputs/                          Données et figures produites
│   ├── monthly_global_means_25.csv   564 mois × 18 variables (entrée canonique)
│   ├── cre_monthly_25.csv            Cloud Radiative Effects (SW/LW/net)
│   ├── climate_co2_monthly.csv       fusion climat + CO2
│   ├── series_transformed.rds        liste des 5 représentations
│   ├── series_{level,anom,resid,d1,d12}.csv   représentations en CSV
│   ├── correlations_4repr.csv        corrélations sur 5 représentations
│   ├── correlations_brut.csv         corrélations brutes (vue 1er ordre)
│   ├── lag_correlations.csv          corrélations vs lag (-12..+12 mois)
│   ├── lasso_path.csv                chemin de régularisation lasso
│   ├── regression_summary.txt        résumés des modèles complet/stepwise/lasso
│   ├── granger_results.csv           tests Granger (resid + d12)
│   ├── synthese_finale.csv           tableau master des 21 variables
│   ├── per_variable_stats.csv        stats détaillées par variable (Sen, MK, amplitude…)
│   ├── trends_summary.csv            tendances Sen + IC bootstrap pour les 21 vars
│   ├── cfsr_to_cfsv2_jumps.csv       saut CFSR→CFSv2 estimé pour chaque variable
│   ├── monthly_global_means_25_homog.csv   version homogénéisée (saut retiré)
│   ├── comparison_homog_correlations.csv   corrélations brute vs homog (résidus)
│   ├── grib_codes_verification.csv         identité des codes GRIB2 CFSR ↔ CFSv2 (18/18 OK)
│   ├── per_variable/                 84 plots PNG (21 variables × 4 plots chacune)
│   │   └── <VAR>/01_timeseries_loess.png, 02_stl_decomposition.png,
│   │              03_seasonal_climato.png, 04_heatmap_anomaly.png
│   └── plots/                        22 figures PNG (synthèse + comparaisons + homog)
│
└── README.md                         ce fichier
```

## Sources de données

| Dataset | Localisation | Période | Volume |
|---|---|---|---|
| Réanalyses CFSR (1979-2010) + CFSv2 (2011-2026) | `/Data/2.5° x 2.5°/YYYY/pgbl04.gdas.YYYYMM.grb2` | 1979-01 → 2025-12 | 3.5 Go en GRIB2 |
| CO2 mondial NOAA GML | `/Final Version/CO2/co2_mm_gl.csv` | 1979-01 → 2025-12 | 24 Ko |

## Variables retenues (18)

| # | Code | Niveau | Famille |
|---|---|---|---|
| 1 | `T2m` | 2 m above ground | Thermo air surface |
| 2 | `T500` | 500 hPa | Thermo mid-troposphère |
| 3 | `SPFH2m` | 2 m above ground | Humidité spécifique |
| 4 | `PWAT` | colonne entière | Vapeur d'eau intégrée |
| 5 | `APCP` | surface | Précipitations cumulées |
| 6 | `TCDC` | colonne entière | Couverture nuageuse totale |
| 7 | `DLWRF` | surface | LW descendant (effet de serre) |
| 8 | `ULWRF` | surface | LW ascendant |
| 9 | `DSWRF` | surface | SW descendant |
| 10 | `PRMSL` | mean sea level | Pression réduite |
| 11 | `USWRF` | surface | SW réfléchi |
| 12 | `CSDSF` | surface | SW descendant ciel clair |
| 13 | `CSUSF` | surface | SW ascendant ciel clair |
| 14 | `CSDLF` | surface | LW descendant ciel clair |
| 15 | `CSULF` | surface | LW ascendant ciel clair |
| 16 | `CDUVB` | surface | UV-B ciel clair |
| 17 | `DUVB` | surface | UV-B all-sky |
| 18 | `ALBDO` | surface | Albédo de surface |

Plus 3 indicateurs dérivés (Cloud Radiative Effects) : `CRE_SW`, `CRE_LW`, `CRE_net`.

## Reproduire le pipeline

```bash
cd "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/scripts"

bash   15_extract_grib_subset.sh    # 1 min  (3.5 Go -> 172 Mo, à faire 1 fois)
bash   01_extract_subset.sh         # 16 sec (subset GRIB -> NetCDF)
Rscript 02_global_means.R           # 10 min (moyennes globales)
Rscript 03_validation.R             # < 1 min
Rscript 04_merge_with_co2.R         # < 1 min
Rscript 05_preparation.R            # < 1 min
Rscript 06_correlations.R           # < 1 min
Rscript 07_regressions.R            # < 1 min
Rscript 08_granger.R                # < 1 min
Rscript 09_synthese.R               # < 1 min
Rscript 10_per_variable_analysis.R  # ~2 min (84 plots)
Rscript 11_trends_summary.R         # ~1 min (bootstrap × 21 vars)
Rscript 12_homogenization.R         # < 1 min (détection saut CFSR→CFSv2)
Rscript 13_phase3_homog_comparison.R # < 1 min (refait phase 3 homogénéisée)
```

Total : ~18 minutes pour une exécution complète depuis 0.

Pour générer le rapport HTML final :

```bash
Rscript -e 'rmarkdown::render("Climat_report.Rmd")'
```

## Conclusions principales

1. **Sanity checks 18/18 OK** : toutes les moyennes globales tombent dans leur plage climatologique attendue (T2m ≈ 288 K, PWAT ≈ 25 kg/m², DLWRF ≈ 341 W/m², TCDC ≈ 60 %, ALBDO ≈ 11 %, PRMSL ≈ 1011 hPa). Cloud Radiative Effect net = **−19.7 W/m²**, conforme au consensus IPCC (~−20 W/m²).

2. **Corrélations très sensibles à la représentation temporelle** :
   - sur les **niveaux bruts** : 11 variables avec |r| > 0.4 (PWAT 0.88, T2m 0.80, T500 0.84) → **trompeur**, dominé par la tendance commune.
   - sur les **résidus détendrés** (signal interannuel pur) : seulement 5 variables avec |r| > 0.3 (CRE_LW −0.52, CRE_SW +0.49, DSWRF +0.44, CRE_net +0.43, PRMSL −0.39).
   - 10 variables sur 21 sont **"spurious"** : forte corrélation sur anomalies brutes, faible sur résidus → leur lien apparent au CO2 vient uniquement de la trend partagée.

3. **Régression multivariée stepwise** : R² = **0.748** sur résidus avec 12 prédicteurs (T500, APCP, DSWRF, PRMSL, CSUSF, CSDLF, CSULF, DUVB, ALBDO …). Le modèle reproduit bien l'enveloppe interannuelle 1979-2025.

4. **Tests de causalité Granger (lag = 6 mois)** :
   - sur **résidus** : aucune relation causale significative (signal trop bruité).
   - sur **d12 (taux annuel)** : **15 variables sur 21** causent significativement le CO2 (X → CO2, p < 0.05), dont 5 bidirectionnellement. Top causal : T500 (p = 2e-8), SPFH2m (p = 6e-9), PWAT (p = 1e-8), CSDLF (p = 3e-9). Seulement 8 variables ont CO2 → X significatif.

5. **Sens dominant à l'échelle interannuelle : CLIMAT → CO2.** Les variations climatiques précèdent de 6 mois les variations du taux annuel CO2, en cohérence avec le résultat ENSO de la phase 1 (lag 6 mois).

6. **Tendances Sen sur 47 ans (16/21 variables significatives, p < 0.05) :**
   - PWAT : +0.21 %/an (humidification globale, conforme Clausius-Clapeyron)
   - TCDC : +0.19 %/an (avec saut algorithmique CFSR→CFSv2)
   - APCP : +0.15 %/an (intensification cycle hydrologique)
   - SPFH2m : +0.12 %/an (humidité spécifique surface)
   - CRE_LW : −0.19 %/an, CRE_SW : −0.17 %/an (perte de l'effet refroidissant + réchauffant des nuages)
   - **CSDLF : +7.84 W/m² sur 47 ans** = signature directe du forçage radiatif des gaz à effet de serre
   - **T2m : +0.78 K sur 47 ans** ≈ +0.017 K/an, proche du consensus GIEC ~0.9 K depuis 1979
   - PRMSL, ALBDO, CSDSF, CSUSF, USWRF : pas de tendance significative

7. **Saut CFSR → CFSv2 (jan 2011) significatif sur 17/21 variables.** Top 5 magnitudes en sd : CRE_LW (−1.92), PRMSL (−1.77), CRE_SW (+1.56), TCDC (+1.14), CRE_net (+1.13). **L'homogénéisation par modèle additif `lm(y ~ t + step + month)` fait chuter le R² du modèle multivarié de 0.75 à 0.44** : un tiers de la variance explicative apparente était portée par cet artefact technique. Les CRE et flux radiatifs all-sky perdent quasi-toute leur corrélation avec le CO2 après correction ; T2m et DLWRF voient leur corrélation au CO2 augmenter (signal réel d'effet de serre démasqué).

## Limites identifiées

- Le détendrage est **linéaire** alors que la trend CO2 est cubique : il reste une courbure résiduelle en U dans le fit du modèle stepwise. Une amélioration possible serait d'utiliser un détendrage cubique ou un GAM.
- Les corrélations sur résidus restent modérées (|r| ≤ 0.5), ce qui reflète la difficulté physique : à l'échelle globale, le CO2 répond surtout aux puits/sources tropicaux (Amazonie, Indonésie) qui ne sont pas explicitement résolus dans une moyenne globale.
- La discontinuité CFSR → CFSv2 en 2011 introduit un petit saut de niveau sur TCDC (~3 %), visible mais non corrigé.

## Suite

Phase 4 envisageable : passer à la résolution **0.5° × 0.5°** pour analyse régionale (corrélations par bandes de latitude, hotspots tropicaux, anomalies par hémisphère).
