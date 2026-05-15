# Analyse Climat 0.5°x0.5° — Pipeline haute résolution

Analyse spatiale du climat global à la résolution 0.5° × 0.5° (grille 720 × 361 = 259 920 cellules par mois) sur la période 1979-01 → 2025-12. Cette phase est complémentaire de l'analyse 2.5° (qui couvre les moyennes globales et bandes larges) : elle apporte des analyses à valeur scientifique nouvelle :

- **Bandes de latitude** (boréale, tempérée nord, tropicale, tempérée sud, australe)
- **Cartes pixel-par-pixel** de tendance Sen + significativité Mann-Kendall pour les 18 variables
- **Cartes de corrélation** entre chaque pixel climat et le CO2_trend (sur résidus)
- **Hotspots régionaux** : Amazonie, Indonésie, Sibérie, Sahel
- **Validation croisée** avec les moyennes globales du pipeline 2.5°

## Structure

```
Analyse Climat 0.5°x0.5°/
├── scripts/                              Pipeline en 8 étapes
│   ├── 15_extract_grib_subset.sh         (étape 0) source 57 Go → subset 3 Go
│   ├── 01_extract_subset.sh              subset GRIB → NetCDF compressé
│   ├── 02_band_means.R                   moyennes par 6 bandes (5 + global)
│   ├── 03_validation.R                   sanity checks + plots
│   ├── 04_trend_maps.R                   cartes de tendance pixel par pixel
│   ├── 05_correlation_maps.R             cartes corrélation climat ↔ CO2 (résidus)
│   ├── 06_hotspot_analysis.R             4 régions clés
│   └── 07_compare_with_25deg.R           validation croisée vs 2.5°
├── outputs/
│   ├── monthly_band_means_05.csv         564 × 6 bandes × 21 colonnes
│   ├── stats_par_bande.csv
│   ├── trends_par_bande.csv
│   ├── trend_grids.rds                   18 grilles de pente Sen
│   ├── correlation_grids.rds             18 grilles de corrélation
│   ├── hotspots_series.csv               séries mensuelles 4 régions × 4 vars
│   ├── hotspots_summary.csv              tendances + corr CO2 par hotspot
│   ├── comparison_05_vs_25.csv           écarts moyennes globales 0.5°/2.5°
│   ├── maps/                             36 cartes (18 trend + 18 correlation)
│   └── plots/                            plots de validation et hotspots
└── notes/
```

## Données

| Dataset | Localisation | Fichiers | Volume |
|---|---|---|---|
| Source haute résolution NCAR | `/Data/0.5° x 0.5°/YYYY/pgbh04.gdas.YYYYMM.{grb2,grib2}` | 566 | 57 Go |
| Subset GRIB (18 records) | `/Final Version/0.5° x 0.5° subset/YYYY/...` | 566 | ~3-4 Go |
| NetCDF compressé | `/Final Version/processed/nc_subset_05/YYYY/YYYYMM.nc` | 566 | ~10-15 Go |

Le préfixe **`pgbh04`** identifie les fichiers haute résolution (vs `pgbl04` en 2.5°). Les **codes paramètre GRIB2 sont identiques** entre les deux résolutions (vérifié en phase 3 du 2.5°), ce qui garantit l'équivalence physique.

## 5 bandes de latitude

| Bande | Plage | Cellules ~ |
|---|---|---|
| Australe | 90°S - 60°S | 21 600 |
| Tempérée S | 60°S - 30°S | 43 200 |
| Tropicale | 30°S - 30°N | 86 400 |
| Tempérée N | 30°N - 60°N | 43 200 |
| Boréale | 60°N - 90°N | 21 600 |
| Global (référence) | -90 - +90 | 259 920 |

## 4 hotspots

| Région | Plage | Justification scientifique |
|---|---|---|
| Amazonie | 5°S - 5°N, 70°W - 50°W | Plus grand puits/source CO2 tropical, sensible El Niño |
| Indonésie | 10°S - 5°N, 95°E - 141°E | Forte modulation ENSO, pic émission incendies en El Niño |
| Sibérie centrale | 55°N - 70°N, 70°E - 130°E | Permafrost, amplification arctique |
| Sahel | 10°N - 20°N, 20°W - 40°E | Semi-aride sensible aux flux SST tropicaux |

## Reproduire

```bash
cd "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/scripts"

bash    15_extract_grib_subset.sh    # ~12 min  (3 Go)
bash    01_extract_subset.sh         # ~5 min   (NetCDF)
Rscript 02_band_means.R              # ~30 min  (6 bandes × 18 vars × 564 mois)
Rscript 03_validation.R              # < 1 min
Rscript 04_trend_maps.R              # ~20 min  (18 cartes)
Rscript 05_correlation_maps.R        # ~15 min  (18 cartes)
Rscript 06_hotspot_analysis.R        # ~5 min   (4 régions × 4 variables)
Rscript 07_compare_with_25deg.R      # < 1 min  (vérification croisée)
```

Total : ~90 minutes pour le pipeline complet depuis 0.

## Articulation avec les autres phases

| Phase | Localisation | Apport |
|---|---|---|
| Phase 1 | `Analyse CO2/` | Caractérisation statistique du CO2 atmosphérique (1979-2025) |
| Phase 2-4 (2.5°) | `Analyse Climat 2.5°x2.5°/` | Moyennes globales + lien climat-CO2, robustesse CFSR/CFSv2 |
| **Phase 5 (0.5°)** | **`Analyse Climat 0.5°x0.5°/`** | **Analyse spatiale et régionale** |

Le 0.5° utilise les **mêmes 18 variables** et la **même fenêtre temporelle** que le 2.5°, ce qui permet la validation croisée des moyennes globales (script 07).
