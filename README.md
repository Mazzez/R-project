# Projet R — Climat & CO2 (1979-2025)

Projet d'analyse statistique en R étudiant la relation entre la concentration atmosphérique mondiale de CO2 (NOAA GML) et 18 variables climatiques issues des réanalyses NCAR (CFSR + CFSv2), à deux résolutions spatiales (2.5° × 2.5° et 0.5° × 0.5°), sur la période **1979-01 → 2025-12**.

> **Questions scientifiques.**
> 1. Comment le CO2 atmosphérique a-t-il évolué depuis 1979, et quels en sont les déterminants statistiques ?
> 2. Les variations interannuelles du climat sont-elles **liées** au CO2 au-delà de la tendance commune ?
> 3. Quel est le **sens** du lien : climat → CO2 (cycle du carbone) ou CO2 → climat (effet de serre direct) ?
> 4. Comment se distribue **géographiquement** le réchauffement, et où le lien climat-CO2 est-il le plus fort ?

## Articulation du projet — 3 phases

| Phase | Dossier | Apport scientifique | Rapport |
|---|---|---|---|
| **1** | `Analyse CO2/` | Caractérisation statistique du CO2 (Sen = 1.88 ppm/an, accélération cubique, cycle saisonnier asymétrique MLO/SPO = 5.6×, modulation ENSO à 6 mois) | `Analyse CO2/rapport/CO2_final_report.html` |
| **2-4** | `Analyse Climat 2.5°x2.5°/` | Moyennes globales pondérées cos(lat), homogénéisation CFSR/CFSv2, régression climat → CO2 (R² = 0.748), causalité Granger climat→CO2 sur 15/21 variables | `Analyse Climat 2.5°x2.5°/Climat_report.html` |
| **5** | `Analyse Climat 0.5°x0.5°/` | Analyse spatiale (260 000 pixels) : amplification arctique, signature GES uniforme (CSDLF), hotspots Amazonie/Indonésie/Sibérie/Sahel, régression et Granger par zone | `Analyse Climat 0.5°x0.5°/Climat_report_05.html` |

Chaque phase a son propre **README détaillé** dans son dossier ; ce fichier est le point d'entrée du projet.

## Structure du projet

```
Final Version/
│
├── README.md                          ← ce fichier (point d'entrée du projet)
│
├── CO2/                               (3.2 MB) Données CO2 utilisées par toutes les phases
│   ├── co2_mm_gl.csv                  série mondiale NOAA GML (cible principale)
│   ├── co2_mm_mlo.csv                 série Mauna Loa
│   ├── co2_spo_surface-flask_..._txt  série Pôle Sud
│   ├── co2nat-noaa.txt                paléo Vostok (référence long-terme)
│   ├── GCB2025v15_MtCO2_flat.csv      Global Carbon Budget (émissions)
│   ├── oni.ascii.txt                  indice ENSO (Oceanic Niño Index)
│   └── 17417124.json                  métadonnées NOAA
│
├── 2.5° x 2.5° subset/                (172 MB) GRIB2 filtré 2.5° — 18 records/mois × 564 mois
├── 0.5° x 0.5° subset/                (4.1 GB) GRIB2 filtré 0.5° — 18 records/mois × 566 mois
│
├── processed/                         (4.7 GB) NetCDF compressés (zlib niveau 4)
│   ├── nc_subset_25/                  144 × 73 — utilisé par la phase 2-4
│   └── nc_subset_05/                  720 × 361 — utilisé par la phase 5
│
├── Analyse CO2/                       (33 MB) Phase 1
│   ├── scripts/                       3 scripts R (co2_analysis*.R)
│   ├── rapport/                       CO2_final_report.Rmd + HTML
│   └── README.md
│
├── Analyse Climat 2.5°x2.5°/          (36 MB) Phases 2-4
│   ├── scripts/                       15 scripts (01-13 R + 14, 15 shell)
│   ├── outputs/                       21 plots + 84 fiches per-variable + CSV/RDS
│   ├── notes/                         Phase 2.txt, Phase 3.txt, Phase 4.txt, Pipeline.txt
│   ├── Climat_report.Rmd + HTML
│   └── README.md
│
├── Analyse Climat 0.5°x0.5°/          (97 MB) Phase 5
│   ├── scripts/                       11 scripts (01-10 R + 15 shell)
│   ├── outputs/                       maps/ (36) + plots/ (8) + CSV/RDS
│   ├── notes/                         Phase 5.txt, Pipeline 0.5°x0.5°.txt
│   ├── Climat_report_05.Rmd + HTML
│   └── README.md
│
└── general view/                      Vues d'ensemble et brouillons
```

**Volume total reproductible :** ~13 GB (subsets GRIB + NetCDF + analyses + rapports). La source GRIB brute originale (3.5 GB pour 2.5° et 57 GB pour 0.5°) reste hors du projet — elle est régénérable via `15_extract_grib_subset.sh` à partir de NCAR.

## Sources de données

Le projet s'appuie sur **deux grandes familles de données scientifiques de référence**, toutes ouvertes et institutionnelles. D'une part, les **réanalyses atmosphériques globales NCAR** (CFSR pour 1979-2010 puis CFSv2 pour 2011-2025), qui combinent modèle physique et assimilation de millions d'observations mensuelles pour produire un champ tridimensionnel cohérent de l'atmosphère sur 47 ans. D'autre part, les **mesures de CO2 atmosphérique NOAA GML** (Mauna Loa, Pôle Sud, série mondiale désaisonnée), complétées par le **Global Carbon Budget** pour les émissions par pays, l'**indice ONI** pour quantifier l'influence d'ENSO, et la **carotte de Vostok** comme contexte paléo. Cette diversité de sources — gridées et stationnaires, modélisées et observationnelles, instantanées et paléo — permet d'aborder le système climat-CO2 à plusieurs échelles spatiales et temporelles avec des données indépendantes les unes des autres.

| Dataset | Origine | Période | Variables / format |
|---|---|---|---|
| **Réanalyses CFSR** | NSF-NCAR `d093002` | 1979-01 → 2010-12 | 18 records GRIB2 par mois |
| **Réanalyses CFSv2** | NSF-NCAR `d094002` | 2011-01 → 2025-12 | 18 records GRIB2 par mois |
| **CO2 mondial** | NOAA GML | 1979-01 → 2025-12 | Série mensuelle globale, désaisonnée |
| **CO2 Mauna Loa** | NOAA GML / Scripps | 1979-01 → 2025-12 | Référence hémisphère Nord |
| **CO2 Pôle Sud** | NOAA GML | 1979-01 → 2025-12 | Référence hémisphère Sud |
| **Global Carbon Budget** | GCB2025v15 | 1979 → 2024 | Émissions annuelles par pays |
| **ONI (ENSO)** | NOAA CPC | 1979-01 → 2025-12 | Anomalies SST Niño 3.4 |
| **Vostok ice core** | NOAA paléo | −420 000 → −1900 | CO2 paléo (contexte) |

## Types de fichiers manipulés

Le projet traverse **plusieurs formats de fichiers, chacun choisi pour un usage spécifique** dans le pipeline. Les données **GRIB2** (`.grb2` / `.grib2`) sont le format binaire compact natif des réanalyses NCEP/NCAR — chaque fichier mensuel contient des centaines de records (variables × niveaux) accessibles via `wgrib2`. Après filtrage aux 18 records utiles, les données sont converties en **NetCDF-4** (`.nc`) avec compression zlib : c'est le standard scientifique pour les grilles temporelles, lisible en R via le paquet `ncdf4` et environ 5× plus compact que le NetCDF-3. Les sorties d'analyse (séries mensuelles, statistiques, comparaisons) sont stockées en **CSV** simple pour inspection visuelle et portabilité, tandis que les objets R complexes (cubes 720 × 361 × 566, listes de grilles de corrélation) le sont en **RDS** binaire R-natif. Enfin, les **figures** sont en PNG (haute résolution 130-140 dpi pour impression) et les **rapports finaux** en HTML auto-générés depuis des sources **Rmd** (RMarkdown), garantissant la reproductibilité du document à partir des données et du code.

## Les 18 variables climatiques retenues

Couvrent les **4 grandes familles** physiques du climat :

| Famille | Variables | Rôle |
|---|---|---|
| **Thermo** | T2m, T500 | Réchauffement de surface et de la troposphère libre |
| **Hydrologique** | SPFH2m, PWAT, APCP | Vapeur d'eau (Clausius-Clapeyron) + précipitations |
| **Nuages** | TCDC | Couverture nuageuse (grande incertitude climatique) |
| **Radiatif all-sky** | DLWRF, ULWRF, DSWRF, USWRF | Bilan radiatif total surface |
| **Radiatif clear-sky** | CSDLF, CSULF, CSDSF, CSUSF | Bilan SANS nuages → **signature GES pure** |
| **Autres** | PRMSL, CDUVB, DUVB, ALBDO | Pression, UV-B, albédo |

Les variables ciel-clair permettent de calculer les **Cloud Radiative Effects (CRE)** qui isolent l'effet propre des nuages : `CRE = (all-sky) − (clear-sky)`.

## Reproduire le projet complet

### Pré-requis

- **R ≥ 4.0** avec les packages : `dplyr`, `tidyr`, `ggplot2`, `ncdf4`, `lmtest`, `glmnet`, `corrplot`, `Kendall`, `trend`, `patchwork`, `rmarkdown`, `knitr`, `scales`
- **wgrib2 ≥ 3.0** (pour l'extraction GRIB → NetCDF)
- **~15 GB d'espace disque** (subsets + NetCDF + outputs)

### Pipeline

```bash
PROJECT="/home/mazzez/Bureau/R project/Final Version"

# Phase 1 — Analyse CO2
cd "$PROJECT/Analyse CO2/scripts"
Rscript 00_install_packages.R          # une seule fois
Rscript co2_analysis.R                  # sections 1-8
Rscript co2_analysis_extended.R         # sections 9-11
Rscript co2_analysis_methodology.R      # sections 12-23
Rscript -e 'rmarkdown::render("../rapport/CO2_final_report.Rmd")'

# Phase 2-4 — Climat 2.5°
cd "$PROJECT/Analyse Climat 2.5°x2.5°/scripts"
bash    15_extract_grib_subset.sh       # source GRIB → subset 172 MB (1 min)
bash    01_extract_subset.sh            # subset → NetCDF (16 sec)
Rscript 02_global_means.R               # moyennes globales (10 min)
Rscript 03_validation.R                 # sanity checks
Rscript 04_merge_with_co2.R             # fusion CO2 + climat
Rscript 05_preparation.R                # 5 représentations temporelles
Rscript 06_correlations.R               # corrélations + heatmap
Rscript 07_regressions.R                # stepwise + lasso
Rscript 08_granger.R                    # tests de causalité
Rscript 09_synthese.R                   # tableau master
Rscript 10_per_variable_analysis.R      # 84 fiches per-variable
Rscript 11_trends_summary.R             # tendances Sen + bootstrap
Rscript 12_homogenization.R             # détection saut CFSR→CFSv2
Rscript 13_phase3_homog_comparison.R    # corrélations avant/après
Rscript -e 'rmarkdown::render("../Climat_report.Rmd")'

# Phase 5 — Climat 0.5°
cd "$PROJECT/Analyse Climat 0.5°x0.5°/scripts"
bash    15_extract_grib_subset.sh       # source GRIB → subset 4 GB (12 min)
bash    01_extract_subset.sh            # subset → NetCDF (5 min)
Rscript 02_band_means.R                 # moyennes par bande (30 min)
Rscript 03_validation.R                 # plots anomalies + GES
Rscript 04_trend_maps.R                 # 18 cartes Sen (20 min)
Rscript 05_correlation_maps.R           # 18 cartes corr CO2 (15 min)
Rscript 06_hotspot_analysis.R           # 4 régions × 4 variables (3 min)
Rscript 07_compare_with_25deg.R         # validation croisée
Rscript 08_regression_per_zone.R        # R² climat→CO2 par zone
Rscript 09_granger_per_zone.R           # Granger par zone
Rscript 10_hemisphere_asymmetry.R       # ratio N/S vs MLO/SPO
Rscript -e 'rmarkdown::render("../Climat_report_05.Rmd")'
```

Durée totale depuis zéro : **~2 heures** sur un Ryzen 5 / 16 GB / SSD.

## Conclusions principales du projet

### Phase 1 — Caractérisation CO2

- Hausse de **+91 ppm** depuis 1979 (Sen = 1.88 ppm/an), trajectoire **cubique** (accélération non-linéaire).
- Cycle saisonnier de **6.6 ppm** dominé par l'hémisphère Nord (asymétrie MLO/SPO = 5.6×).
- Modulation ENSO claire : corrélation taux annuel CO2 ↔ ONI = +0.28 à **lag 6 mois**.

### Phases 2-4 — Lien climat-CO2 à l'échelle globale

- **18/18 sanity checks** sur les moyennes globales (T2m ≈ 288 K, CRE_net = −19.7 W/m² ≈ IPCC).
- Sur les niveaux bruts : 11 variables corrélées au CO2 (r > 0.5), **fortement trompeur** (tendance commune).
- Sur les **résidus** : 5 variables avec |r| > 0.3 ; **R² multivarié = 0.748** avec 12 prédicteurs.
- Causalité Granger d12 lag 6 : **15/21 variables climat → CO2** (top : SPFH2m, CSDLF, PWAT, DLWRF, T500 avec p < 10⁻⁸).
- **Sens dominant : climat → CO2 à 6 mois** (carbon cycle response), cohérent avec ENSO.
- Saut CFSR/CFSv2 (jan 2011) significatif sur 17/21 variables, R² brut 0.75 → 0.44 après homogénéisation.

### Phase 5 — Distribution spatiale et hotspots

- **Amplification arctique** : Boréale chauffe à +0.052 K/an, 3.7× plus vite que les tropiques (+0.014 K/an).
- **Signature GES uniforme** : CSDLF (IR ciel-clair) monte dans les 5 bandes (+5 à +9 W/m² en 47 ans), même en Antarctique où T2m baisse.
- **Lien climat→CO2 spatialement concentré aux tropiques** : R² = 0.69 (Tropicale) vs 0.28 (Boréale) vs 0/18 variables Granger-causales en Australe.
- **Hotspot Indonésie en tête** : 4/4 variables Granger-causales pour le CO2, corr 0.15-0.17 sur résidus.
- **Asymétrie hémisphérique** : T2m N/S = 1.98×, PWAT N/S = 4.27× < CO2 MLO/SPO = 5.6× → l'asymétrie CO2 transite par le cycle hydrologique/biosphère, pas directement par la température.
- **Validation croisée 0.5° vs 2.5°** : corrélations ≥ 0.9946 sur 18 variables — pipelines équivalents.

## Articulation et points de cohérence inter-phases

```
Phase 1  ─────────────►  Asymétrie MLO/SPO = 5.6×  ──┐
                                                      ▼
Phase 5  ─────────────►  T2m N/S = 1.98×  +  PWAT N/S = 4.27×  ──► cohérence vérifiée
                         (climat moins asymétrique que CO2, transite via biosphère)

Phase 4  ─────────────►  R² globale = 0.748, climat → CO2 lag 6 mois  ──┐
                                                                          ▼
Phase 5  ─────────────►  R² globale (recalculée 18 vars) = 0.746  ──► validation
                         R² tropicale = 0.69, Granger 11/18 vars

Phase 4  ─────────────►  CSDLF +7.8 W/m² en 47 ans (forçage GES global)  ──┐
                                                                            ▼
Phase 5  ─────────────►  CSDLF positif dans 5/5 bandes, même Australe  ──► signature uniforme
```

## Pour aller plus loin

- **3 rapports HTML auto-générés** dans chaque sous-dossier (~7 MB chacun) — c'est le **point d'entrée pédagogique** pour comprendre les résultats, avec analogies, encadrés « à retenir », et plots commentés.
- Les **notes/`Phase X.txt`** documentent chaque phase en détail (méthodes, choix scientifiques, limites).
- Les CSV de sortie (`outputs/`) permettent de **rejouer toute analyse statistique** indépendamment.

## Historique du projet — deux tentatives ratées avant la version finale

Avant d'aboutir à la **`Final Version/`** présentée ici, le projet a traversé **deux tentatives successives qui ont échoué**. Documenter ces échecs n'est pas anecdotique : c'est ce qui a permis d'arriver à une architecture propre, reproductible et scientifiquement défendable.

### Tentative 1 — `R-project failure/` : l'approche brute-force par énumération

**Méthode tentée.** Au lieu d'utiliser les **noms physiques** des variables (`TMP:2 m above ground`, `DLWRF:surface`, etc.), j'ai essayé d'identifier chaque variable par son **indice numérique** dans le fichier GRIB. Pour cela, j'ai parcouru manuellement les 6 niveaux de fichiers GDAS (`pgbl01.gdas.YYYYMM.grib2` à `pgbl06.gdas.YYYYMM.grib2`) et catalogué les 666 couches du fichier `pgbl01` (`analyse_complete_pgbl01.txt`).

**Pourquoi ça a échoué.**
- La quasi-totalité des couches sortent étiquetées **`RESERVED=Reserved; (prodType 0, cat X, subcat Y)`** — `terra::rast()` n'a pas pu identifier les variables, et il a fallu deviner leur signification par leur plage de valeurs.
- Les **indices numériques ne sont pas stables** entre CFSR (1979-2010, `.grb2`) et CFSv2 (2011-2025, `.grib2`) — la 20ᵉ couche d'un fichier 1979 n'est pas la même variable que la 20ᵉ d'un fichier 2024.
- Le projet s'est noyé dans **~50 scripts brouillons** (`search_radiations.r`, `find_indices_pgbl06.r`, `analyse_robuste.r`, etc.), avec 3 sous-expériences internes successives (`1er experience/`, `2eme experience/`, `3eme experience ---finale---/`) qui n'ont jamais convergé.
- Le `SOMMAIRE_INDICES_COMPLET.txt` final liste péniblement 11 niveaux de température et quelques radiations, mais sans **garantie de cohérence physique** entre les fichiers.

**Leçon apprise.** **Ne jamais s'appuyer sur des indices numériques pour identifier des variables GRIB2.** La bonne approche est d'utiliser le **nom de paramètre + niveau** via `wgrib2 -match` ou `grib_ls -p shortName,level`.

### Tentative 2 — `R-project failure 2/` : l'approche over-engineering

**Méthode tentée.** Une fois compris qu'il fallait identifier les variables par nom, j'ai construit un `grib2_extractor.r` avec une liste propre de variables (`TMP`, `SPFH`, `RH`, `APCP`, `DSWRF`, etc.) et tenté de tout consolider dans une **base SQLite** via `RSQLite` (`climate_analysis.r`). Un module `diagnostic/` produisait des comparaisons CSV/PDF entre les 6 types de fichiers `pgbl0X`.

**Pourquoi ça a échoué.**
- Choix du paquet **`rNOMADS`** (téléchargement direct depuis NOAA) puis abandonné — fragile, non reproductible, dépendant de la disponibilité réseau.
- **SQLite était over-engineering** : pour 564 mois × 18 variables, un CSV de 1 MB suffit. La complexité technique de gérer une base relationnelle a ralenti le projet sans rien apporter.
- Le diagnostic `pgbl_types_comparison.csv` montre l'existence de **6 types de fichiers (`pgbl01` à `pgbl06`)** chacun avec des variables et niveaux différents — mais je n'ai jamais su lequel choisir pour les 18 variables cibles.
- Aucune **analyse statistique CO2 ↔ climat** n'a abouti : le projet s'est arrêté à l'étape d'extraction.

**Leçon apprise.** **Choisir les outils les plus simples qui marchent.** Un CSV bien organisé bat une base SQLite. `wgrib2 -match` bat `rNOMADS`. Et il faut **toujours valider scientifiquement le choix des fichiers** (ici : `pgbl04` est le bon car il contient les 18 variables au pas standard ; les autres niveaux sont des doublons ou des niveaux verticaux additionnels).

### Ce qui a changé dans `Final Version/`

| Aspect | Tentatives 1 & 2 | Final Version |
|---|---|---|
| Identification variables | Indices numériques / énumération | **`wgrib2 -match` par nom + niveau** (pattern explicite) |
| Choix des fichiers | `pgbl01` → `pgbl06` aléatoire | **`pgbl04` (2.5°) + `pgbh04` (0.5°)** justifié scientifiquement |
| Stockage intermédiaire | SQLite + tableaux ad-hoc | **NetCDF compressé** (standard scientifique) |
| Cohérence CFSR ↔ CFSv2 | Non vérifiée | **Vérifiée par codes GRIB2** (script 14) |
| Pipeline | Pas reproductible | **8 à 15 étapes numérotées + README** |
| Architecture | 50 scripts brouillons | **3 phases articulées** (CO2 / 2.5° / 0.5°) |
| Documentation | Sommaires improvisés | **3 rapports HTML pédagogiques + notes par phase** |
| Résultat scientifique | Aucun | **R² = 0.748, Granger climat→CO2, amplification arctique, signature GES** |

**Les deux dossiers `R-project failure/` et `R-project failure 2/`** sont conservés hors du projet final comme **archives méthodologiques** — pas pour exécution, mais pour documenter le chemin d'apprentissage.

## Auteur

**Mazzez Mohamed Amine** — projet R, 2026.
