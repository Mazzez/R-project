# ============================================================
# 02_band_means.R  (résolution 0.5° × 0.5°)
# Calcule les moyennes pondérées cos(lat) des 18 variables par
# bande de latitude pour chaque mois (1979-01 → 2025-12).
#
# 5 bandes :
#   - boréale     : 60°N à 90°N
#   - tempérée N  : 30°N à 60°N
#   - tropicale   : 30°S à 30°N
#   - tempérée S  : 60°S à 30°S
#   - australe    : 90°S à 60°S
#   - global      : -90 à +90 (référence pour comparaison 2.5°)
#
# Entrée : Final Version/processed/nc_subset_05/YYYY/YYYYMM.nc  (564+ fichiers)
# Sortie : Analyse Climat 0.5°x0.5°/outputs/monthly_band_means_05.csv
# ============================================================

suppressPackageStartupMessages({
  library(ncdf4); library(dplyr); library(tidyr)
})

NC_BASE  <- "/home/mazzez/Bureau/R project/Final Version/processed/nc_subset_05"
OUT_DIR  <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 0.5°x0.5°/outputs"
OUT_FILE <- file.path(OUT_DIR, "monthly_band_means_05.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

VAR_MAP <- c(
  TMP_2maboveground                                = "T2m",
  TMP                                              = "T500",  # plevel=500 dans le 0.5°
  SPFH_2maboveground                               = "SPFH2m",
  PWAT_entireatmosphere_consideredasasinglelayer_  = "PWAT",
  APCP_surface                                     = "APCP",
  TCDC_entireatmosphere_consideredasasinglelayer_  = "TCDC",
  DLWRF_surface                                    = "DLWRF",
  ULWRF_surface                                    = "ULWRF",
  DSWRF_surface                                    = "DSWRF",
  USWRF_surface                                    = "USWRF",
  PRMSL_meansealevel                               = "PRMSL",
  CSDSF_surface                                    = "CSDSF",
  CSUSF_surface                                    = "CSUSF",
  CSDLF_surface                                    = "CSDLF",
  CSULF_surface                                    = "CSULF",
  CDUVB_surface                                    = "CDUVB",
  DUVB_surface                                     = "DUVB",
  ALBDO_surface                                    = "ALBDO"
)

# Définition des bandes de latitude (intervalles NON-chevauchants)
# Convention : tropical inclut [-30, 30] ; les autres bandes excluent leur
# frontière tropicale pour éviter le double-comptage des pixels limites.
# Total : 60 + 60 + 121 + 60 + 60 = 361 lignes de latitude (= ny).
BANDS <- list(
  austral     = function(l) l <  -60,
  temperate_S = function(l) l >= -60 & l <  -30,
  tropical    = function(l) l >= -30 & l <=  30,
  temperate_N = function(l) l >   30 & l <=  60,
  boreal      = function(l) l >   60,
  global      = function(l) rep(TRUE, length(l))
)

files <- sort(list.files(NC_BASE, pattern = "\\.nc$",
                         recursive = TRUE, full.names = TRUE))
cat("Fichiers NetCDF trouvés :", length(files), "\n")
if (length(files) == 0) stop("Aucun .nc sous ", NC_BASE)

# Préparation des poids et masques de bande
nc0 <- nc_open(files[1])
lat <- nc0$dim$latitude$vals    # 361 points, -90 à +90 par 0.5°
lon <- nc0$dim$longitude$vals   # 720 points, 0 à 359.5
nc_close(nc0)

nx <- length(lon)
ny <- length(lat)
cat("Grille :", nx, "x", ny, "\n")
cat("lat range :", min(lat), "à", max(lat), "\n\n")

w_lat  <- cos(lat * pi / 180)
w_grid <- matrix(rep(w_lat, each = nx), nrow = nx, ncol = ny)

# Masques par bande (matrice booléenne 720 × 361)
make_mask <- function(test_fn) {
  mask_lat <- test_fn(lat)
  matrix(rep(mask_lat, each = nx), nrow = nx, ncol = ny)
}
masks <- lapply(BANDS, make_mask)

# Vérification de non-chevauchement (somme des 5 bandes = ny rows pleins)
zone_masks <- masks[setdiff(names(masks), "global")]
n_membership <- Reduce(`+`, lapply(zone_masks, function(m) m[1, ]))
stopifnot(all(n_membership == 1))

cat("Cellules par bande :\n")
for (b in names(masks)) {
  cat(sprintf("  %-12s : %d cellules (%.1f %%)\n",
              b, sum(masks[[b]]),
              100 * sum(masks[[b]]) / length(masks[[b]])))
}

# Fonction : moyenne pondérée d'une variable sur un masque
band_mean <- function(arr, w, mask) {
  ok <- !is.na(arr) & mask
  sum(arr[ok] * w[ok]) / sum(w[ok])
}

# ------------------------------------------------------------
# Boucle principale
# ------------------------------------------------------------
result <- vector("list", length(files))
t0 <- Sys.time()

for (i in seq_along(files)) {
  f <- files[i]
  yyyymm <- sub("\\.nc$", "", basename(f))
  yyyy   <- as.integer(substr(yyyymm, 1, 4))
  mm     <- as.integer(substr(yyyymm, 5, 6))

  nc <- nc_open(f)
  vars_in_nc <- names(nc$var)

  rows <- list()
  for (b in names(masks)) {
    row <- list(year = yyyy, month = mm,
                date = as.Date(sprintf("%04d-%02d-01", yyyy, mm)),
                band = b)
    for (nc_name in names(VAR_MAP)) {
      short <- VAR_MAP[[nc_name]]
      if (nc_name %in% vars_in_nc) {
        arr <- ncvar_get(nc, nc_name)   # collapse_degen = TRUE par défaut : enlève les dim de taille 1
        if (length(dim(arr)) != 2 || !all(dim(arr) == c(nx, ny))) {
          # Cas inattendu — log et NA
          message("  ", yyyymm, " : ", nc_name, " dim inhabituelle ",
                  paste(dim(arr), collapse = "×"))
          row[[short]] <- NA_real_
          next
        }
        row[[short]] <- band_mean(arr, w_grid, masks[[b]])
      } else {
        row[[short]] <- NA_real_
      }
    }
    rows[[b]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }

  nc_close(nc)
  result[[i]] <- bind_rows(rows)

  if (i %% 30 == 0 || i == length(files)) {
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    eta <- elapsed * (length(files) - i) / i
    cat(sprintf("  %3d / %3d  (%.0f %%)  %s   elapsed=%.0fs  eta=%.0fs\n",
                i, length(files), 100 * i / length(files),
                yyyymm, elapsed, eta))
  }
}

dt <- Sys.time() - t0
cat("\nDurée totale :", round(as.numeric(dt, units = "secs"), 1), "s\n")

df <- bind_rows(result) |> arrange(date, band)
cat("Dimensions du tableau final :", paste(dim(df), collapse = " x "), "\n")
cat("Plage temporelle :", as.character(min(df$date)), "->",
    as.character(max(df$date)), "\n")

cat("\nNA par variable :\n")
print(colSums(is.na(df)))

cat("\nAperçu (6 premières lignes - une par bande) :\n")
print(head(df, 6))

write.csv(df, OUT_FILE, row.names = FALSE)
cat("\n=== CSV sauvegardé :", OUT_FILE, "===\n")
