# ============================================================
# 02_global_means.R
# Calcule les moyennes globales pondérées cos(lat) des 18 variables
# climatiques pour chaque mois (1979-01 → 2025-12).
#
# Entrée  : Final Version/processed/nc_subset_25/YYYY/YYYYMM.nc  (564 fichiers)
# Sortie  : Analyse Climat 2.5°x2.5°/outputs/monthly_global_means_25.csv
# ============================================================

suppressPackageStartupMessages({
  library(ncdf4)
  library(dplyr)
})

NC_BASE  <- "/home/mazzez/Bureau/R project/Final Version/processed/nc_subset_25"
OUT_DIR  <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
OUT_FILE <- file.path(OUT_DIR, "monthly_global_means_25.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Mapping des noms NetCDF (issus de wgrib2 -netcdf) vers des noms courts
VAR_MAP <- c(
  TMP_2maboveground                                = "T2m",
  TMP_500mb                                        = "T500",
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

# ------------------------------------------------------------
# Préparation des poids cos(lat) à partir du premier fichier
# (la grille est constante sur tous les mois)
# ------------------------------------------------------------
files <- sort(list.files(NC_BASE, pattern = "\\.nc$",
                         recursive = TRUE, full.names = TRUE))
cat("Fichiers NetCDF trouvés :", length(files), "\n")

if (length(files) == 0) stop("Aucun .nc trouvé sous ", NC_BASE)

nc0 <- nc_open(files[1])
lat <- nc0$dim$latitude$vals    # 73 points, -90 à +90 en pas de 2.5°
lon <- nc0$dim$longitude$vals   # 144 points
nc_close(nc0)

cat("Grille :", length(lon), "x", length(lat),
    "  lat range =", min(lat), "à", max(lat), "\n")

# Vecteur de poids cos(lat) puis matrice 144 x 73
w_lat  <- cos(lat * pi / 180)
w_grid <- matrix(rep(w_lat, each = length(lon)),
                 nrow = length(lon), ncol = length(lat))
cat("Somme des poids :", round(sum(w_grid), 4),
    "  (= 144 * sum(cos(lat)))\n\n")

# ------------------------------------------------------------
# Fonction : moyenne globale pondérée d'une variable d'un .nc
# ------------------------------------------------------------
weighted_global_mean <- function(nc, var_name, w) {
  # ncvar_get retourne (lon, lat[, time]) ; on aplatit la dim time si présente
  arr <- ncvar_get(nc, var_name, collapse_degen = FALSE)
  if (length(dim(arr)) == 3) {
    # collapse time dim (taille 1 normalement après harmonisation)
    arr <- apply(arr, c(1, 2), mean, na.rm = TRUE)
  }
  if (any(is.nan(arr))) arr[is.nan(arr)] <- NA
  ok <- !is.na(arr)
  sum(arr[ok] * w[ok]) / sum(w[ok])
}

# ------------------------------------------------------------
# Boucle principale sur les 564 mois
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

  row <- list(year = yyyy, month = mm,
              date = as.Date(sprintf("%04d-%02d-01", yyyy, mm)))

  for (nc_name in names(VAR_MAP)) {
    short <- VAR_MAP[[nc_name]]
    if (nc_name %in% vars_in_nc) {
      row[[short]] <- weighted_global_mean(nc, nc_name, w_grid)
    } else {
      row[[short]] <- NA_real_
      message("  ", yyyymm, " : variable manquante ", nc_name)
    }
  }

  nc_close(nc)
  result[[i]] <- as.data.frame(row, stringsAsFactors = FALSE)

  if (i %% 60 == 0 || i == length(files)) {
    cat(sprintf("  %3d / %3d  (%.0f %%)  %s\n",
                i, length(files), 100 * i / length(files), yyyymm))
  }
}

dt <- Sys.time() - t0
cat("\nDurée totale :", round(as.numeric(dt, units = "secs"), 1), "s\n")

# ------------------------------------------------------------
# Concaténation et sauvegarde
# ------------------------------------------------------------
df <- bind_rows(result) |> arrange(date)
cat("\nDimensions du tableau final :", paste(dim(df), collapse = " x "), "\n")
cat("Plage temporelle :", as.character(min(df$date)), "->",
    as.character(max(df$date)), "\n")

cat("\nNombre de NA par variable :\n")
print(colSums(is.na(df)))

cat("\nAperçu (3 premières lignes) :\n")
print(head(df, 3))

cat("\nAperçu (3 dernières lignes) :\n")
print(tail(df, 3))

write.csv(df, OUT_FILE, row.names = FALSE)
cat("\n=== CSV sauvegardé :", OUT_FILE, "===\n")
