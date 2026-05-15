#!/usr/bin/env bash
# =============================================================
# 01_extract_subset.sh
# Convertit en NetCDF les fichiers GRIB du sous-ensemble
# (déjà filtrés aux 18 records utiles par 15_extract_grib_subset.sh).
#
# Source : Final Version/2.5° x 2.5° subset/YYYY/pgbl04.gdas.YYYYMM.{grb2,grib2}
# Sortie : Final Version/processed/nc_subset_25/YYYY/YYYYMM.nc
#
# Pré-requis : avoir lancé d'abord 15_extract_grib_subset.sh pour
# créer le dossier subset à partir de la source 2.5° x 2.5°/.
# =============================================================

set -uo pipefail

SRC_BASE="/home/mazzez/Bureau/R project/Final Version/2.5° x 2.5° subset"
DST_BASE="/home/mazzez/Bureau/R project/Final Version/processed/nc_subset_25"

# Vérification que le subset existe
if [[ ! -d "$SRC_BASE" ]]; then
  echo "ERREUR : le dossier subset n'existe pas : $SRC_BASE"
  echo "        Lance d'abord : bash 15_extract_grib_subset.sh"
  exit 1
fi

# Dry-run optionnel : lancer "01_extract_subset.sh test" ne traite que 3 mois
DRY=0
if [[ "${1:-}" == "test" ]]; then
  DRY=1
fi

mkdir -p "$DST_BASE"

total=0
ok=0
fail=0
skipped=0
warn_files=()

shopt -s nullglob

for src in "$SRC_BASE"/*/pgbl04.gdas.??????.grb2 "$SRC_BASE"/*/pgbl04.gdas.??????.grib2; do
  total=$((total+1))

  fname=$(basename "$src")
  yyyymm=$(echo "$fname" | grep -oE '[0-9]{6}' | head -1)
  yyyy=${yyyymm:0:4}

  # En mode test : ne traite que 3 mois témoins
  if [[ $DRY -eq 1 && "$yyyymm" != "197901" && "$yyyymm" != "200001" && "$yyyymm" != "202412" ]]; then
    continue
  fi

  dst_dir="$DST_BASE/$yyyy"
  dst="$dst_dir/${yyyymm}.nc"

  mkdir -p "$dst_dir"

  # Skip si déjà converti
  if [[ -f "$dst" && -s "$dst" ]]; then
    skipped=$((skipped+1))
    continue
  fi

  # Sanity check : vérifier que le subset contient bien 18 records
  n=$(wgrib2 "$src" 2>/dev/null | wc -l)
  if [[ "$n" -ne 18 ]]; then
    warn_files+=("$fname (subset contient $n records au lieu de 18)")
    fail=$((fail+1))
    continue
  fi

  # Conversion GRIB -> NetCDF avec harmonisation de la date / fenêtre
  # de prévision (sinon wgrib2 -netcdf rejette les records aux fenêtres
  # temporelles différentes : certaines en 0-4h, d'autres en 4-6h).
  if wgrib2 "$src" \
       -set_date "${yyyymm}0100" \
       -set_ftime "0-1 day ave fcst" \
       -netcdf "$dst" >/dev/null 2>&1; then
    ok=$((ok+1))
  else
    warn_files+=("$fname (netcdf error)")
    fail=$((fail+1))
  fi
done

echo ""
echo "==== Bilan conversion GRIB -> NetCDF ===="
echo "Total fichiers du subset : $total"
echo "Convertis OK             : $ok"
echo "Déjà présents (skip)     : $skipped"
echo "Échecs                   : $fail"
if (( fail > 0 )); then
  echo ""
  echo "Détail des échecs :"
  printf '  - %s\n' "${warn_files[@]}"
fi
echo ""
echo "Source : $SRC_BASE"
echo "Sortie : $DST_BASE"
