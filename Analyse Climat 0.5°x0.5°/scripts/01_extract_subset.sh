#!/usr/bin/env bash
# =============================================================
# 01_extract_subset.sh  (résolution 0.5° × 0.5°)
# Convertit en NetCDF compressé les fichiers GRIB du subset
# (déjà filtrés aux 18 records par 15_extract_grib_subset.sh).
#
# Source : Final Version/0.5° x 0.5° subset/YYYY/pgbh04.gdas.YYYYMM.{grb2,grib2}
# Sortie : Final Version/processed/nc_subset_05/YYYY/YYYYMM.nc
#
# Note : on utilise -nc4 (NetCDF-4 + zlib compression) car la grille
# 720x361 produit des fichiers ~25 Mo en NetCDF-3 et ~6 Mo en NetCDF-4
# avec compression niveau 4.
# =============================================================

set -uo pipefail

SRC_BASE="/home/mazzez/Bureau/R project/Final Version/0.5° x 0.5° subset"
DST_BASE="/home/mazzez/Bureau/R project/Final Version/processed/nc_subset_05"

if [[ ! -d "$SRC_BASE" ]]; then
  echo "ERREUR : le dossier subset n'existe pas : $SRC_BASE"
  echo "        Lance d'abord : bash 15_extract_grib_subset.sh"
  exit 1
fi

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

for src in "$SRC_BASE"/*/pgbh04.gdas.??????.grb2 "$SRC_BASE"/*/pgbh04.gdas.??????.grib2; do
  total=$((total+1))

  fname=$(basename "$src")
  yyyymm=$(echo "$fname" | grep -oE '[0-9]{6}' | head -1)
  yyyy=${yyyymm:0:4}

  if [[ $DRY -eq 1 && "$yyyymm" != "197901" && "$yyyymm" != "200001" && "$yyyymm" != "202412" ]]; then
    continue
  fi

  dst_dir="$DST_BASE/$yyyy"
  dst="$dst_dir/${yyyymm}.nc"

  mkdir -p "$dst_dir"

  if [[ -f "$dst" && -s "$dst" ]]; then
    skipped=$((skipped+1))
    continue
  fi

  n=$(wgrib2 "$src" 2>/dev/null | wc -l)
  if [[ "$n" -ne 18 ]]; then
    warn_files+=("$fname (subset contient $n records au lieu de 18)")
    fail=$((fail+1))
    continue
  fi

  # NetCDF-4 + compression niveau 4 ; harmonisation date/ftime indispensable
  if wgrib2 "$src" \
       -set_date "${yyyymm}0100" \
       -set_ftime "0-1 day ave fcst" \
       -nc4 \
       -nc_nlev 1 \
       -netcdf "$dst" >/dev/null 2>&1; then
    ok=$((ok+1))
  else
    warn_files+=("$fname (netcdf error)")
    fail=$((fail+1))
  fi

  if (( total % 50 == 0 )); then
    echo "  ... $total fichiers traités, ok=$ok skipped=$skipped fail=$fail"
  fi
done

echo ""
echo "==== Bilan conversion GRIB -> NetCDF (0.5°) ===="
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
echo "Volumes :"
du -sh "$SRC_BASE" "$DST_BASE" 2>/dev/null
echo ""
echo "Source : $SRC_BASE"
echo "Sortie : $DST_BASE"
