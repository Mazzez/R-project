#!/usr/bin/env bash
# =============================================================
# 15_extract_grib_subset.sh
# Extrait les 18 records sélectionnés de chaque fichier GRIB2
# mensuel de la source 2.5° x 2.5° et les écrit en GRIB2 dans
# une structure miroir.
#
# Source : Data/2.5° x 2.5°/YYYY/pgbl04.gdas.YYYYMM.grb2|grib2
# Sortie : Final Version/2.5° x 2.5° subset/YYYY/pgbl04.gdas.YYYYMM.grb2|grib2
#          (mêmes noms, mêmes extensions, structure d'années identique)
#
# Volume attendu :
#   - source     ~3.5 Go
#   - subset     ~6 Mo (rapport 600x)
# Durée attendue : ~1 min sur SSD
# =============================================================

set -uo pipefail

SRC_BASE="/home/mazzez/Bureau/R project/Data/2.5° x 2.5°"
DST_BASE="/home/mazzez/Bureau/R project/Final Version/2.5° x 2.5° subset"

# Pattern de sélection des 18 records (identique au script 01)
MATCH='(:(TMP:2 m above ground|TMP:500 mb|SPFH:2 m above ground|APCP:surface|PRMSL:mean sea level|CSDSF:surface|CSUSF:surface|CSDLF:surface|CSULF:surface|CDUVB:surface|DUVB:surface|ALBDO:surface):)|(:(DLWRF|ULWRF|USWRF|DSWRF):surface:[0-9]+@4 hour)|(:(PWAT|TCDC):entire atmosphere )'

mkdir -p "$DST_BASE"

total=0
ok=0
skipped=0
fail=0
warn_files=()

shopt -s nullglob

for src in "$SRC_BASE"/*/pgbl04.gdas.??????.grb2 "$SRC_BASE"/*/pgbl04.gdas.??????.grib2; do
  total=$((total+1))

  fname=$(basename "$src")
  yyyymm=$(echo "$fname" | grep -oE '[0-9]{6}' | head -1)
  yyyy=${yyyymm:0:4}
  ext="${fname##*.}"

  dst_dir="$DST_BASE/$yyyy"
  dst="$dst_dir/$fname"  # même nom et extension que la source

  mkdir -p "$dst_dir"

  # Skip si déjà extrait avec 18 records
  if [[ -f "$dst" ]]; then
    n=$(wgrib2 "$dst" 2>/dev/null | wc -l)
    if [[ "$n" -eq 18 ]]; then
      skipped=$((skipped+1))
      continue
    fi
    rm -f "$dst"
  fi

  # Extraction GRIB2 -> GRIB2 (pas de conversion NetCDF)
  if wgrib2 "$src" -match "$MATCH" -grib_out "$dst" >/dev/null 2>&1; then
    n=$(wgrib2 "$dst" 2>/dev/null | wc -l)
    if [[ "$n" -eq 18 ]]; then
      ok=$((ok+1))
    else
      warn_files+=("$fname (got $n records, expected 18)")
      fail=$((fail+1))
    fi
  else
    warn_files+=("$fname (wgrib2 error)")
    fail=$((fail+1))
  fi
done

echo ""
echo "==== Bilan extraction GRIB miroir ===="
echo "Total fichiers source  : $total"
echo "Extraits OK            : $ok"
echo "Déjà présents (skip)   : $skipped"
echo "Échecs / incomplets    : $fail"
if (( fail > 0 )); then
  echo ""
  echo "Détail des échecs :"
  printf '  - %s\n' "${warn_files[@]}"
fi

echo ""
echo "Volume :"
du -sh "$SRC_BASE" "$DST_BASE" 2>/dev/null

echo ""
echo "Sortie : $DST_BASE"
echo ""
echo "Vérification : structure miroir"
echo "  Source : $SRC_BASE/<YYYY>/pgbl04.gdas.<YYYYMM>.grb2|grib2"
echo "  Subset : $DST_BASE/<YYYY>/pgbl04.gdas.<YYYYMM>.grb2|grib2"
