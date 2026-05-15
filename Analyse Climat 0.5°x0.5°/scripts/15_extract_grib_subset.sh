#!/usr/bin/env bash
# =============================================================
# 15_extract_grib_subset.sh  (résolution 0.5° × 0.5°)
# Extrait les 18 records sélectionnés de chaque fichier GRIB2
# mensuel de la source 0.5° x 0.5° et les écrit en GRIB2 dans
# une structure miroir.
#
# Source : Data/0.5° x 0.5°/YYYY/pgbh04.gdas.YYYYMM.{grb2,grib2}
# Sortie : Final Version/0.5° x 0.5° subset/YYYY/pgbh04.gdas.YYYYMM.{grb2,grib2}
#
# Volume attendu :
#   - source     ~57 Go
#   - subset     ~3 Go (rapport ~19x)
# Durée attendue : ~10-15 min
# =============================================================

set -uo pipefail

SRC_BASE="/home/mazzez/Bureau/R project/Data/0.5° x 0.5°"
DST_BASE="/home/mazzez/Bureau/R project/Final Version/0.5° x 0.5° subset"

# Pattern identique au 2.5° (codes paramètre GRIB2 identiques entre pgbl/pgbh)
MATCH='(:(TMP:2 m above ground|TMP:500 mb|SPFH:2 m above ground|APCP:surface|PRMSL:mean sea level|CSDSF:surface|CSUSF:surface|CSDLF:surface|CSULF:surface|CDUVB:surface|DUVB:surface|ALBDO:surface):)|(:(DLWRF|ULWRF|USWRF|DSWRF):surface:[0-9]+@4 hour)|(:(PWAT|TCDC):entire atmosphere )'

# Mode test : ne traite que 3 mois témoins
DRY=0
if [[ "${1:-}" == "test" ]]; then
  DRY=1
fi

mkdir -p "$DST_BASE"

total=0
ok=0
skipped=0
fail=0
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
  dst="$dst_dir/$fname"

  mkdir -p "$dst_dir"

  if [[ -f "$dst" ]]; then
    n=$(wgrib2 "$dst" 2>/dev/null | wc -l)
    if [[ "$n" -eq 18 ]]; then
      skipped=$((skipped+1))
      continue
    fi
    rm -f "$dst"
  fi

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

  # Progression toutes les 50 itérations
  if (( total % 50 == 0 )); then
    echo "  ... $total fichiers traités, ok=$ok skipped=$skipped fail=$fail"
  fi
done

echo ""
echo "==== Bilan extraction GRIB miroir 0.5° ===="
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
echo "Volumes :"
du -sh "$SRC_BASE" "$DST_BASE" 2>/dev/null

echo ""
echo "Sortie : $DST_BASE"
