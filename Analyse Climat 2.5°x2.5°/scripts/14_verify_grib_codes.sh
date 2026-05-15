#!/usr/bin/env bash
# =============================================================
# 14_verify_grib_codes.sh
# Vérifie que les 18 records sélectionnés correspondent exactement
# aux mêmes paramètres physiques GRIB2 entre CFSR (.grb2) et
# CFSv2 (.grib2).
#
# Compare 4 fichiers témoins :
#   - CFSR : 197901, 200001
#   - CFSv2 : 201101, 202412
#
# Pour chaque variable, l'identifiant complet est de la forme :
#   var<discipline>_<master>_<local>_<table>_<category>_<param>
#
# Si tous les fichiers retournent le même identifiant pour une
# variable donnée, on a la preuve qu'il s'agit bien du même
# paramètre physique (même entrée dans la table GRIB2 NCEP).
#
# Sortie :
#   outputs/grib_codes_verification.csv
# =============================================================

set -uo pipefail

OUT_DIR="/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
OUT="$OUT_DIR/grib_codes_verification.csv"

MATCH='(:(TMP:2 m above ground|TMP:500 mb|SPFH:2 m above ground|APCP:surface|PRMSL:mean sea level|CSDSF:surface|CSUSF:surface|CSDLF:surface|CSULF:surface|CDUVB:surface|DUVB:surface|ALBDO:surface):)|(:(DLWRF|ULWRF|USWRF|DSWRF):surface:[0-9]+@4 hour)|(:(PWAT|TCDC):entire atmosphere )'

declare -A TEST_FILES=(
  ["CFSR_1979_01"]="/home/mazzez/Bureau/R project/Data/2.5° x 2.5°/1979/pgbl04.gdas.197901.grb2"
  ["CFSR_2000_01"]="/home/mazzez/Bureau/R project/Data/2.5° x 2.5°/2000/pgbl04.gdas.200001.grb2"
  ["CFSv2_2011_01"]="/home/mazzez/Bureau/R project/Data/2.5° x 2.5°/2011/pgbl04.gdas.201101.grib2"
  ["CFSv2_2024_12"]="/home/mazzez/Bureau/R project/Data/2.5° x 2.5°/2024/pgbl04.gdas.202412.grib2"
)

extract_codes() {
  local f="$1"
  # wgrib2 -var -lev -varX produit : "rec:bytes:VAR:LEVEL:varX_code"
  # 5 champs séparés par ":" -> $3=VAR, $4=LEVEL, $5=CODE
  wgrib2 "$f" -match "$MATCH" -var -lev -varX 2>/dev/null \
    | awk -F: '{print $3 "@@" $4 "@@" $5}' | sort
}

# Construire le tableau commun
echo "=== Codes paramètre GRIB2 par fichier ==="
TMP=$(mktemp -d)

for label in "${!TEST_FILES[@]}"; do
  f="${TEST_FILES[$label]}"
  extract_codes "$f" > "$TMP/$label.txt"
  echo "$label : $(wc -l < "$TMP/$label.txt") records"
done

echo ""
echo "=== Comparaison des codes paramètre (varX) ==="
# Joindre par numéro de ligne (sort identique des 4 fichiers)
paste "$TMP/CFSR_1979_01.txt" "$TMP/CFSR_2000_01.txt" \
      "$TMP/CFSv2_2011_01.txt" "$TMP/CFSv2_2024_12.txt" |
awk -F'\t' 'BEGIN{
  print "var,level,CFSR_1979_01,CFSR_2000_01,CFSv2_2011_01,CFSv2_2024_12,all_match"
}{
  split($1, a, "@@"); split($2, b, "@@");
  split($3, c, "@@"); split($4, d, "@@");
  match_str = (a[3] == b[3] && b[3] == c[3] && c[3] == d[3]) ? "YES" : "NO"
  printf "%s,\"%s\",%s,%s,%s,%s,%s\n",
         a[1], a[2], a[3], b[3], c[3], d[3], match_str
}' > "$OUT"

echo "Tableau de comparaison :"
column -t -s, "$OUT"

echo ""
N_MATCH=$(awk -F, 'NR>1 && $7=="YES"' "$OUT" | wc -l)
N_TOTAL=$(awk -F, 'NR>1' "$OUT" | wc -l)
echo "=== Résultat ==="
echo "$N_MATCH / $N_TOTAL records ont des codes GRIB2 identiques sur tous les 4 fichiers."

if [[ "$N_MATCH" == "$N_TOTAL" ]]; then
  echo "✓ Les 18 variables correspondent bien à la même quantité physique"
  echo "  dans CFSR (.grb2) et CFSv2 (.grib2)."
  echo "  Le saut au passage 2010/2011 vient donc des modèles atmosphériques"
  echo "  et schémas de paramétrisation différents, PAS d'une erreur"
  echo "  de sélection de variable."
fi

rm -rf "$TMP"
echo ""
echo "Sauvegarde : $OUT"
