#!/usr/bin/env bash

FILE=${1:-"data/paragens_preprocessado.csv"}

sed -i 's/Ã¢/â/g' "$FILE"
sed -i 's/Ãª/ê/g' "$FILE"
sed -i 's/Ãº/ú/g' "$FILE"
sed -i 's/Ã/Á/g' "$FILE"
sed -i 's/Ã­/í/g' "$FILE"
sed -i 's/Ã§/ç/g' "$FILE"
sed -i 's/Ã£/ã/g' "$FILE"
sed -i 's/Ã³/ó/g' "$FILE"
sed -i 's/Ã¡/á/g' "$FILE"
sed -i 's/Ã©/é/g' "$FILE"
sed -i 's/Â´/e /g' "$FILE"
