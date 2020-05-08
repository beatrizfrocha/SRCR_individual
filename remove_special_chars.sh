#!/usr/bin/env bash

FILE=${1:-"data/paragens_preprocessado.csv"}

sed -i 's/â/a/g' "$FILE"
sed -i 's/ê/e/g' "$FILE"
sed -i 's/ú/u/g' "$FILE"
sed -i 's/Á/A/g' "$FILE"
sed -i 's/í/i/g' "$FILE"
sed -i 's/ç/c/g' "$FILE"
sed -i 's/ã/a/g' "$FILE"
sed -i 's/ó/o/g' "$FILE"
sed -i 's/á/a/g' "$FILE"
sed -i 's/é/e/g' "$FILE"

